# "This is the script that produces LB 0.7985 for me with more features engineering"
# Version 1 from https://www.kaggle.com/mrooijer/springleaf-marketing-response/xgboost-run-local/code

# TODO, bring in features of predict.R:
# X practicalities: use 'fread', rewrite wrinting of results
# X more rounds
# X adding of date variants
# X adding of 'countNA' feature
# X handling of symbolics with -1 fields
# X add profession fields (grouped) [NB: iffy, can be improved]
# X symbolic binning for 'character' columns
# - numeric binning
# - univariate selection
# x deselection of correlated predictors
# - deselection of linearly correlated predictors
# X using geo/zip information from the datasets

source("funcs.R")
# BTW see also metrics in https://github.com/benhamner/Metrics
# for this or future competitions

library(xgboost)
library(readr)
library(data.table)
library(bit64)
library(lubridate)
library(pROC)
library(caret)
library(dplyr)
library(stringdist)

###########################
# Settings
###########################

get <- function(settingsName) {
  if (is.null(settings[[settingsName]])) {
    stop(paste("Missing setting:", settingsName))
  }
  return (settings[[settingsName]])
  
}

settings_small <- list(
  "useSmallSample"=TRUE
  ,"doScoring"=T
  ,"nrounds"=200
  ,"print.every.n"=10
  ,"eta"=0.01
  ,"min_child_weight"=6
  ,"max_depth"=6
  ,"alpha"=4
  ,"lambda"=5
  ,"random_seed"=1948
  ,"sb_threshold"=0.001
  , "valpct" = 20
  ,"subsample" = 0.7
  ,"colsample_bytree" = 0.5
)

settings_big <- list(
  "useSmallSample"=FALSE
  ,"doScoring"=T
  ,"nrounds"=200
  ,"print.every.n"=10
  ,"eta"=0.0075
  ,"min_child_weight"=6
  ,"max_depth"=9
  ,"alpha"=4
  ,"lambda"=5
  ,"random_seed"=1948
  ,"sb_threshold"=0.001
  , "valpct" = 20
  ,"subsample" = 0.7
  ,"colsample_bytree" = 0.5
)

if (!exists("settings")) {
  settings <- settings_small
}

# params doc: https://github.com/dmlc/xgboost/blob/master/doc/parameter.md

param0 <- list(
  # general , non specific params - just guessing
  "objective"  = "binary:logistic"
  , "eval_metric" = "auc"
  , "eta" = get("eta")
  , "subsample" = get("subsample")
  , "colsample_bytree" = get("colsample_bytree")
  , "min_child_weight" = get("min_child_weight")
  , "max_depth" = get("max_depth")
  , "alpha" = get("alpha")
  , "lambda" = get("lambda")
  , "nthreads" = 3
)

set.seed(get("random_seed"))
epoch <- now()
valPercentage <- 0.20 # percentage used for early stopping / validation

###########################
# Data read
###########################

if (get("useSmallSample")) {
  train <- fread( "data/train_small.csv",header = T, sep = ",",
                  stringsAsFactors=F,integer64="double",data.table=F )
  if (get("doScoring")) {
    test <- fread( "data/test_small.csv",header = T, sep = ",",
                   stringsAsFactors=F,integer64="double",data.table=F)
  }
} else {
  train <- fread( "data/train-2.csv",header = T, sep = ",",
                  stringsAsFactors=F,integer64="double",data.table=F )
  if (get("doScoring")) {
    test <- fread( "data/test-2.csv",header = T, sep = ",",
                   stringsAsFactors=F,integer64="double",data.table=F)
  }
}
cat("Train set:",dim(train),fill=T)
y <- train$target
train <- select(train, -ID, -target)
if (get("doScoring")) {
  testIDs <- test$ID
  test <- select(test, -ID)
}
valSetIndices <- sample(1:nrow(train), get("valpct") * nrow(train) / 100) # also used for early stopping

###########################
# Data preparation
###########################

# Quickly replace missings by NA in symbolic fields
# (should influence NA row count below)
symNAs <- c("") # left out -1
for (colName in colnames(train)[which(sapply(train, function(col) { return (!is.numeric(col)) } ))]) {
  #   print(createSymbin(train[[colName]],train$target)) # [-valSetIndices] !!!
  train[[colName]][train[[colName]] %in% symNAs] <- NA
  if (get("doScoring")) {
    test[[colName]][test[[colName]] %in% symNAs] <- NA
  }
  #   print(createSymbin(train[[colName]],train$target)) # [-valSetIndices] !!!
}

# change "[]" in VAR_0044 to "false" 

# Row-wise count of number of strange values
print("Counting NA's per row - time consuming")
countNA <- function(ds) 
{
  return (as.double(apply(ds,1,
                          function(x) 
                            sum(is.na(x) | grepl("99[6789]$",as.character(x))))))
}
train$numMissing <- countNA(train)
if (get("doScoring")) {
  test$numMissing <- countNA(test)
}

# Date field detection

isDate <- function(vec) { 
  all( grepl( "^\\d{2}[A-Z]{3}\\d{2}", vec[nzchar(vec)]) ) # check date fmt "12OCT13" (or empty)
}

dateFldNames <- colnames(train)[sapply(colnames(train), function(colName) 
{ class(train[[colName]]) == "character" & isDate(na.omit(train[[colName]])) } )]
cat("Date fields: ", dateFldNames, " (", length(dateFldNames), ")", fill=T)

# Standard conversion of dates:
# - derived fields day of week/month/year
# - time till today
# - absolute time
processDateFlds <- function(ds, colNames) {
  result <- ds
  for (colName in colNames) {
    asDate <- strptime(ds[[colName]], format="%d%b%y:%H:%M:%S")
    result[[paste(colName, "wday", sep="_")]] <- wday(asDate)
    result[[paste(colName, "mday", sep="_")]] <- mday(asDate)
    result[[paste(colName, "yday", sep="_")]] <- yday(asDate)
    result[[paste(colName, "hour", sep="_")]] <- hour(asDate)
    result[[paste(colName, "minute", sep="_")]] <- minute(asDate)
    result[[paste(colName, "second", sep="_")]] <- second(asDate)
    result[[paste(colName, "_today", sep="_")]] <- as.double(difftime(epoch, asDate,units='days'))
    result[[colName]] <- as.double(asDate)
    # keep absolute date in the current field, but append '_date' to it
    names(result)[ which(names(result) == colName) ] <- paste(colName,"date",sep="_")
  }
  return(result)
}

if (length(dateFldNames) > 0) {
  train <- processDateFlds(train, dateFldNames)
  if (get("doScoring")) {
    test <- processDateFlds(test, dateFldNames)
  }
}

# Create combinations of all possible date pairs.
combineDates <- function(ds, fldNames) {
  if (length(fldNames) >= 2) {
    first <- fldNames[1]
    rest <- fldNames[2:length(fldNames)]
    
    # add new column side by side with old one (new name)
    for (second in rest) {
      combinedName <- paste(first, second, sep="_")
      #cat("Combine dates: ", combinedName, fill=T)
      ds[[combinedName]] <- ds[[second]] - ds[[first]]
    }
    return(combineDates(ds, rest))
  } else {
    return(ds)
  }
}

if (length(dateFldNames) > 0) {
  train <- combineDates(train, paste(dateFldNames,"date",sep="_"))
  if (get("doScoring")) {
    test <- combineDates(test, paste(dateFldNames,"date",sep="_"))
  }
}

#
# GEO
#

print("Adding geo info")

# Geo cleansing: fix city names that have same zip/state by differ only marginally
# Consider: similar but with city being empty (but maybe not so relevant; review 'dupes' result for this)
cat("Unique city names before cleanup:", length(unique(train$VAR_0200)), ", dim:", dim(train), fill=T)
reviewDupes <- select(train, VAR_0200, VAR_0237, VAR_0241) %>% 
  summarise(freq = n()) %>%
  mutate(stateZip = paste(VAR_0241, VAR_0237, sep="_"),
         fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_")) %>%
  distinct() %>% 
  ungroup() %>%
  arrange(stateZip, desc(freq))
potentialDupes <- group_by(reviewDupes, stateZip) %>% 
  dplyr::summarise(n = n(), altName = first(VAR_0200), altID = first(fullGeoID)) %>% 
  filter(n > 1)
dupes <- mutate(left_join(potentialDupes, reviewDupes, by="stateZip"), 
                dist=stringdist(altName, VAR_0200)) %>% 
  filter(dist >= 1 & dist <= 2)
print(dupes)

train <- mutate(train, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
train <- left_join(train, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
  mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
  select(-fullGeoID, -altName)
if (get("doScoring")) {
  test <- mutate(test, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
  test <- left_join(test, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
    mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
    select(-fullGeoID, -altName)
}
cat("Unique city names after cleansing:", length(unique(train$VAR_0200)), ", dim:", dim(train), fill=T)


# Replace city by combined city-state name
train = mutate(train, combinedCityState=paste(VAR_0200, VAR_0237, sep="_")) %>% select(-VAR_0200)
if (get("doScoring")) {
  test = mutate(test, combinedCityState=paste(VAR_0200, VAR_0237, sep="_")) %>% select(-VAR_0200)
  allZipData <- rbind(select(train, combinedCityState, VAR_0241),
                      select(test, combinedCityState, VAR_0241))
} else {
  allZipData <- select(train, combinedCityState, VAR_0241)
}
# Count number of zip codes per city as a proxy for the city size:
zipcodesByCity <- group_by(unique(allZipData), combinedCityState) %>% 
  dplyr::summarise(proxyCitySize = n()) %>% 
  arrange(desc(proxyCitySize))
# Count of same zips as a proxy for population density
countByZip <- group_by(allZipData, VAR_0241) %>%
  dplyr::summarise(proxyPopulation = n()) %>%
  arrange(desc(proxyPopulation))

train <- left_join(train, zipcodesByCity, by="combinedCityState")
train <- left_join(train, countByZip, by="VAR_0241")
if (get("doScoring")) {
  test <- left_join(test, zipcodesByCity, by="combinedCityState")
  test <- left_join(test, countByZip, by="VAR_0241")
}

# Create fields for first 2 and 3 chars of zip code and do symbolic binning to bin rank
# This will be a proxy to response behavior by location (full zip may be too granular, state too course)
train$zip2 <- substr(train$VAR_0241, 1, 2)
if (get("doScoring")) {
  test$zip2 <- substr(test$VAR_0241, 1, 2)
}

train$zip3 <- substr(train$VAR_0241, 1, 3)
if (get("doScoring")) {
  test$zip3 <- substr(test$VAR_0241, 1, 3)
}

rm("allZipData")

#
# Group title/profession fields
#

processTitle <- function(ds) {
  data <- ifelse(ds$VAR_0404 == "-1", ds$VAR_0493, 
                 ifelse(ds$VAR_0493 == "-1", ds$VAR_0404, 
                        paste(ds$VAR_0404, ds$VAR_0493)))
  
  
  result <- ds
  
  result[["title_isExecutive"]] <- match( data, 
                                          c("DIRECTOR", "PRESIDENT", "CEO", "MANAGER", "CHIEF EXECUTIVE OFFICER",
                                            "BOARD MEMBER", "CFO", "CHIEF FINANCIAL OFFICER", "MANAGING MEMBER",
                                            "VP", "CHAIRMAN", "MANAG") )
  result[["title_Entrepeneur"]] <- match( data, c("INDIVIDUAL - SOLE OWNER", "OWNER", "FOUNDER") )
  result[["title_Medical"]] <- match( data, c("MEDICAL ASSISTANT", "PHARMACY TECHNICIAN", "NURSE", "NURSING", 
                                              "THERAPIST", "MEDICATION", "DENTAL",
                                              "HYGIENIST", "BARBER", "MANICURIST", "PHARMACIST", "COSMETOLOGIST") )
  result[["title_Financial"]] <- match( data, c("TREASURER","REGISTRANT","INSURANCE","TAX","LEGAL","ACCOUNTANT"))
  result[["title_Asistant"]] <- match( data, c("SECRETARY","ASSISTANT"))
  result[["title_Officer"]] <- match( data, c("OFFICER"))
  result[["title_Legal"]] <- match( data, c("ATTORNEY", "LAW", "LEGAL"))
  result[["title_Social"]] <- match( data, c("SOCIAL","COUNSELOR"))
  result[["title_Tech"]] <- match( data, c("TECH","ELECTRICIANS"))
  result[["title_RealEstate"]] <- match( data, c("REAL ESTATE","MORTGAGE"))
  result[["title_Missing"]] <- match( data, c("-1","OTHER","TITLE NOT SPECIFIED")) | is.na(data) | data == "" | data == " "
  
  return (result)
}

train <- processTitle(train)
if (get("doScoring")) {
  test <- processTitle(test)
}

######

# Regularize dummy variables (https://www.kaggle.com/c/springleaf-marketing-response/forums/t/16414/springleleaf-regularize-your-dummy-variables) 

# duplicates: https://www.kaggle.com/c/springleaf-marketing-response/forums/t/16082/var-0434-var-0449

###########################
# Data analysis
###########################

print("Check (near) zero variance")

# time consuming:
# nfs <- nearZeroVar(train, saveMetrics = FALSE)

# TODO switch back to nearZeroVar()
zeroVarCols <- colnames(train)[sapply(colnames(train), 
                                      function(colName) { 
                                        return (length(unique(train[[colName]])) < 2)
                                      })]
cat("Removed zero variance cols:", length(zeroVarCols), fill=T)
train <- train[,!(names(train) %in% zeroVarCols)]
if (get("doScoring")) {
  test  <- test[,!(names(test) %in% zeroVarCols)]
}

# Symbolic recoding

# back to simple recode factor levels; use test set??
for (i in 1:ncol(train)) {
  if (class(train[[i]]) == "character") {
    #sb <- createSymbin(train[[i]] [-valSetIndices], y[-valSetIndices], get("sb_threshold"))
    #train[[i]] <- applySymbinRank(sb, train[[i]])
    setnames(train, i, paste(names(train)[i], "sym", sep="_"))
    if (get("doScoring")) {
      u <- unique(c(train[[i]], test[[i]]))
    } else {
      u <- unique(c(train[[i]]))
    }
    train[[i]] <- as.integer(factor(train[[i]], levels=u)) 
    if (get("doScoring")) {
      #test[[i]] <- applySymbinRank(sb, test[[i]])
      setnames(test, i, paste(names(test)[i], "sym", sep="_"))
      test[[i]] <- as.integer(factor(test[[i]], levels=u)) 
    }
  }
}

###########################
# NA handling
###########################

# see also .. deletes binary columns that contain 56, 89 or 918 NaN values in the Training set. A binary 
# column with two values where one of those values is NaN simply tells you which row is NaN and which is not
# https://www.kaggle.com/raddar/springleaf-marketing-response/removing-irrelevant-vars/code

train[is.na(train)] <- -98765
if (get("doScoring")) {
  test[is.na(test)] <- -98765
}

###########################
# Highly correlated vars
###########################

#emove unimportant variables (https://www.kaggle.com/raddar/springleaf-marketing-response/removing-irrelevant-vars)

# Watch some of Owen Zhang's talks on YouTube if you haven't already. They'll give you a few more ideas.

trainCor <- cor( sample_n(train, min(nrow(train),50000)), method='spearman') # TODO: effect of this number?
trainCor[is.na(trainCor)] <- 0
correlatedVars <- colnames(train)[findCorrelation(trainCor, cutoff = .99, verbose = F)]
cat("Removed highly correlated cols:", length(correlatedVars), 
    "(of", length(names(train)), ")", fill=T)
train <- train[,!(names(train) %in% correlatedVars)]
if (get("doScoring")) {
  test  <- test[,!(names(test) %in% correlatedVars)]
}

cat("Dim train:",dim(train), fill=T)

###########################
# Model building
###########################

xgtrain = xgb.DMatrix(as.matrix(train[-valSetIndices,]), label = y[-valSetIndices], missing = NA)
xgval = xgb.DMatrix(as.matrix(train[valSetIndices,]), label = y[valSetIndices], missing = NA)
gc()

# history <- xgb.cv(  nrounds = get("nrounds")
#                     , params = param0
#                     , data = xgtrain
#                     , early.stop.round = 100
#                     , nfold = 5
#                     , print.every.n = get("print.every.n") )
#                     
# print(history)

watchlist <- list('val' = xgval, 'dev' = xgtrain)
model = xgb.train(
  nrounds = get("nrounds")
  , params = param0
  , data = xgtrain
  , early.stop.round = 100
  , watchlist = watchlist
  , print.every.n = get("print.every.n")
)

bst <- model$bestInd

cat("\nBest XGB iteration:", bst, fill=T)
cat("Best XGB score:", model$bestScore,fill=T)
cat("Number of vars: ", length(colnames(train)), fill=T)

valPerf <- as.double(auc(y[valSetIndices], predict(model, xgval, ntreelimit=bst)))
cat("AUC val:", valPerf, fill=T)
devPerf <- as.double(auc(y[-valSetIndices], predict(model, xgtrain, ntreelimit=bst)))
cat("AUC dev:", devPerf, fill=T)

# feature importance
importance_mx <- xgb.importance(names(train), model=model)
print( xgb.plot.importance(head(importance_mx,50) )) 
if (F) {
  # dump for use in e.g. small dataset creator
  write.table(importance_mx, "./importance_mx.csv", row.names=FALSE, sep=";", dec=",")
  print( paste( head(importance_mx[['Feature']], 100), collapse='","') )
}

###########################
# Score test set and write out
###########################

if (get("doScoring")) {
  
  cat("Scoring test set.",fill=T)
  #rm("train")
  rm("xgval") 
  rm("xgtrain")
  gc()
  
  #   xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
  #   preds_out <- predict(model, xgtest, ntreelimit = bst)
  
  # Process in batches to prevent OOM on windows
  preds_out <- rep(0.0, nrow(test))
  batchSize <- 10000
  for (rows in split(1:nrow(test), ceiling((1:nrow(test))/batchSize))) {
    xgtest <- xgb.DMatrix(as.matrix(test[rows,]), missing = NA)
    preds_out[rows] <- predict(model, xgtest, ntreelimit = bst)
  }
  
  #   all(preds_out == preds_out2)
  
  subm <- data.frame(testIDs, preds_out)
  colnames(subm) <- c('ID','target')
  fname <- paste("submissions/", gsub("\\.","_",
                                      paste("subm",
                                            format(model$bestScore,digits=6, nsmall=6),
                                            format(epoch, format="%Y%m%d_%H%M%S"), sep="_")),
                 ".csv",sep="")
  write.csv(subm, fname, row.names=FALSE)
  cat("Written submission score",model$bestScore,"to",fname,fill=T)
} else {
  print("Not scoring test set")
}

duration <- as.double(difftime(now(),epoch,units='mins'))
cat('Duration:', duration, 'minutes', fill=T )

results <- list("when"=as.character(epoch),
                "bestScore"=model$bestScore,
                "bestRound"=model$bestInd,
                "valPerf"=valPerf,
                "devPerf"=devPerf,
                "duration"=duration,
                "settings"=settings)

rm("settings")