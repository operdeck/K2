# Main code for Kaggle Springleaf competition.
# Use standalone via one of the settings lists, or drive from tuneGrid to vary tuning params

# Ideas:
# X more rounds
# X adding of date variants
# X adding of 'countNA' feature
# - add Ivar's version of this
# X handling of symbolics with -1 fields
# X add profession fields (grouped) [NB: iffy, can be improved]
# X symbolic binning for 'character' columns
# - numeric binning
# - univariate selection
# X-X deselection of correlated predictors
# - deselection of linearly correlated predictors
# X using geo/zip information from the datasets

# currently not used (binning etc):
# source("funcs.R")

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
  ,"nrounds"= 100 #8000
  ,"eta"=0.0075
  ,"min_child_weight"=6
  ,"max_depth"=9
  ,"alpha"=4
  ,"lambda"=5
  ,"sb_threshold"=0.001
  ,"corr_threshold"=0.0
  ,"valpct" = 16.66667 # should give 5000 rows
  ,"corr_pct" = 50
  ,"subsample" = 0.7
  ,"colsample_bytree" = 0.5
  ,"addGeoFields" = F
  ,"addJobFields" = F
  ,"random_seed"=1948
  ,"early.stop.round" = 100 # 500
  ,"print.every.n"=10
)

settings_big <- list(
  "useSmallSample"=F
  ,"nrounds"= 8000
  ,"eta"=0.0075
  ,"min_child_weight"=6
  ,"max_depth"=9
  ,"alpha"=4
  ,"lambda"=5
  ,"sb_threshold"=0.0 # 0.001
  ,"corr_threshold"=0.0
#   ,"valpct" = 10.32837 # should give 15000 rows
  ,"valpct" = 3.44279 # should give 5000 rows
  ,"corr_pct" = 50
  ,"subsample" = 1 # 0.7 # <-- what about these...
  ,"colsample_bytree" = 1 # 0.5
  ,"addGeoFields" = F
  ,"addJobFields" = F
  ,"random_seed"=1948
  ,"early.stop.round" = 1000 # 500
  ,"print.every.n"=10
)

if (!exists("settings")) {
  settings <- settings_big
}

# params doc: https://github.com/dmlc/xgboost/blob/master/doc/parameter.md

xgbParams <- list(
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

###########################
# Data read
###########################

if (get("useSmallSample")) {
  train <- fread( "data/train_small.csv",header = T, sep = ",",
                  stringsAsFactors=F,integer64="double",data.table=F )
  test <- fread( "data/test_small.csv",header = T, sep = ",",
                 stringsAsFactors=F,integer64="double",data.table=F)
} else {
  train <- fread( "data/train-2.csv",header = T, sep = ",",
                  stringsAsFactors=F,integer64="double",data.table=F )
  test <- fread( "data/test-2.csv",header = T, sep = ",",
                 stringsAsFactors=F,integer64="double",data.table=F)
}
cat("Train set:",dim(train),fill=T)
y <- train$target
train <- select(train, -ID, -target)
testIDs <- test$ID
test <- select(test, -ID)
valCnt <- round(get("valpct") * nrow(train) / 100)
cat("Validation",get("valpct"),"%, ",valCnt,"rows",fill=T)
valSetIndices <- sample(1:nrow(train), valCnt) # also used for early stopping

###########################
# Data preparation
###########################

# Quickly replace missings by NA in symbolic fields
# (should influence NA row count below)
symNAs <- c("") # left out -1
for (colName in colnames(train)[which(sapply(train, function(col) { return (!is.numeric(col)) } ))]) {
  train[[colName]][train[[colName]] %in% symNAs] <- NA
  test[[colName]][test[[colName]] %in% symNAs] <- NA
}

# TODO: change "[]" in VAR_0044 to "false" 
# TODO: extra grep not used/optional

# Row-wise count of number of strange values
# TODO: drop grepping for 99 etc but use Ivar's function also
print("Counting NA's per row - time consuming")
countNA <- function(ds) 
{
  return (as.double(apply(ds,1,
                          function(x) 
                            sum(is.na(x) | grepl("99[6789]$",as.character(x))))))
}
train$xtraNumNAs1 <- countNA(train)
test$xtraNumNAs1 <- countNA(test)
cat("Xtra num NAs 1 min/Q1/median/mean/Q3/max:",summary(train$xtraNumNAs1),"unique:",length(unique(train$xtraNumNAs1)),fill=T)

countNACombos <- function(ds) {
  naCount <- rep(0, nrow(ds))
  for (i in 1:ncol(ds)) {
    naRows <- which(is.na(ds[[i]]))
    naCount[naRows] <- naCount[naRows] + i + runif(1) # use a random number for a unique weight of every column
    # adding 'i' would perhaps localize the clusters somewhat
  }
  return(naCount)
}
train$xtraNumNAs2 <- countNACombos(train) 
test$xtraNumNAs2 <- countNACombos(test) 
cat("Xtra num NAs 2 min/Q1/median/mean/Q3/max:",summary(train$xtraNumNAs2),"unique:",length(unique(train$xtraNumNAs2)),fill=T)

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

# TODO: switch back - this is the original code    
#     result[[paste(colName, "week", sep="_")]] <- week(asDate)
#     result[[colName]] <- as.double(difftime(epoch, asDate,units='days'))
    
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
  test <- processDateFlds(test, dateFldNames)
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
  test <- combineDates(test, paste(dateFldNames,"date",sep="_"))
}

#
# GEO
#

# TODO: add test set

if (get("addGeoFields")) {
  
  print("Adding geo info")
  
  # Geo cleansing: fix city names that have same zip/state by differ only marginally
  # Consider: similar but with city being empty (but maybe not so relevant; review 'dupes' result for this)
  cat("Unique city names before cleanup:", 
      length(unique(train$VAR_0200)), ", dim:", dim(train), fill=T)
  reviewDupes <- group_by(train, VAR_0200, VAR_0237, VAR_0241) %>% 
    dplyr::summarise(freq = n()) %>%
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
  test <- mutate(test, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
  test <- left_join(test, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
    mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
    select(-fullGeoID, -altName)
  cat("Unique city names after cleansing:", 
      length(unique(train$VAR_0200)), ", dim:", dim(train), fill=T)
  
  # Replace city by combined city-state name
  train = mutate(train, 
                 combinedCityState=paste(VAR_0200, VAR_0237, sep="_")) %>% select(-VAR_0200)
  test = mutate(test, 
                combinedCityState=paste(VAR_0200, VAR_0237, sep="_")) %>% select(-VAR_0200)
  allZipData <- rbind(select(train, combinedCityState, VAR_0241),
                      select(test, combinedCityState, VAR_0241))
  
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
  test <- left_join(test, zipcodesByCity, by="combinedCityState")
  test <- left_join(test, countByZip, by="VAR_0241")
  
  # Create fields for first 2 and 3 chars of zip code and do symbolic binning to bin rank
  # This will be a proxy to response behavior by location (full zip may be too granular, state too course)
  train$zip2 <- substr(train$VAR_0241, 1, 2)
  test$zip2 <- substr(test$VAR_0241, 1, 2)
  
  train$zip3 <- substr(train$VAR_0241, 1, 3)
  test$zip3 <- substr(test$VAR_0241, 1, 3)
  
  rm("allZipData")
}

#
# Group title/profession fields
#

if (get("addJobFields")) {
  
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
test  <- test[,!(names(test) %in% zeroVarCols)]

# Symbolic recoding

# TODO: use "sb_threshold" if not 0 to do real sym binning

for (i in 1:ncol(train)) {
  if (class(train[[i]]) == "character") {
    tmp= as.numeric(as.factor(c(train[[i]], test[[i]])))
    train[[i]]<- head(tmp, nrow(train))
    test[[i]]<- tail(tmp, nrow(test))
  }
}

###########################
# NA handling
###########################

# see also .. deletes binary columns that contain 56, 89 or 918 NaN values in the Training set. A binary 
# column with two values where one of those values is NaN simply tells you which row is NaN and which is not
# https://www.kaggle.com/raddar/springleaf-marketing-response/removing-irrelevant-vars/code

train[is.na(train)] <- -98765
test[is.na(test)] <- -98765

###########################
# Highly correlated vars
###########################

#emove unimportant variables (https://www.kaggle.com/raddar/springleaf-marketing-response/removing-irrelevant-vars)

# Watch some of Owen Zhang's talks on YouTube if you haven't already. They'll give you a few more ideas.

if (get("corr_threshold") != 0.0) {
  corSampleN <- nrow(train)*get("corr_pct")/100.0
  trainCor <- cor( sample_n(train, corSampleN, method='spearman'))
  trainCor[is.na(trainCor)] <- 0
  correlatedVars <- colnames(train)[findCorrelation(trainCor, cutoff = get("corr_threshold"), verbose = F)]
  cat("Removed highly correlated cols:", length(correlatedVars), 
      "(of", length(names(train)), ")", fill=T)
  train <- train[,!(names(train) %in% correlatedVars)]
  test  <- test[,!(names(test) %in% correlatedVars)]
}
cat("Dimensions of final train set:",dim(train), fill=T)
cat("Dimensions of final test set:",dim(test), fill=T)

###########################
# Model building
###########################

xgtrain = xgb.DMatrix(as.matrix(train[-valSetIndices,]), label = y[-valSetIndices], missing = NA)
xgval = xgb.DMatrix(as.matrix(train[valSetIndices,]), label = y[valSetIndices], missing = NA)
gc()

# history <- xgb.cv(  nrounds = get("nrounds")
#                     , params = xgbParams
#                     , data = xgtrain
#                     , early.stop.round = 100
#                     , nfold = 5
#                     , print.every.n = get("print.every.n") )
#                     
# print(history)

watchlist <- list('val' = xgval, 'dev' = xgtrain)
model = xgb.train(
  nrounds = get("nrounds")
  , params = xgbParams
  , data = xgtrain
  , early.stop.round = get("early.stop.round")
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

###########################
# Score test set and write out
###########################

doScoring <- !get("useSmallSample")
if (doScoring) {
  
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
  if (!file.exists("submissions")){
    dir.create(file.path(".", "submissions"))
  }
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