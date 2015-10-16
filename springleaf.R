# Main code for Kaggle Springleaf competition.
# Use standalone via one of the settings lists, or drive from tuneGrid to vary tuning params

# Ideas:
# X more rounds
# X adding of date variants
# X adding of 'countNA' feature
# X add Ivar's version of this
# X handling of symbolics with -1 fields
# X add profession fields (grouped) [NB: iffy, can be improved]
# X symbolic binning for 'character' columns
# - numeric binning
# - univariate selection
# X-X deselection of correlated predictors
# - deselection of linearly correlated predictors
# X using geo/zip information from the datasets
# X report on univariate performance of all extra predictors
# X count series of booleans

# currently not used (binning etc):
# source("funcs.R")

# BTW see also metrics in https://github.com/benhamner/Metrics
# for this or future competitions

library(plyr) # for Ivar's use of mapvalues
library(xgboost)
library(readr)
library(data.table)
library(bit64)
library(lubridate)
library(pROC)
library(caret)
library(dplyr)
library(stringdist)
library(ggplot2)
library(Ckmeans.1d.dp)
library(tm)
library(RCurl)
library(qdap)

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
  ,"nrounds"= 200 #8000
  ,"eta"=0.0075
  ,"min_child_weight"=6
  ,"max_depth"=9
  ,"alpha"=4
  ,"lambda"=5
  ,"subsample" = 0.7
  ,"colsample_bytree" = 0.5
  ,"sb_threshold"=0 # 0.001 # Not currently used
  ,"corr_threshold"=0.0
  ,"corr_pct" = 50 # Percentage used to check against corr threshold. If that's 0, not used.
  ,"cv.nfold" = 0 # 5 # n-fold CV, set to 0 to use validation set 
  ,"valpct" = 20 # 16.66667 should give 5000 rows - not used if cv.nfold > 0, if 0 no validation
  ,"early.stop.round" = 0  # 100 # 100 # 500
  ,"addGeoFields" = T
  ,"addJobFields" = T
  ,"random_seed"=1948
  ,"print.every.n"=10
)

settings_big <- list(
  "useSmallSample"=F
  ,"nrounds"= 100 # 10000
  ,"eta"=0.0065
  ,"min_child_weight"=6
  ,"max_depth"=10
  ,"alpha"=4
  ,"lambda"=5
  ,"subsample" = 0.8 # 0.7 # <-- what about these...
  ,"colsample_bytree" = 0.8 # 0.5
  ,"sb_threshold"=0 # 0.001
  ,"corr_threshold"=0
  ,"corr_pct" = 50 # Percentage used to check against corr threshold. If that's 0, not used.
  ,"cv.nfold" = 0 # 5 # n-fold CV, set to 0 to use validation set 
  ,"valpct" = 20 # 16.66667 should give 5000 rows - not used if cv.nfold > 0, if 0 no validation
  ,"early.stop.round" = 1000
  ,"addGeoFields" = T
  ,"addJobFields" = T
  ,"random_seed"=1948
  ,"print.every.n"=100
)

if (!exists("settings")) {
#   settings <- settings_small
  settings <- settings_big
}

# params doc: https://github.com/dmlc/xgboost/blob/master/doc/parameter.md

#
# This is for some bootstrapping. 
# Run a full session with "reg:linear", then save the scores (if satisfied) in "prevscores".
# Then change back the settings to "reg:logistic" and make sure to turn ON inclusion of the
# prev scores
#

# NOTE: make sure to do both consistently for same size runs!!

considerPreviousScores <- F
includePreviousScores <- T

if (considerPreviousScores) {
  if (includePreviousScores) {
    xgbObjective <- "binary:logistic" # this has been delivering the best results so far
  } else {
    xgbObjective <- "reg:linear"
  }
} else {
  xgbObjective <- "binary:logistic"
}

xgbParams <- list(
  "objective"  = xgbObjective
  , "eval_metric" = "auc"
  , "eta" = get("eta")
  , "subsample" = get("subsample")
  , "colsample_bytree" = get("colsample_bytree")
  , "min_child_weight" = get("min_child_weight")
  , "max_depth" = get("max_depth")
  , "alpha" = get("alpha")
  , "lambda" = get("lambda")
#   , "nthreads" = 3
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
trainOriginalNames <- names(train) # will be used later, when reporting on the new fields
testIDs <- test$ID
test <- select(test, -ID)

if (considerPreviousScores) {
  if (includePreviousScores) {
    if (file.exists("prevscores")) {
      # Include previous scores - really only works for full data set
      prev1_train <- fread("prevscores/train_xgb_linear.csv", drop=c("ID"))
      colnames(prev1_train)[ncol(prev1_train)] <- "prev1"
      prev1_test <- fread("prevscores/test_xgb_linear.csv", drop=c("ID"))
      colnames(prev1_test)[ncol(prev1_test)] <- "prev1"
        
      if (nrow(prev1_train) == nrow(train) & nrow(prev1_test) == nrow(test)) {
        train <- cbind(train, prev1_train)
        test <- cbind(test, prev1_test)
      } else {
        print("!!!!! previous scores not same length as current datasets - skipping")
        print(dim(train))
        print(dim(prev1_train))
        print(dim(test))
        print(dim(prev1_test))
      }
    } else {
      print("!!!!! previous scores not available - skipping")
    }
  }
}

if (get("cv.nfold" )== 0) {
  validationSetSize <- round(get("valpct") * nrow(train) / 100)
  cat("Validation: ",get("valpct"),"%, ",validationSetSize,"rows",fill=T)
  if (validationSetSize == 0) {
    valSetIndices <- NULL
  } else {
    valSetIndices <- sample(1:nrow(train), validationSetSize) # also used for early stopping
  }
} else {
  validationSetSize <- 0
  cat("Validation: ",get("cv.nfold"),"-fold CV",fill=T)
}

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

# Create a new column from NA's - from Ivar
naCountTrain <- rep(0, nrow(train))
naCountTest  <- rep(0, nrow(test))
names <- names(train)
for (i in 1:length(names)) {
  col <- names[i]
  rnd <- runif(1)
  naCountTrain[is.na(train[[col]])] <- naCountTrain[is.na(train[[col]])] + i + rnd # use a random number for more differentation
  naCountTest[is.na(test[[col]])] <- naCountTest[is.na(test[[col]])] + i + rnd
}
train <- cbind(train, naCountTrain)
colnames(train)[ncol(train)] <- "xtraNumNAs2"
test <- cbind(test, naCountTest)
colnames(test)[ncol(test)] <- "xtraNumNAs2"
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
    result[[paste(colName, "yday", sep="_")]] <- yday(asDate)
    result[[paste(colName, "hour", sep="_")]] <- hour(asDate)
    result[[paste(colName, "minute", sep="_")]] <- minute(asDate)
    result[[paste(colName, "second", sep="_")]] <- second(asDate)
    result[[paste(colName, "till_today", sep="_")]] <- as.double(difftime(epoch, asDate,units='days'))
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

if (get("addGeoFields")) {
  
  print("Adding geo info")
  
  # Geo cleansing: fix city names that have same zip/state by differ only marginally
  # Consider: similar but with city being empty (but maybe not so relevant; review 'dupes' result for this)
  cat("Unique city names before cleanup:", 
      length(unique(train$VAR_0200)), ", dim:", dim(train), fill=T)
  bothSets <- rbind(select(train, VAR_0200, VAR_0237, VAR_0241),
                    select(test, VAR_0200, VAR_0237, VAR_0241))
  reviewDupes <- group_by(bothSets, VAR_0200, VAR_0237, VAR_0241) %>% 
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

  rm("allZipData")
  
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

  train$zip4 <- substr(train$VAR_0241, 1, 4)
  test$zip4 <- substr(test$VAR_0241, 1, 4)
  
  train$sameState <- (train$VAR_0237 == train$VAR_0274)
  test$sameState <- (test$VAR_0237 == test$VAR_0274)
  
  # VAR_0274 is also a state. Add this combination next to the VAR_0273 - VAR_0200 we already have.
  train$combinedCityState2 <- paste(train$VAR_0200, train$VAR_0274, sep="_")
  test$combinedCityState2 <- paste(test$VAR_0200, test$VAR_0274, sep="_")

  # First 5 of integer64 variable. See forum:
  # 'first 5 digits of VAR_0212 look like post code' - guess: Zip-Code + Zip4 + HouseNumber
  # 'Zip Code + Property information. Some of these properties, it seems, are listed with real estate agencies and the last digits correspond to MLS ID'
  train$VAR_0212_5 <- substr(train$VAR_0212, 1, 5)
  test$VAR_0212_5 <- substr(test$VAR_0212, 1, 5)
}

#
# Group title/profession fields
#

if (get("addJobFields")) {
  getTextCluster <- function ( values )
  {
    uniques <- unique(values)
    uniques[is.na(uniques)] <- ""
    string.distances <- stringdistmatrix(uniques,uniques,method = "jw")
    is.na(string.distances) <- do.call(cbind,lapply(string.distances, is.infinite))
    rownames(string.distances) <- uniques
    hc <- hclust(as.dist(string.distances))
    dfClust <- data.frame(uniques, cutree(hc, k=100))
    names(dfClust) <- c('modelname','cluster')
    #   plot(table(dfClust$cluster))
    t <- table(dfClust$cluster)
    t <- cbind(t,t/length(dfClust$cluster))
    t <- t[order(t[,2], decreasing=TRUE),]
    p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
    dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
    dfClust <- dfClust[rev(order(dfClust$binCount)),]
    names(dfClust) <-  c('cluster','modelname')
    mapping <- dfClust[c('cluster','modelname')]
    mapping$modelname <- as.character(mapping$modelname)
    return(mapping)
  }
  
  map493 <- getTextCluster(c(unique(train$VAR_0493),unique(test$VAR_0493)))
  names(map493) <- c("VAR_0493_cluster", "VAR_0493")
  print(summary(map493))
  train <- left_join(train, map493)
  test <- left_join(test, map493)
  
  map404 <- getTextCluster(c(unique(train$VAR_0404),unique(test$VAR_0404)))
  names(map404) <- c("VAR_0404_cluster", "VAR_0404")
  print(summary(map404))
  train <- left_join(train, map404)
  test <- left_join(test, map404)
}

#
# Counting booleans
#
pow <- 1
sumup <- function(ds, vars) {
  s <- rep(0, nrow(ds))
  for (var in vars) {
    s <- s + ds[[var]]^pow
    pow <- 2*pow
  }
  return(s)
}
train$concatBool_1 <- sumup(train, c("VAR_0180","VAR_0181","VAR_0182"))
test$concatBool_1  <- sumup(test,  c("VAR_0180","VAR_0181","VAR_0182"))
train$concatBool_2 <- sumup(train, c("VAR_0383","VAR_0384"))
test$concatBool_2  <- sumup(test,  c("VAR_0383","VAR_0384"))
train$concatBool_3 <- sumup(train, c("VAR_0502","VAR_0503","VAR_0504","VAR_0505"))
test$concatBool_3  <- sumup(test,  c("VAR_0502","VAR_0503","VAR_0504","VAR_0505"))
train$concatBool_4 <- sumup(train, c("VAR_0566","VAR_0567"))
test$concatBool_4  <- sumup(test,  c("VAR_0566","VAR_0567"))
train$concatBool_5 <- sumup(train, c("VAR_0740","VAR_0741"))
test$concatBool_5  <- sumup(test,  c("VAR_0740","VAR_0741"))
train$concatBool_6 <- sumup(train, c("VAR_1162","VAR_1163","VAR_1164","VAR_1165"))
test$concatBool_6  <- sumup(test,  c("VAR_1162","VAR_1163","VAR_1164","VAR_1165"))

# Combine all columns with 2 unique values (binaries) into one new column - from Ivar
combinedTrain <- rep(0, nrow(train))
combinedTest  <- rep(0, nrow(test))
names <- names(train)
count <- 0
for (i in 1:length(names)) {
  col <- names[i]
  uniques <- unique(c(train[[col]], test[[col]]))
  vals <- length(uniques)
  if (vals == 2 && col != "target") {
    count <- count + 1
    t <- table(c(train[[col]], test[[col]]), useNA="ifany") # use all available data, both train and test data
    keys <- rownames(t)
    #freqs <- t # frequency values not used
    vals <- c(0,1)
    rnd <- runif(1)
    zeroOne <- mapvalues(train[[col]], from=keys, to=vals, warn_missing = F)
    combinedTrain <- combinedTrain + as.numeric(zeroOne)*(count + rnd) 
    zeroOne <- mapvalues(test[[col]], from=keys, to=vals, warn_missing = F)
    combinedTest <- combinedTest + as.numeric(zeroOne)*(count + rnd) 
  }
}
cat("Number of binary variables found=",count,fill=T)
train <- cbind(train, combinedTrain)
colnames(train)[ncol(train)] <- "combinedBinaries"
test <- cbind(test, combinedTest)
colnames(test)[ncol(test)] <- "combinedBinaries"

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
  trainCor <- cor( sample_n(train, corSampleN), method='spearman')
  trainCor[is.na(trainCor)] <- 0
  correlatedVars <- colnames(train)[findCorrelation(trainCor, cutoff = get("corr_threshold"), verbose = F)]
  cat("Removed highly correlated cols:", length(correlatedVars), 
      "(of", length(names(train)), ")", fill=T)
  train <- train[,!(names(train) %in% correlatedVars)]
  test  <- test[,!(names(test) %in% correlatedVars)]
}
cat("Dimensions of final train set:",dim(train), fill=T)
cat("Dimensions of final test set:",dim(test), fill=T)

newlyEngineeredFields <- setdiff(names(train), trainOriginalNames)
print("Newly Engineered fields:")
if (validationSetSize > 0) {
  newFields <- data.frame(ValAUC=sapply( newlyEngineeredFields, function(v) 
  { return (0.5 + abs(0.5-auc(y[valSetIndices], train[[v]][valSetIndices])))}),
  DevAUC=sapply( newlyEngineeredFields, function(v) 
  { return (0.5 + abs(0.5-auc(y[-valSetIndices], train[[v]][-valSetIndices])))}))
} else {
  newFields <- data.frame(ValAUC=NA,
                          DevAUC=sapply( newlyEngineeredFields, function(v) 
                          { return (0.5 + abs(0.5-auc(y, train[[v]])))}))
}
print(newFields)

###########################
# Model building
###########################

if (validationSetSize > 0) {
  xgtrain = xgb.DMatrix(as.matrix(train[-valSetIndices,]), label = y[-valSetIndices], missing = NA)
  xgval = xgb.DMatrix(as.matrix(train[valSetIndices,]), label = y[valSetIndices], missing = NA)
} else {
  xgtrain = xgb.DMatrix(as.matrix(train), label = y, missing = NA)
  xgval = NULL
}
earlyStop <- get("early.stop.round")
if (earlyStop == 0) {
  earlyStop <- NULL
}
gc()

valPerf <- NA
valStdDev <- NA
devPerf <- NA

#
# Optional N-Fold cross-validation
#

if (get("cv.nfold") > 0) {
  cvResults <- xgb.cv(  nrounds = get("nrounds")
                      , params = xgbParams
                      , data = xgtrain
                      , early.stop.round = earlyStop
                      , nfold = get("cv.nfold")
                      , print.every.n = get("print.every.n") )
     
  if (is.null(earlyStop)) {
    cv.bestIndex <- which.max(cvResults$test.auc.mean)
  } else {
    cv.bestIndex <- nrow(cvResults)
  }
  
  valPerf <- cvResults$test.auc.mean[cv.bestIndex]
  valStdDev <- cvResults$test.auc.std[cv.bestIndex]
  
  print("CV validation:")
  print(cvResults[cv.bestIndex,])
#   print(history)
}

#
# Actual model run
#

if (validationSetSize > 0) {
  watchlist <- list(val = xgval, dev = xgtrain)
} else {
  watchlist <- list(dev = xgtrain)
  earlyStop <- NULL
}

model = xgb.train(
  nrounds = get("nrounds")
  , params = xgbParams
  , data = xgtrain
  , early.stop.round = earlyStop
  , watchlist = watchlist
  , print.every.n = get("print.every.n")
)

if (is.null(earlyStop)) {
  train.bestIndex <- NA
} else {
  cat("Best XGB score:", model$bestScore,fill=T)
  train.bestIndex <- model$bestInd
}
cat("\nBest XGB iteration:", train.bestIndex, fill=T)

if (validationSetSize > 0) {
  if (is.na(train.bestIndex)) {
    valPerf <- as.double(auc(y[valSetIndices], predict(model, xgval)))
    devPerf <- as.double(auc(y[-valSetIndices], predict(model, xgtrain)))
  } else {
    valPerf <- as.double(auc(y[valSetIndices], predict(model, xgval, ntreelimit=train.bestIndex)))
    devPerf <- as.double(auc(y[-valSetIndices], predict(model, xgtrain, ntreelimit=train.bestIndex)))
  }
} else {
  # valPerf could be set by CV or still be NA
  devPerf <- as.double(auc(y, predict(model, xgtrain)))
}

cat("AUC val:", valPerf, "std:", valStdDev, fill=T)
cat("AUC dev:", devPerf, fill=T)

# feature importance
importance_mx <- xgb.importance(names(train), model=model)
print( xgb.plot.importance(head(importance_mx,50) )) 
ggsave("feature_importance.png")

###########################
# Score test set and write out
###########################

scoreKey <- valPerf
if (is.na(valPerf)) {
  scoreKey <- devPerf # validation not always available
}

doScoring <- !get("useSmallSample")
if (doScoring) {
  
  cat("Scoring test set.",fill=T)
  #rm("train")
  rm("xgval") 
  rm("xgtrain")
  gc()
  
  #   xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
  #   predictionsTest <- predict(model, xgtest, ntreelimit = bst)
  
  print("Generating predictions on test set:")
  # Process in batches to prevent OOM on windows
  predictionsTest <- rep(0.0, nrow(test))
  batchSize <- 10000
  for (rows in split(1:nrow(test), ceiling((1:nrow(test))/batchSize))) {
    xgtest <- xgb.DMatrix(as.matrix(test[rows,]), missing = NA)
    if (!is.na(train.bestIndex)) {
      predictionsTest[rows] <- predict(model, xgtest, ntreelimit = train.bestIndex)
    } else {
      predictionsTest[rows] <- predict(model, xgtest)
    }
  }
  
  #   all(predictionsTest == predictionsTest2)
  if (!file.exists("submissions")){
    dir.create(file.path(".", "submissions"))
  }
  subm <- data.frame(testIDs, predictionsTest)
  colnames(subm) <- c('ID','target')
  
  fname <- paste("submissions/", gsub("\\.","_",
                                      paste("subm",
                                            format(scoreKey,digits=6, nsmall=6),
                                            format(epoch, format="%Y%m%d_%H%M%S"), sep="_")),
                 ".csv",sep="")
  write.csv(subm, fname, row.names=FALSE)
  cat("Written submission score",scoreKey,"to",fname,fill=T)
  
  print("Generating predictions on train set:")
  # Process in batches to prevent OOM on windows
  predictionsTrain <- rep(0.0, nrow(train))
  batchSize <- 10000
  for (rows in split(1:nrow(train), ceiling((1:nrow(train))/batchSize))) {
    xgtrain <- xgb.DMatrix(as.matrix(train[rows,]), missing = NA)
    if (!is.na(train.bestIndex)) {
      predictionsTrain[rows] <- predict(model, xgtrain, ntreelimit = train.bestIndex)
    } else {
      predictionsTrain[rows] <- predict(model, xgtrain)
    }
  }
  prevDecisions <- data.frame(prevScore=predictionsTrain)
  fname <- paste("submissions/", gsub("\\.","_",
                                      paste("train",
                                            format(scoreKey,digits=6, nsmall=6),
                                            format(epoch, format="%Y%m%d_%H%M%S"), sep="_")),
                 ".csv",sep="")
  write.csv(prevDecisions, fname, row.names=FALSE)
  cat("Written previous train scores",scoreKey,"to",fname,fill=T)
  
} else {
  print("Not scoring test set")
}

duration <- as.double(difftime(now(),epoch,units='mins'))
cat('Duration:', duration, 'minutes', fill=T )

results <- list("when"=as.character(epoch),
                "bestScore"=scoreKey,
                "bestRound"=train.bestIndex,
                "valPerf"=valPerf,
                "valStdDev"=valStdDev,
                "devPerf"=devPerf,
                "duration"=duration,
                "settings"=settings)

rm("settings")