# load required packages
require(xgboost) # not on CRAN? devtools::install_github('dmlc/xgboost',subdir='R-package')
library(data.table)
require(bit64)
library(gbm)
library(tidyr)
library(ggplot2) 
library(scales)
library(caret)
library(lubridate)
library(corrplot)
library(plyr)
library(dplyr)
library(pROC)

# TODO: use xgboost vs gbm
# See 
# https://www.kaggle.com/michaelpawlus/springleaf-marketing-response/xgboost-example-0-76178
# https://www.kaggle.com/michaelpawlus/springleaf-marketing-response/xgboost-example-0-76178/discussion

macWD <- "~/Documents/science/kaggle/springleaf/K2"
winWD <- "D:/usr/science/kaggle/springleaf/K2"
if (file.exists(macWD)) {
  setwd(macWD)
} else {
  setwd(winWD)
}
source('funcs.R') # common funcs - inherited from previous Kaggle project

try(dev.off(),silent=T) # prevent graphics errors
set.seed(314159)
Sys.setlocale("LC_TIME", "C")
findTuningParams <- F # set to T to use caret for finding model tuning parameters, otherwise direct model
useSample <- F # set to true to quickly test changes on a subset of the training data
epoch <- now()
symbinThreshold <- 0.01

###########################
# Read Data
###########################

train <- fread("./data/train-2.csv", header = T, sep = ",",stringsAsFactors=F,integer64="double",data.table=F)
test <- fread("./data/test-2.csv", header = T, sep = ",",stringsAsFactors=F,integer64="double",data.table=F)
for (col in 1:ncol(train)) { # change logicals into numerics
  if (is.logical(train[[col]])) train[[col]] <- as.numeric(train[[col]])
}
for (col in 1:ncol(test)) {
  if (is.logical(test[[col]])) test[[col]] <- as.numeric(test[[col]])
}
if (useSample) { # speed up things
  train <- sample_frac(train, 0.20)
  test <- sample_frac(test, 0.20)
}
testIDs <- test$ID
trainIndex <- createDataPartition(train$target, p=0.80, list=FALSE)
train_dev <- train[ trainIndex,]
train_val <- train[-trainIndex,]

###########################
# Data Analysis
###########################

dataMetrics <- dataAnalysis(train, train_dev, train_val, symbinThreshold)

# dump & write to files for external analysis
print(head(dataMetrics))
write.table(cbind(rownames(dataMetrics), data.frame(lapply(dataMetrics, as.character), stringsAsFactors=FALSE)), 
            "./dataMetrics.csv", sep=";", row.names=F,
            col.names = c("Field", names(dataMetrics)))

# Save a dataset with the vars with many distincts for review
# NB: VAR_0200 seems to be a region, perhaps use that. But VAR_0274 is state already.
dataSetManyDistincts <- train[, which(dataMetrics$nDistinct > 50 & dataMetrics$isSymbolic & !dataMetrics$isDate)]
write.table(dataSetManyDistincts, "./dataSetManyDistincts.csv", sep=";", row.names=F)

write.table(sample_n(train, 10000), "./trainSample10k.csv", sep=";")

###########################
# Add/remove fields
###########################

print("Feature engineering")

# Convert dates to time to epoch and add derived field(s) like weekday
# TODO consider combinations between dates
processDateFlds <- function(ds, colNames) {
  extraDateFlds <- NULL
  for (colName in colNames) {
    #cat("Date: ", colName, fill=T)
    asDate <- strptime(ds[[colName]], format="%d%b%y")
    asDate_wday <- wday(asDate)
    asDate_week <- week(asDate)
    if (is.null(extraDateFlds)) {
      extraDateFlds <- data.frame(asDate_wday, asDate_week)
      colnames(extraDateFlds) <- c(paste(colName, "_wday", sep=""),
                                   paste(colName, "_week", sep=""))
    } else {
      prevNames <- colnames(extraDateFlds)
      extraDateFlds <- cbind(extraDateFlds, asDate_wday, asDate_week)
      colnames(extraDateFlds) <- c(prevNames, 
                                   paste(colName, "_wday", sep=""),
                                   paste(colName, "_week", sep=""))
    }
    
    # convert field itself to date and append field name
    ds[[colName]] <- as.double(epoch - asDate)
    colnames(ds) [which(colnames(ds) == colName)] <- paste(colName, "_asdate", sep="")
  }
  return(cbind(ds, extraDateFlds))
}

dateFldNames <- rownames(dataMetrics) [dataMetrics$isDate]
cat("Date fields: ", dateFldNames, fill=T)

train_dev <- processDateFlds(train_dev, dateFldNames)
train_val <- processDateFlds(train_val, dateFldNames)
test <- processDateFlds(test, dateFldNames)

# Create combinations of all possible date pairs. Perhaps some date differences are a good predictor.
combineDates <- function(ds, fldNames) {
  if (length(fldNames) >= 2) {
    first <- fldNames[1]
    rest <- fldNames[2:length(fldNames)]
    for (second in rest) {
      combinedName <- paste(first, second, sep="_")
      cat("Combine dates: ", combinedName, fill=T)
      ds[[combinedName]] <- ds[[second]] - ds[[first]]
    }
    return(combineDates(ds, rest))
  } else {
    return(ds)
  }
}
train_dev <- combineDates(train_dev, paste( dateFldNames, '_asdate', sep=""))
train_val <- combineDates(train_val, paste( dateFldNames, '_asdate', sep=""))
test <- combineDates(test, paste( dateFldNames, '_asdate', sep=""))

# Replace remaining symbolic fields by mean outcome
remainingSymbolicColumns <- rownames(dataMetrics) [dataMetrics$isSymbolic & !dataMetrics$isDate]
cat("Symbinning remaining symbolic columns: ", remainingSymbolicColumns, fill=T)
for (colName in remainingSymbolicColumns) {
  cat("Symbinning", colName, fill=T)
  binner <- createSymBin2(train_dev, colName, "target", threshold=symbinThreshold) # min casecount per bin

  # too many to review individually, skip plots:
  # sb.plotOne(binner, train_dev, train_val, test, colName, "target")
  
  train_dev[[colName]] <- applySymBin(binner, train_dev[[colName]])
  train_val[[colName]] <- applySymBin(binner, train_val[[colName]])
  test[[colName]]      <- applySymBin(binner, test[[colName]])

  colnames(train_dev) [which(colnames(train_dev) == colName)] <- 
    colnames(train_val) [which(colnames(train_val) == colName)] <- 
    colnames(test) [which(colnames(test) == colName)] <- paste(colName, "_symbin", sep="")
}

# Perhaps numerics should be replaced by mean outcome as well? Some are not numeric but categorical.
# Maybe plot first
# nb.plotAll(train_dev,train_val,test, "target")

# Write data analysis results again, now including the newly created fields
newFields <- c( setdiff(colnames(train_dev), rownames(dataMetrics)), "target" )
metricsNewFields <- dataAnalysis(rbind(train_dev[, newFields], 
                                       train_val[, newFields]), 
                                 train_dev[, newFields], 
                                 train_val[, newFields],
                                 symbinThreshold)
dataMetrics <- rbind(dataMetrics, metricsNewFields)
write.table(cbind(rownames(dataMetrics), 
                  data.frame(lapply(dataMetrics, as.character), stringsAsFactors=FALSE)), 
            "./dataMetrics.csv", sep=";", row.names=F,
            col.names = c("Field", names(dataMetrics)))

print("Feature selection")

# Brute force NA imputation - not sure. Maybe not needed, and knn would be beter anyhow.
train_dev <- imputeNAs(train_dev)
train_val <- imputeNAs(train_val)
test <- imputeNAs(test)

# Univariate selection
univariateAUCCutOff <- 0.52
removedFields <- rownames(dataMetrics) [dataMetrics$isSymbolic | 
                                          (!is.na(dataMetrics$AUC_rec_val) &
                                             dataMetrics$AUC_rec_val < univariateAUCCutOff & 
                                             dataMetrics$AUC_rec_val > (1-univariateAUCCutOff))]
cat("Removed after univariate analysis:", removedFields, " (", length(removedFields), ")", fill=T)
train_dev <- train_dev[,!(names(train_dev) %in% removedFields)]
train_val <- train_val[,!(names(train_val) %in% removedFields)]
test      <- test[,!(names(test) %in% removedFields)]
cat("Remaining:", colnames(train_dev), " (", length(colnames(train_dev)), ")", fill=T)

###########################
# Correlations
###########################
cat("Correlation", fill=T)
trainCor <- cor( sample_n(select(train_dev, -target), 10000))
trainCor[is.na(trainCor)] <- 0
#corrplot(trainCor, method="circle", type="upper", order = "hclust", addrect = 3)
highlyCorrelatedVars <- rownames(trainCor) [findCorrelation(trainCor, cutoff=0.98)]
if (length(highlyCorrelatedVars) > 0) {
  cat("Removing highly correlated variables: ", highlyCorrelatedVars, " (", length(highlyCorrelatedVars), ")", fill=T)
  train_dev <- train_dev[!(names(train_dev) %in% highlyCorrelatedVars)]
  train_val <- train_val[!(names(train_val) %in% highlyCorrelatedVars)]
  test      <- test[!(names(test) %in% highlyCorrelatedVars)]
  
  #trainCor <- cor(select(imputeNAs(train_dev), -IsClick))
  #corrplot(trainCor, method="circle", type="upper", order = "hclust", addrect = 3)
} else {
  print("No highly correlated variables according to trim.matrix")
}

cat("Remaining:", colnames(train_dev), " (", length(colnames(train_dev)), ")", fill=T)

# Dump the data here. Taking the col names from our analysis but using the original data.
# Just for the purpose of analyzing with ADM/PAD limiting the number of columns to < 1000 - hopefully.
if (F) {
  keep_cols <- names(train_dev)
  test_ori <- fread("./data/test-2.csv", header = T, sep = ",",stringsAsFactors=T, nrows=1000) # just for cols
  drop_cols <- setdiff(names(test_ori),keep_cols)
  
  train_ori <- fread("./data/train-2.csv", header = T, sep = ",",stringsAsFactors=T, drop=drop_cols)
  test_ori <- fread("./data/test-2.csv", header = T, sep = ",",stringsAsFactors=T, drop=drop_cols)  
  
  cat("Dimension: ", dim(train_ori), fill=T)
  write.csv(sample_n(train_ori, 10000), "./data/train-trunc-small.csv", row.names=FALSE)
  write.csv(sample_n(test_ori, 10000), "./data/test-trunc-small.csv", row.names=FALSE)
  write.csv(train_ori, "./data/train-trunc-full.csv", row.names=FALSE)
  write.csv(test_ori, "./data/test-trunc-full.csv", row.names=FALSE)
}

###########################
# Fit model
###########################
print('Fit model')
if (findTuningParams) {
  # unfinished - see http://topepo.github.io/caret/training.html

  train_dev$target <- as.factor(train_dev$target) # model needs factor
  levels(train_dev$target) <- c("no", "yes") # predicting probs needs this for some reason
  
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 5,
    ## repeated ten times
    repeats = 1,
    classProbs = TRUE,
    summaryFunction = twoClassSummary)
  
  gbmGrid <- expand.grid(
    interaction.depth = seq(10,30,by=10), # splits 
    n.trees=c(100),             # number of trees; more often better
    shrinkage=c(0.02,0.01,0.005),           # learning rate parameter; smaller often better
    n.minobsinnode=c(10))
  
  modelTime <- system.time(model <- train(target ~ ., data = train_dev, 
                                          method="gbm",
                                          tuneGrid=gbmGrid,
                                          #preProcess = c("knnImpute"),
                                          metric = "ROC",
                                          trControl = fitControl,
                                          # cv.folds=5
                                          # n.cores=2
                                          verbose= T))

  cat("Duration:",modelTime,fill=T)
  cat("Duration:",modelTime[3]/3600,"hrs",fill=T)
  print(model)
  
  trellis.par.set(caretTheme())
  print( plot(model) )
  
  va <- varImp(model, scale = FALSE) # Caret variable importance
  print(va)
  print( plot(va, top=50) )
  
  stop("Stopping after caret parameter tuning step")
  
} else {
  modelTime <- system.time(model <- gbm(target ~ ., data = train_dev, 
                                        distribution = "bernoulli",
                                        n.trees = 300,
                                        interaction.depth = 20,
                                        shrinkage = 0.02,
                                        cv.folds=3,
                                        # n.cores=2
                                        verbose= T))
  cat("Duration:",modelTime,fill=T)
  cat("Duration:",modelTime[3]/3600,"hrs",fill=T)
  print(model)
  
  best.iter <- gbm.perf(model, method="cv")
  print(best.iter)
  print(head( summary(model, n.trees=best.iter), 30 )) # plot all and print top-N predictors
  #print(pretty.gbm.tree(model, best.iter))
}


# Get score on validation set
# make predictions
# inp <- imputeNAs( select(train_val, -IsClick) )
#prep <- preProcess(inp, method='knnImpute')
predictions <- predict(model, select(train_val, -target), best.iter, type="response")
#cat('Some predictions: ', head(predictions), fill=T)
predictions_dev <- predict(model, select(train_dev, -target), best.iter, type="response")
cat('Val AUC:', auc(train_val$target, predictions), 
    '#predictors:', ncol(test), 
    'total time:', (now()-epoch)/60, 'minutes',
    fill=T )
cat('Dev AUC:', auc(train_dev$target, predictions_dev), 
    fill=T )

###########################
# Apply model on test set
###########################

if (!useSample) {
  print("Scoring test set")
  pr <- predict(model, test, best.iter, type="response")
  
  subm <- data.frame(testIDs, pr)
  colnames(subm) <- c('ID','target')
  write.csv(subm, "./submission.csv", row.names=FALSE)
} else {
  print("Not scoring test set")
}
