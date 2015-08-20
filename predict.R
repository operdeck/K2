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

###########################
# Read Data
###########################

train <- fread("./data/train-2.csv", header = T, sep = ",",stringsAsFactors=F,integer64="double",data.table=F)
test <- fread("./data/test-2.csv", header = T, sep = ",",stringsAsFactors=F,integer64="double",data.table=F)

if (useSample) { # speed up things
  train <- sample_frac(train, 0.20)
  test <- sample_frac(test, 0.20)
}

trainIndex <- createDataPartition(train$target, p=0.80, list=FALSE)
train_dev <- train[ trainIndex,]
train_val <- train[-trainIndex,]

###########################
# Data Analysis
###########################

isDate <- function(vec) { 
  all( grepl( "^\\d{2}[A-Z]{3}\\d{2}", vec[nzchar(vec)]) ) # check date fmt "12OCT13" (or empty)
}
isBoolean <- function(vec) { 
  all( grepl( "^true$|^false$", vec[nzchar(vec)]) )
}

dataMetrics <- nearZeroVar(train, saveMetrics=TRUE)
dataMetrics$className <- lapply(train,class)
dataMetrics$isSymbolic <- dataMetrics$className %in% c("factor", "character")
dataMetrics$nDistinct <- dataMetrics$percentUnique * nrow(train) / 100
symbolicFldNames <- rownames(dataMetrics) [dataMetrics$isSymbolic]
dataMetrics$isDate <- rownames(dataMetrics) %in% symbolicFldNames[ sapply(symbolicFldNames, function(colName) { isDate(train[[colName]]) } ) ]
dataMetrics$isBoolean <- rownames(dataMetrics) %in% symbolicFldNames[ sapply(symbolicFldNames, function(colName) { isBoolean(train[[colName]]) } ) ]
  
# Get AUC estimates for all predictors
print("Analyzing univariate performance for all predictors")
dataMetrics$AUC_raw_dev <- NA
dataMetrics$AUC_raw_val <- NA
dataMetrics$AUC_rec_dev <- NA
dataMetrics$AUC_rec_val <- NA
for (fldNo in 1:nrow(dataMetrics)) {
  fldName <- rownames(dataMetrics)[fldNo]
  cat("Field:",fldNo,fldName,fill=T)
  if (dataMetrics$nDistinct[fldNo] > 1 && fldName != "target") {
    vec <- train_dev[,fldNo]
    if (is.numeric(vec)) {
      # fit a mini regression model?
      lm.model <- lm(target ~ ., data=train_dev[, c("target",fldName)])
      pf_dev <- data.frame( train_dev[, fldNo] )
      pf_val <- data.frame( train_val[, fldNo] )
      names(pf_dev) <- c(fldName)
      names(pf_val) <- c(fldName)
      dataMetrics$AUC_raw_dev[fldNo] <- auc(train_dev$target, predict.lm(lm.model, pf_dev))
      dataMetrics$AUC_raw_val[fldNo] <- auc(train_val$target, predict.lm(lm.model, pf_val))
      sb <- createSymBin2(train_dev, fldName, "target", threshold=0)
      dataMetrics$AUC_rec_dev[fldNo] <- auc(train_dev$target, applySymBin(sb, train_dev[fldNo]))
      dataMetrics$AUC_rec_val[fldNo] <- auc(train_val$target, applySymBin(sb, train_val[fldNo]))
    } else {
      sb <- createSymBin2(train_dev, fldName, "target", threshold=0)
      dataMetrics$AUC_rec_dev[fldNo] <- auc(train_dev$target, applySymBin(sb, train_dev[fldNo]))
      dataMetrics$AUC_rec_val[fldNo] <- auc(train_val$target, applySymBin(sb, train_val[fldNo]))
    }
  }
}

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
# Fix up data/create extra fields
###########################

print("Feature selection")

# Convert dates to time to epoch and add derived field(s) like weekday
# TODO consider combinations between dates
processDateFlds <- function(ds, colNames) {
  extraDateFlds <- NULL
  for (col in colNames) {
    cat("Date column: ", col, fill=T)
    asDate <- strptime(ds[[col]], format="%d%b%y")
    asDate_wday <- wday(asDate)
    asDate_week <- week(asDate)
    if (is.null(extraDateFlds)) {
      extraDateFlds <- data.frame(asDate_wday, asDate_week)
      colnames(extraDateFlds) <- c(paste(col, "_wday", sep=""),
                                   paste(col, "_week", sep=""))
    } else {
      prevNames <- colnames(extraDateFlds)
      extraDateFlds <- cbind(extraDateFlds, asDate_wday, asDate_week)
      colnames(extraDateFlds) <- c(prevNames, 
                                   paste(col, "_wday", sep=""),
                                   paste(col, "_week", sep=""))
    }
    
    # convert to date and append field name
    ds[[col]] <- as.double(epoch - asDate)
    colnames(ds) [which(colnames(ds) == col)] <- paste(colnames(ds) [which(colnames(ds) == col)], "_asdate", sep="")
  }
  return(cbind(ds, extraDateFlds))
}
dateFldNames <- rownames(dataMetrics) [dataMetrics$isDate]
train_dev <- processDateFlds(train_dev, dateFldNames)
train_val <- processDateFlds(train_val, dateFldNames)
test <- processDateFlds(test, dateFldNames)

# Replace remaining symbolic fields by mean outcome
remainingSymbolicColumns <- rownames(dataMetrics) [dataMetrics$isSymbolic & !dataMetrics$isDate]
cat("Symbinning remaining symbolic columns:", remainingSymbolicColumns, fill=T)
for (colName in remainingSymbolicColumns) {
  cat("Symbinning", colName, fill=T)
  binner <- createSymBin2(train_dev, colName, "target", threshold=0.01) # min casecount per bin
  sb.plotOne(binner, train_dev, train_val, test, colName, "target")
  train_dev[[colName]] <- applySymBin(binner, train_dev[[colName]])
  train_val[[colName]] <- applySymBin(binner, train_val[[colName]])
  test[[colName]]      <- applySymBin(binner, test[[colName]])
}

# Perhaps numerics should be replaced by mean outcome as well? Some are not numeric but categorical.
# Maybe plot first
# nb.plotAll(train_dev,train_val,test, "target")

# Brute force NA imputation - not sure. Maybe not needed, and knn would be beter anyhow.
train_dev <- imputeNAs(train_dev)
train_val <- imputeNAs(train_val)
test <- imputeNAs(test)

# Remove zero (& near zero) variance fields. Recalculate because of addition of extra fields.
# TODO: just run DA again on the mutilated fields
finalVars <- nearZeroVar(train_dev, saveMetrics=T)
finalVarsZV <- rownames(finalVars) [finalVars$zeroVar | finalVars$nzv]
cat("Zero and near-zero variance columns after data analysis (removed):", finalVarsZV, fill=T)
train_dev <- train_dev[,!(names(train_dev) %in% finalVarsZV)]
train_val <- train_val[,!(names(train_val) %in% finalVarsZV)]
test      <- test[,!(names(test) %in% finalVarsZV)]

# Correlations
cat("Correlation", fill=T)
trainCor <- cor( sample_n(select(train_dev, -target), 1000))
trainCor[is.na(trainCor)] <- 0
#corrplot(trainCor, method="circle", type="upper", order = "hclust", addrect = 3)
highlyCorrelatedVars <- rownames(trainCor) [findCorrelation(trainCor, cutoff=0.98)]
if (length(highlyCorrelatedVars) > 0) {
  cat("Removing highly correlated variables: ", highlyCorrelatedVars, fill=TRUE)
  train_dev <- train_dev[!(names(train_dev) %in% highlyCorrelatedVars)]
  train_val <- train_val[!(names(train_val) %in% highlyCorrelatedVars)]
  test      <- test[!(names(test) %in% highlyCorrelatedVars)]
  
  #trainCor <- cor(select(imputeNAs(train_dev), -IsClick))
  #corrplot(trainCor, method="circle", type="upper", order = "hclust", addrect = 3)
} else {
  print("No highly correlated variables according to trim.matrix")
}

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
                                        n.trees = 50,
                                        interaction.depth = 20,
                                        shrinkage = 0.01,
                                        cv.folds=3,
                                        # n.cores=2
                                        verbose= T))
  cat("Duration:",modelTime,fill=T)
  cat("Duration:",modelTime[3]/3600,"hrs",fill=T)
  print(model)
  
  best.iter <- gbm.perf(model, method="cv")
  print(best.iter)
  print(head( summary(model, n.trees=best.iter), 30 )) # plot all and print top-N predictors
  print(pretty.gbm.tree(model, best.iter))
}


# Get score on validation set
# make predictions
# inp <- imputeNAs( select(train_val, -IsClick) )
#prep <- preProcess(inp, method='knnImpute')
predictions <- predict(model, select(train_val, -target), best.iter, type="response")
cat('Some predictions: ', head(predictions), fill=T)
cat('Validation set AUC:', auc(train_val$target, predictions), fill=T )

###########################
# Apply model on test set
###########################

if (!useSample) {
  print("Scoring test set")
  pr <- predict(model, test, best.iter, type="response")
  
  subm <- data.frame(test$ID, pr)
  colnames(subm) <- c('ID','target')
  write.csv(subm, "./submission.csv", row.names=FALSE)
} else {
  print("Not scoring test set")
}
