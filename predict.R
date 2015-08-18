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

setwd("~/Documents/science/kaggle/springleaf/K2")
set.seed(314159)
Sys.setlocale("LC_TIME", "C")
findTuningParams <- F # set to T to use caret for finding model tuning parameters, otherwise direct model
useSample <- F # set to true to quickly test changes on a subset of the training data
epoch <- now()

# read data
train <- fread("./data/train-2.csv", header = T, sep = ",",stringsAsFactors=T)
test <- fread("./data/test-2.csv", header = T, sep = ",",stringsAsFactors=T)

if (useSample) {
  train <- sample_frac(train, 0.20)
}

###########################
# Fix up data
###########################

# Deselect predictors with zero variance
dataMetrics <- nearZeroVar(train, saveMetrics=TRUE)
#print(dataMetrics)
zeroVarianceColumns <- rownames(dataMetrics) [dataMetrics$zeroVar]
cat("Removing zero variance columns:", zeroVarianceColumns, fill=T)
train <- train[,!(names(train) %in% zeroVarianceColumns), with=F] # syntax for data.table
test  <- test[,!(names(test) %in% zeroVarianceColumns), with=F]

# Find symbolic fields
classtypes <- lapply(train,class)
symbolicFlds <- which( classtypes == "factor" | classtypes == "character" )
head(train[,symbolicFlds,with=F])

# Find date fields
isDate <- function(vec) { 
  all( grepl( "^\\d{2}[A-Z]{3}\\d{2}", vec[nzchar(vec)]) ) # are all dates (or empty)
}
dateFlds <- symbolicFlds[ sapply(symbolicFlds, function(colIdx) { isDate(train[[colIdx]]) } ) ]

# Convert dates to time to epoch and add derived field(s) like weekday
processDateFlds <- function(ds, dateFieldIndices) {
  extraDateFlds <- NULL
  for (col in dateFieldIndices) {
    cat("Date column: ", names(ds)[col], fill=T)
    asDate <- strptime(ds[[col]], format="%d%b%y")
    asDate_wday <- wday(asDate)
    if (is.null(extraDateFlds)) {
      extraDateFlds <- data.frame(asDate_wday)
    } else {
      extraDateFlds <- cbind(extraDateFlds, asDate_wday)
    }
    colnames(extraDateFlds)[ncol(extraDateFlds)] <- paste(names(ds)[col], "_wday", sep="")
    ds[[col]] <- as.double(epoch - asDate)
  }
  return(cbind(ds, extraDateFlds))
}
train <- processDateFlds(train, dateFlds)
test <- processDateFlds(test, dateFlds)

# Find boolean fields, replace by 1/0
isBoolean <- function(vec) { 
  all( grepl( "^true$|^false$", vec[nzchar(vec)]) )
}
boolFlds <- symbolicFlds[ sapply(symbolicFlds, function(colIdx) { isBoolean(train[[colIdx]]) } ) ]
for (col in boolFlds) {
  cat("Boolean column: ", names(train)[col], fill=T)
  train[[col]] <- ifelse( train[[col]] == "true", 1, ifelse( train[[col]] == "false", 0, NA))
  test[[col]] <- ifelse( test[[col]] == "true", 1, ifelse( test[[col]] == "false", 0, NA))
}

# TODO: Replace remaining symbolic fields by mean outcome
# for now: removed
remainingSymbolicFlds <- symbolicFlds[ !(symbolicFlds %in% dateFlds) && !(symbolicFlds %in% boolFlds) ]
train <- train[, -remainingSymbolicFlds, with=F]
test <- test[, -remainingSymbolicFlds, with=F] 

#cat("Near zero variance:",colnames(train) [nzv(train)],fill=TRUE)

# brute force NA imputation
# todo: use knn or at least mean
train[is.na(train)] <- 9999
test[is.na(test)] <- 9999

# split 80/20
trainIndex <- createDataPartition(train$target, p=0.80, list=FALSE)
train_dev <- as.data.frame(train)[ trainIndex,]
train_val <- as.data.frame(train)[-trainIndex,]

###########################
# Fit model
###########################
print('Fit model')
if (findTuningParams) {
  # unfinished - see http://topepo.github.io/caret/training.html
  gbmGrid <- expand.grid(
    interaction.depth = seq(10,20,by=10), # splits 
    n.trees=seq(5,10,by=5),             # number of trees; more often better
    shrinkage=c(0.01),           # learning rate parameter; smaller often better
    n.minobsinnode=c(10))
  
  modelTime <- system.time(model <- train(target ~ ., data = train_dev, 
                                          method="gbm",
                                          tuneGrid=gbmGrid,
                                          preProcess = c("knnImpute"),
                                          # cv.folds=5
                                          # n.cores=2
                                          verbose= T))
  #do this if model is from caret 'train'
  #trellis.par.set(caretTheme())
  #print( plot(model) )
  
  #va <- varImp(model, scale = FALSE) # Caret variable importance
  #print(va)
  #print( plot(va, top=20) )
  
} else {
  modelTime <- system.time(model <- gbm(target ~ ., data = train_dev, 
                                        distribution = "bernoulli",
                                        n.trees = 10,
                                        interaction.depth = 10,
                                        shrinkage = 0.01,
                                        cv.folds=3,
                                        # n.cores=2
                                        verbose= T))
  best.iter <- gbm.perf(model, method="cv")
  print(best.iter)
  print(head( summary(model, n.trees=best.iter), 30 )) # plot all and print top-N predictors
  print(pretty.gbm.tree(model, best.iter))
}

cat("Duration:",modelTime,fill=T)
cat("Duration:",modelTime[3]/3600,"hrs",fill=T)
print(model)

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
