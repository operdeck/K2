# "This is the script that produces LB 0.7985 for me with more features engineering"
# Version 1 from https://www.kaggle.com/mrooijer/springleaf-marketing-response/xgboost-run-local/code

# TODO, bring in features of predict.R:
# X practicalities: use 'fread', rewrite wrinting of results
# X more rounds
# X adding of date variants
# X adding of 'countNA' feature
# X handling of symbolics with -1 fields
# - symbolic binning for 'character' columns
# - numeric binning
# - univariate selection
# - deselection of correlated predictors
# - deselection of linearly correlated predictors

#--------- L I B R A R Y ------------------------------------------------
library(xgboost)
library(readr)
library(data.table)
library(bit64)
library(lubridate)
library(pROC)
library(caret)

#--------- P A R A M S ------------------------------------------------

# params doc: https://github.com/dmlc/xgboost/blob/master/doc/parameter.md

param0 <- list(
  # general , non specific params - just guessing
  "objective"  = "binary:logistic"
  , "eval_metric" = "auc"
  , "eta" = 0.005 # instead of 0.01 --> improvement
  , "subsample" = 0.7
  , "colsample_bytree" = 0.5
  , "min_child_weight" =6
  , "max_depth" = 9
  , "alpha" = 4
  , "lambda" = 10 # instead of not present at all; 100 is too slow
  , "nthreads" = 3
)

settings.doScoring <- T

version="local"
set.seed(1948)
epoch <- now()

###########################
# Data read
###########################

train <- fread( "data/train-2.csv",header = T, sep = ",",
                stringsAsFactors=F,integer64="double",data.table=F )
y <- train$target
train <- train[,-c(1, 1934)]
test <- fread( "data/test-2.csv",header = T, sep = ",",
               stringsAsFactors=F,integer64="double",data.table=F)
testIDs <- test$ID
test <- test[,-1]

###########################
# Data preparation
###########################

# Quickly replace missings by NA in symbolic fields
# (should influence NA row count below)
# BUT POTENTIALLY HARMFUL
symNAs <- c("-1","")
for (colName in colnames(train)[which(sapply(train, function(col) { return (!is.numeric(col)) } ))]) {
  #   print(createSymbin(train[[colName]],train$target))
  train[[colName]][train[[colName]] %in% symNAs] <- NA
  test[[colName]][test[[colName]] %in% symNAs] <- NA
  #   print(createSymbin(train[[colName]],train$target))
}

# Special extra fields - before doing any further processing
# Add variable with nr of missing values per row. Make double to ensure numeric treatment.
print("Counting NA's per row - time consuming")
train$xtraNumNAs <- as.double(apply(train, 1, function(z) sum(is.na(z))))
test$xtraNumNAs <- as.double(apply(test, 1, function(z) sum(is.na(z))))

# Date field detection

isDate <- function(vec) { 
  all( grepl( "^\\d{2}[A-Z]{3}\\d{2}", vec[nzchar(vec)]) ) # check date fmt "12OCT13" (or empty)
}

dateFldNames <- colnames(train)[sapply(colnames(train), function(colName) 
{ class(train[[colName]]) == "character" & isDate(na.omit(train[[colName]])) } )]
cat("Date fields: ", dateFldNames, " (", length(dateFldNames), ")", fill=T)

# Convert dates to time to epoch and add derived field(s) like weekday
processDateFlds <- function(ds, colNames) {
  result <- ds
  for (colName in colNames) {
    #cat("Date: ", colName, fill=T)
    # what happens with NAs?
    asDate <- strptime(ds[[colName]], format="%d%b%y")
    result[[paste(colName, "_wday", sep="")]] <- wday(asDate)
    result[[paste(colName, "_mday", sep="")]] <- mday(asDate)
    result[[paste(colName, "_week", sep="")]] <- week(asDate)
    result[[colName]] <- as.double(difftime(epoch, asDate,units='days'))
  }
  return(result)
}

# Create combinations of all possible date pairs. Perhaps some date differences are a good predictor.
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

train <- processDateFlds(train, dateFldNames)
test <- processDateFlds(test, dateFldNames)

if (length(dateFldNames) > 0) {
  train <- combineDates(train, dateFldNames)
  test <- combineDates(test, dateFldNames)
}

###########################
# Data analysis
###########################

print("Check (near) zero variance")

# time consuming:
# nfs <- nearZeroVar(train, saveMetrics = FALSE)

# TODO switch back to nearZeroVar()
zeroVarCols <- colnames(train)[sapply(colnames(train), function(colName) 
{return (length(unique(train[[colName]])) < 2)})]
cat("Removed zero variance cols:", length(zeroVarCols), fill=T)
train <- train[,!(names(train) %in% zeroVarCols)]
test  <- test[,!(names(test) %in% zeroVarCols)]

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

train[is.na(train)] <- -9999
test[is.na(test)] <- -9999

# some simple feature cleaning/engineering boosts the LB-AUC by 0.0025. 

###########################
# Model building
###########################

hold <- sample(1:nrow(train), 15000) #10% training data for stopping
xgtrain = xgb.DMatrix(as.matrix(train[-hold,]), label = y[-hold], missing = NA)
xgval = xgb.DMatrix(as.matrix(train[hold,]), label = y[hold], missing = NA)
gc()
watchlist <- list('val' = xgval, 'dev' = xgtrain)
model = xgb.train(
  nrounds = 2000   # increase for more results at home
  , params = param0
  , data = xgtrain
  , early.stop.round = 100
  , watchlist = watchlist
  , print.every.n = 10
)

bst <- model$bestInd

cat("Best XGB iteration:", bst, fill=T)
cat("Best XGB score:", model$bestScore,fill=T)
cat("Number of vars: ", length(colnames(train)), fill=T)

cat("AUC val:", auc(y[hold], predict(model, xgval, ntreelimit=bst)), fill=T)
cat("AUC dev:", auc(y[-hold], predict(model, xgtrain, ntreelimit=bst)), fill=T)

# feature importance
importance_mx <- xgb.importance(names(train), model=model)
print( xgb.plot.importance(head(importance_mx,50) )) 

###########################
# Score test set and write out
###########################

if (settings.doScoring) {
  
  cat("Scoring test set.",fill=T)
  rm("train")
  rm("xgval") 
  rm("xgtrain")
  gc()
  xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
  
  preds_out <- predict(model, xgtest, ntreelimit = bst)
  
  subm <- data.frame(testIDs, preds_out)
  colnames(subm) <- c('ID','target')
  write.csv(subm, "./new_submission.csv", row.names=FALSE)
  print("Written submission")
} else {
  print("Not scoring test set")
}

cat('total time:', difftime(now(),epoch,units='mins'), 'minutes', fill=T )

