# This is the script that produces LB 0.7985 for me with more features engineering
# Version 1 from https://www.kaggle.com/mrooijer/springleaf-marketing-response/xgboost-run-local/code

# TODO, bring in features of predict.R:
# - practicalities: use 'fread', rewrite wrinting of results
# - more rounds
# - adding of date variants
# - adding of 'countNA' feature
# - handling of symbolics with -1 fields
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

#--------- P A R A M S ------------------------------------------------

param0 <- list(
  # general , non specific params - just guessing
  "objective"  = "binary:logistic"
  , "eval_metric" = "auc"
  , "eta" = 0.01
  , "subsample" = 0.7
  , "colsample_bytree" = 0.5
  , "min_child_weight" =6
  , "max_depth" = 9
  , "alpha" = 4
  , "nthreads" = 3
)

settings.doScoring <- T

version="local"
set.seed(1948)
epoch <- now()

# -------- d a t a ---------------

train <- fread( "data/train-2.csv",header = T, sep = ",",
                stringsAsFactors=F,integer64="double",data.table=F )
y <- train$target
train <- train[,-c(1, 1934)]
test <- fread( "data/test-2.csv",header = T, sep = ",",
               stringsAsFactors=F,integer64="double",data.table=F)
testIDs <- test$ID
test <- test[,-1]

# Data analysis

for (i in 1:ncol(train)) {
  if (class(train[[i]]) == "character") {
    tmp= as.numeric(as.factor(c(train[[i]], test[[i]])))
    train[[i]]<- head(tmp, nrow(train))
    test[[i]]<- tail(tmp, nrow(test))
  }
}

# NA handling

train[is.na(train)] <- -9999
test[is.na(test)] <- -9999

# some simple feature cleaning/engineering boosts the LB-AUC by 0.0025. 

hold <- sample(1:nrow(train), 15000) #10% training data for stopping
xgtrain = xgb.DMatrix(as.matrix(train[-hold,]), label = y[-hold], missing = NA)
xgval = xgb.DMatrix(as.matrix(train[hold,]), label = y[hold], missing = NA)
gc()
watchlist <- list('val' = xgval)
model = xgb.train(
  nrounds = 250   # increase for more results at home
  , params = param0
  , data = xgtrain
  , early.stop.round = 5
  , watchlist = watchlist
  , print.every.n = 5
)

cat("Best XGB iteration:",model$bestInd,fill=T)
cat("Best XGB score:",model$bestScore,fill=T)
cat("Number of vars: ", length(colnames(train)), fill=T)

# feature importance
importance_mx <- xgb.importance(names(train), model=model)
print( xgb.plot.importance(head(importance_mx,50) )) 

# Watchlist best Score 0.775643

# -------- w r i t e   r e s u l t  ------

if (settings.doScoring) {
  
  cat("Scoring test set.",fill=T)
  rm("train")
  rm("xgval"); gc()
  train_val <- predict(model, newdata=xgtrain)
  rm("xgtrain")
  xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
  
  bst <- model$bestInd
  preds_out <- predict(model, xgtest, ntreelimit = bst)

  subm <- data.frame(testIDs, preds_out)
  colnames(subm) <- c('ID','target')
  write.csv(subm, "./new_submission.csv", row.names=FALSE)
  print("Written submission")
} else {
  print("Not scoring test set")
}

cat('total time:', difftime(now(),epoch,units='mins'), 'minutes', fill=T )

