# This is the script that produces LB 0.7985 for me with more features engineering
# 

#--------- L I B R A R Y ------------------------------------------------
library(xgboost)
library(readr)

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

version="local"
subversion = 1
set.seed(1948 ^ subversion)

PROD = F

# -------- d a t a ---------------

train <- read_csv( "data/train-2.csv" )
y <- train$target
train <- train[,-c(1, 1934)]
test <- read_csv( "data/test-2.csv")
test <- test[,-1]
for (i in 1:ncol(train)) {
  if (class(train[, i]) == "character") {
    tmp= as.numeric(as.factor(c(train[,i], test[,i])))
    train[,i]<- head(tmp, nrow(train))
    test[,i]<- tail(tmp, nrow(test))
  }
}
train[is.na(train)] <- -9999
test[is.na(test)] <- -9999

# some simple feature cleaning/engineering boosts the LB-AUC by 0.0025. 

# -------- r u n s  i n 8 GB at home but not here ---------------

if (PROD){
  hold <- sample(1:nrow(train), 15000) #10% training data for stopping
  xgtrain = xgb.DMatrix(as.matrix(train[-hold,]), label = y[-hold], missing = NA)
  xgval = xgb.DMatrix(as.matrix(train[hold,]), label = y[hold], missing = NA)
  gc()
  watchlist <- list('val' = xgval)
  model = xgb.train(
    nrounds = 50   # increase for more results at home
    , params = param0
    , data = xgtrain
    , early.stop.round = 5
    , watchlist = watchlist
    , print.every.n = 5
  )
  cat("\nWatchlist best Score", model$bestScore)
  # Watchlist best Score 0.775643
  
  # -------- w r i t e   r e s u l t  ------
  
  if (model$bestScore > 0.79) {
    
    cat("\nProducing Guesstimate....")
    rm("train")
    rm("xgval"); gc()
    train_val <-predict(model, newdata=xgtrain)
    rm("xgtrain")
    xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
    
    bst <- model$bestInd
    preds_out <- predict(model, xgtest, ntreelimit = bst)
    
    sub <-
      read_csv("sample_submission.csv")
    sub$target <- preds_out
    write_csv(sub, "test_submission.csv")
    et <- -start.time + proc.time()[3]
    cat("\nTotal elapsed in seconds: ", et)
    cat("\nOutput okay: end ", format(Sys.time()),"\n")
  }
}