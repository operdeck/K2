# load required packages
#require(readr)
require(xgboost) # not on CRAN? devtools::install_github('dmlc/xgboost',subdir='R-package')
library(data.table)
require(bit64)
library(gbm)
set.seed(314159)

# read data
xtrain <- fread("./data/train-2.csv", header = T, sep = ",",stringsAsFactors=T)
#id_train <- xtrain$ID; xtrain$ID <- NULL
#y <- xtrain$target; xtrain$target <- NULL

xtest <- fread("./data/test-2.csv", header = T, sep = ",",stringsAsFactors=T)
#id_test <- xtest$ID; xtest$ID <- NULL

xtrain[is.na(xtrain)] <- 9999 # ???
xtest[is.na(xtest)] <- 9999

classtypes <- lapply(xtrain,class)
deselect <- which( classtypes == "factor" | classtypes == "character" )
xtrain_num <- xtrain[, -deselect, with=F] # special syntax because it's a data table not data frame

# fit model
# xtrain[,-fact_cols]
mod0 <- gbm(target ~ ., data = xtrain_num, distribution="bernoulli",
                n.trees = 5, 
                shrinkage = 0.01, interaction.depth = 10, 
            # cv.folds=5
            # n.cores=2
            verbose= T)

# check performance using 5-fold cross-validation
#best.iter <- gbm.perf(mod0,method="cv")
#print(best.iter)
# plot the performance # plot variable influence
#summary(mod0,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity
# print(pretty.gbm.tree(mod0,1))
# print(pretty.gbm.tree(mod0,mod0$n.trees))

# generate prediction
#  xtest[,-fact_cols]
xtest_num <- xtest[, -deselect, with=F]
pr <- predict(mod0, xtest_num, mod0$n.trees, type="response")

subm <- data.frame(xtest$ID, pr)
colnames(subm) <- c('ID','target')
write.csv(subm, "./submission.csv", row.names=FALSE)
