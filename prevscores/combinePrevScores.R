library(data.table)

prev_gbm_test <- fread("gbm/submission.csv")
prev_gbm_train <- fread("gbm/scores_train.csv")

prev_xgb_linear_test <- fread("xgb_linear/subm.csv")
prev_xgb_linear_train <- fread("xgb_linear/train.csv")

prev_test <- data.frame(
#   prev_gbm=prev_gbm_test$target
                        prev_xgb=prev_xgb_linear_test$target
                        )

prev_train <- data.frame(
#   prev_gbm=prev_gbm_train$Prediction
                         prev_xgb=prev_xgb_linear_train$prevScore
                         )
print(dim(prev_test)) # should have 14532 rows
print(dim(prev_train)) # should have 14531 rows

write.csv(prev_test, "prev_test.csv", row.names=F)
write.csv(prev_train, "prev_train.csv", row.names=F)
