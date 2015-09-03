# Create small set for quick testing

library(data.table)
require(bit64)

macWD <- "~/Documents/science/kaggle/springleaf/K2"
winWD <- "D:/usr/science/kaggle/springleaf/K2"
if (file.exists(macWD)) {
  setwd(macWD)
} else {
  setwd(winWD)
}

train <- fread("./data/train-2.csv", header = T, sep = ",",
               nrows=20000,
               stringsAsFactors=F,integer64="double",data.table=F)
test <- fread("./data/test-2.csv", header = T, sep = ",",
              nrows=20000,
              stringsAsFactors=F,integer64="double",data.table=F)

keepVars <- c('VAR_0073','VAR_0176','VAR_1329','VAR_0795','VAR_0855',
              'VAR_0968','VAR_0212','VAR_0884','VAR_0806','VAR_0004',
              'VAR_0540','VAR_0881','VAR_0720','VAR_0003','VAR_0014',
              'VAR_0200','VAR_0274','VAR_0342','VAR_0404','VAR_0493',
              'ID', 'target',
              paste('VAR_',seq(1000,1100,by=1),sep=""))
train <- train[(names(train) %in% keepVars)]
test  <- test[(names(test) %in% keepVars)]

write.table(train, "data/train_small.csv", sep=",", row.names=F)
write.table(test, "data/test_small.csv", sep=",", row.names=F)


