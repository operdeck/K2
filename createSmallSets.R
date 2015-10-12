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
               nrows=30000,
               stringsAsFactors=F,integer64="double",data.table=F)
test <- fread("./data/test-2.csv", header = T, sep = ",",
              nrows=30000,
              stringsAsFactors=F,integer64="double",data.table=F)

keepVars <- c('VAR_0073','VAR_0075','VAR_0204','VAR_0176','VAR_1329', # these are special
              'VAR_0795','VAR_0855', 
              'VAR_0968','VAR_0212','VAR_0884','VAR_0806','VAR_0004', # for some reason
              'VAR_0540','VAR_0881','VAR_0720','VAR_0003','VAR_0014', # (geo, dates, strings)
              'VAR_0200','VAR_0274','VAR_0342','VAR_0404','VAR_0493',
              'VAR_0241','VAR_0242','VAR_0237','VAR_0342',
              "VAR_0180","VAR_0181","VAR_0182","VAR_0383","VAR_0384", # for the bool counts
              "VAR_0502","VAR_0503","VAR_0504","VAR_0505","VAR_0566","VAR_0567",
              "VAR_0740","VAR_0741","VAR_1162","VAR_1163","VAR_1164","VAR_1165",
              'ID', 'target',
              # below the top N from a big run
              "VAR_0070","VAR_0795","VAR_0074","VAR_0071","VAR_1329","VAR_0004","VAR_0241",
              "VAR_0970","VAR_1127","VAR_0080","VAR_0137","VAR_0886","VAR_0881","VAR_0003",
              "VAR_0540","VAR_0968","VAR_0087","VAR_1791","VAR_0227","VAR_0853","VAR_0505",
              "VAR_0707","VAR_1747","VAR_0855","VAR_0200","VAR_0212","VAR_0810","VAR_0002",
              "VAR_0720","VAR_1748","VAR_0613","VAR_1495","VAR_0880","VAR_1136","VAR_0544",
              "VAR_0272","VAR_0884","VAR_0885","VAR_0078","VAR_0273","VAR_0105","VAR_0708",
              "VAR_0242","VAR_1751","VAR_1934","VAR_1132","VAR_0503","VAR_1128","VAR_0611",
              "VAR_0555","VAR_0279","VAR_1130","VAR_0806","VAR_0237","VAR_0005","VAR_0871",
              "VAR_0854","VAR_0868","VAR_0302","VAR_0579","VAR_0322","VAR_0006","VAR_0712",
              "VAR_0711","VAR_1823","VAR_0324","VAR_0909","VAR_1530","VAR_1390","VAR_1398",
              "VAR_1110","VAR_0228","VAR_1151","VAR_0274","VAR_0282","VAR_0341","VAR_0861",
              "VAR_1750","VAR_1146","VAR_0837","VAR_0807","VAR_1150","VAR_1201","VAR_0361",
              "VAR_0863","VAR_0860","VAR_0304","VAR_1531","VAR_1512","VAR_0550","VAR_0896",
              "VAR_1358","VAR_0198","VAR_0859","VAR_0905","VAR_0293","VAR_0079","VAR_0921")






train <- train[(names(train) %in% keepVars)]
test  <- test[(names(test) %in% keepVars)]

write.table(train, "data/train_small.csv", sep=",", row.names=F)
write.table(test, "data/test_small.csv", sep=",", row.names=F)


