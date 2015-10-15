library(data.table)
library(dplyr)
library(stringdist)
library(pROC)
train <- fread( "data/train-2.csv",header = T, sep = ",",
                stringsAsFactors=F,integer64="double",data.table=F )

isBooleanField <- sapply(names(train), function(v) 
  { return((class(train[[v]]) == "integer") & 
             (length(unique(na.omit(train[[v]]))) == 2) &
             all(unique(na.omit(train[[v]])) %in% c(0,1))) })


prev_u <- -1
prev_sequence <- 0
dsSeq <- data.frame( fieldNo=which(isBooleanField), 
                     fieldName=names(train)[which(isBooleanField)],
                     sequenceNo=0)
for (i in 1:nrow(dsSeq)) {
  if (dsSeq$fieldNo[i] == (prev_u + 1)) {
    sequence <- prev_sequence
  } else {
    sequence <- prev_sequence + 1
  }
  dsSeq$sequenceNo[i] <- sequence
  
  b <- train[[dsSeq$fieldNo[i]]] == 1
  
  prev_u <- dsSeq$fieldNo[i]
  prev_sequence <- sequence
}
dsSeq <- left_join(dsSeq, group_by(dsSeq, sequenceNo) %>% summarise(seqSize = n())) %>% 
  filter(seqSize > 1)
dsSeq$sequenceNo <- as.integer(as.factor(dsSeq$sequenceNo)) # recode to 1 .. N
print(dsSeq)


sumup <- function(ds, vars) {
  pow <- 1
  s <- rep(0, nrow(ds))
  for (var in vars) {
    s <- s + ds[[var]]^pow
    pow <- pow*2
  }
  return(s)
}

# Create derived fields
for (i in 1:max(dsSeq$sequenceNo)) {
  print(paste("Boolean sequence",i,":",paste(dsSeq$fieldName[dsSeq$sequenceNo == i], collapse=","),sep=" "))  
  
  train[[paste("concatBool", i, paste(dsSeq$fieldName[dsSeq$sequenceNo == i], collapse="_"), sep="_")]] <-
    sumup(train, dsSeq$fieldName[dsSeq$sequenceNo == i])
}

print(summary( select(train, starts_with("concatBool_")) ))
print(data.frame(DevAUC=sapply( c(as.character(dsSeq$fieldName),
                                  names(select(train, starts_with("concatBool_")))), 
                          function(v) { 
                            return (0.5 + abs(0.5-auc(train$target, train[[v]])))})))

write.table( train[,names(train) %in% c("target",as.character(dsSeq$fieldName))] ,
             "boolsOnly.csv",
             row.names=F, sep=",", na="")

