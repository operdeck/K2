# Settings for model tuning

settings <- list()

# see https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
settings["eta"] <- sample( seq(0.003,0.02,by=0.001), 1)
settings["nrounds"] <- 4000
settings["print.every.n"] <- 100
settings["useSmallSample"] <- F
settings["doScoring"] <- T
settings["min_child_weight"] <- sample( seq(6,20,by=1), 1)
settings["max_depth"] <- sample( seq(6,10,by=1), 1)
settings["alpha"] <- sample( c(0,1,2,4,6,10), 1)
settings["lambda"] <- sample( c(1,2,4,6,10), 1)
settings["gamma"] <- sample( c(0,1,2,4,8,16,32,64,128), 1)

str <- ""
for (i in 1:length(settings)) {
  str <- paste(str, ifelse(i == 1, "", ", "), names(settings)[i], ":", settings[[i]], sep="")
}
print(str)

