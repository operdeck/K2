# Settings for model tuning

settings <- list()

# see https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
# below settings from c999f6e... LB 0.79820 - see if that's consistent 
settings["eta"] <- runif(1, 0.001, 0.009) # 0.0075 # round(runif(1, 0.003, 0.04),4)
settings["nrounds"] <- 4000
settings["print.every.n"] <- 100
settings["useSmallSample"] <- F
settings["doScoring"] <- T
settings["min_child_weight"] <- 6 # trunc(runif(1, 6, 20))
settings["max_depth"] <- 9 # trunc(runif(1, 6, 10))
settings["alpha"] <- 4 # trunc(runif(1, 0, 10))
settings["lambda"] <- 5 # trunc(runif(1, 1, 10))

str <- ""
for (i in 1:length(settings)) {
  str <- paste(str, ifelse(i == 1, "", ", "), names(settings)[i], ":", settings[[i]], sep="")
}
print(str)

