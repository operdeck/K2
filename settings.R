# Settings for model tuning

settings <- list()

# see https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
# below settings from c999f6e... LB 0.79820 - see if that's consistent 
settings["eta"] <- sample(seq(0.001,0.02,by=0.001),1) # 0.0075
settings["nrounds"] <- 4000 
settings["print.every.n"] <- 100 
settings["useSmallSample"] <- F 
settings["doScoring"] <- T 
settings["min_child_weight"] <- round(runif(1, 6, 20)) 
settings["max_depth"] <- round(runif(1, 8, 10)) 
settings["alpha"] <- round(runif(1, 0, 10)) 
settings["lambda"] <- round(runif(1, 1, 10))
settings["random_seed"] <- round(runif(1, 1, 10000))
settings["sb_threshold"] <- sample(seq(0.001,0.01,by=0.001),1)

str <- ""
for (i in 1:length(settings)) {
  str <- paste(str, ifelse(i == 1, "", ", "), names(settings)[i], ":", settings[[i]], sep="")
}
print(str)
