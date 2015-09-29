# Settings for model tuning
settings <- list()

# see https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
# below settings from c999f6e... LB 0.79820 - see if that's consistent 
settings["nrounds"] <- 4000 
settings["print.every.n"] <- 100 
settings["useSmallSample"] <- F
settings["doScoring"] <- T 
settings["eta"] <- sample(seq(0.015,0.060,by=0.001),1) # 0.0075
settings["min_child_weight"] <- round(runif(1, 6, 10)) 
settings["max_depth"] <- 9 # round(runif(1, 8, 10)) 
settings["alpha"] <- round(runif(1, 4, 10)) 
settings["lambda"] <- round(runif(1, 1, 10))
settings["random_seed"] <- round(runif(1, 1, 10000))
settings["sb_threshold"] <- sample(seq(0.0001,0.01,by=0.0001),1)
settings["valpct"] <- 20
settings["subsample"] <- 0.7
settings["colsample_bytree"] <- 0.5

# This guy is using gamma and max_delta_step
# https://www.kaggle.com/michaelpawlus/springleaf-marketing-response/xgb-3/run/65321/code
# perhaps colsample_bytree, subsample and valpercentage should also be parameters
# 10% is not much

str <- ""
for (i in 1:length(settings)) {
  str <- paste(str, ifelse(i == 1, "", ", "), names(settings)[i], ":", settings[[i]], sep="")
}
print(str)
