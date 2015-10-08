# Settings for model tuning
settings <- list()

# TODO: use xgb.cv to cross validate settings properly

# see https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
# below settings from c999f6e... LB 0.79820 - see if that's consistent 
settings["nrounds"] <- 8000 
settings["eta"] <- sample(seq(0.0065, 0.0090, by=0.0005),1)
settings["min_child_weight"] <- 6 # sample(c(25,100,seq(6,10)),1) # 6
settings["max_depth"] <- sample(c(9,11),1)
settings["alpha"] <- round(runif(1, 4, 6)) # 4
settings["lambda"] <- round(runif(1, 5, 10)) # 5
settings["sb_threshold"] <- 0.0 # sample(seq(0.0001,0.01,by=0.0001),1) # currently not used
settings["corr_threshold"] <- 0.0 # 0..1; 0 turns it off
settings["valpct"] <- 5 # sample(c(10.3283734188981, 10, 5), 1)
settings["corr_pct"] <- 50 # % used to test correlation
settings["subsample"] <- sample(seq(0.8,1.0,by=0.1),1)
settings["colsample_bytree"] <- sample(seq(0.8,1.0,by=0.1),1)
settings["addGeoFields"] <- T #sample(c(T,F),1)
settings["addJobFields"] <- F
settings["random_seed"] <- round(runif(1, 1, 10000)) # 1948
settings["early.stop.round"] <- 1000
settings["print.every.n"] <- 100 
settings["useSmallSample"] <- F

# This guy is using gamma and max_delta_step
# https://www.kaggle.com/michaelpawlus/springleaf-marketing-response/xgb-3/run/65321/code
# perhaps colsample_bytree, subsample and valpercentage should also be parameters
# 10% is not much

str <- ""
for (i in 1:length(settings)) {
  str <- paste(str, ifelse(i == 1, "", ", "), names(settings)[i], ":", settings[[i]], sep="")
}
print(str)
