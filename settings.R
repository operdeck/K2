# Settings for model tuning
settings <- list()

# see https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
settings["nrounds"] <- 8000 
settings["eta"] <- sample(seq(0.0060, 0.0090, by=0.0005),1)
settings["min_child_weight"] <- 6 # sample(c(25,100,seq(6,10)),1)
settings["max_depth"] <- 10 # sample(seq(9,11),1)
settings["alpha"] <- round(runif(1, 3, 6)) # 4
settings["lambda"] <- round(runif(1, 4, 10)) # 5
settings["sb_threshold"] <- 0.0 # sample(seq(0.0001,0.01,by=0.0001),1) # currently not used
settings["corr_threshold"] <- 0 # 0..1; 0 turns it off
settings["corr_pct"] <- 50 # % used to test correlation (only used if corr_threshold > 0)
settings["cv.nfold"] <- 0 # N-fold cross validation. If > 0, val pct is not used
settings["valpct"] <- 5 # sample(c(10.3283734188981, 10, 5), 1), if 0, no validation set in train
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

str <- ""
for (i in 1:length(settings)) {
  str <- paste(str, ifelse(i == 1, "", ", "), names(settings)[i], ":", settings[[i]], sep="")
}
print(str)
