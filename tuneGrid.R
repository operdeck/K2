# Driver to find model tuning parameters

nRows <- 100
tuneGrid <- data.frame(vector(,nRows))

# see https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
tuneGrid$eta <- sample( seq(0.001,0.01,by=0.001), nRows, replace=T )
tuneGrid$nrounds <- 4000
tuneGrid$print.every.n <- 100
tuneGrid$useSmallSample <- T
tuneGrid$doScoring <- F
tuneGrid$min_child_weight <- sample( seq(1,20,by=2), nRows, replace=T )
tuneGrid$max_depth <- sample( seq(6,10,by=1), nRows, replace=T )
tuneGrid$alpha <- sample( c(0,1,2,4,6,10,50,100), nRows, replace=T )
tuneGrid$lambda <- sample( c(1,2,4,6,10,50,100), nRows, replace=T )
tuneGrid$gamma <- sample( c(0,1,2,4,8,16,32,64,128), nRows, replace=T )

tuneGrid <- tuneGrid[-1]
print(tuneGrid)

for (r in 1:nrow(tuneGrid)) {
  settings <- list()
  for (colName in names(tuneGrid)) {
    settings[colName] <- tuneGrid[[colName]][r] 
  }
  cat("Tuning parameters (round", r, "):", fill=T)
  print(tuneGrid[r,])
  
  # Model building here
  source("springleaf.R")
  
  # Process the results
  newResults <- data.frame(vector(,1))
  for (i in 1:length(results)) {
    if (!("list" %in% class(results[[i]]))) {
      newResults[[names(results)[i]]] <- results[[i]]
    }
  }
  # Add the tuning parameters
  for (param in names(tuneGrid)) {
    newResults[[param]] <- tuneGrid[[param]][r]
  }
  newResults <- newResults[-1]
  
  if(file.exists("./tuneResults.csv")) {
    tuneResults <- read.csv2("./tuneResults.csv", stringsAsFactors=F)

    # trickery to add new empty row to tuneResults
    temprow <- matrix(c(rep.int(NA,length(tuneResults))),nrow=1,ncol=length(tuneResults))
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(tuneResults)
    tuneResults <- rbind(tuneResults,newrow)
    
    # merge in the new results
    for (i in 1:length(newResults)) {
      tuneResults[[names(newResults)[i]]] [nrow(tuneResults)] <- newResults[1,i]
    }
    
    tuneResults <- arrange(tuneResults, desc(bestScore))
  } else {
    tuneResults <- newResults
  }
  cat("New results (round", r, "):", fill=T)
  print(newResults)
  print("Best results:")
  print(tuneResults[1,])
  write.csv2(tuneResults, "./tuneResults.csv", row.names=F)
  
  if (exists("settings")) {
    rm("settings")
  }
}
