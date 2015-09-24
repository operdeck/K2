# Driver to find model tuning parameters

for (r in 1:100) {
  cat("Tuning parameters (round", r, "):", fill=T)
  source("./settings.R")
  
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
  for (param in names(results[["settings"]])) {
    newResults[[param]] <- results[["settings"]][[param]]
  }
  newResults <- newResults[-1]
  
  if(file.exists("submissions/tuneResults.csv")) {
    tuneResults <- read.csv2("submissions/tuneResults.csv", stringsAsFactors=F)

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
  
  # only keep > .78 and the current best (just in case there's no great scores yet)
  tuneResults <- tuneResults[unique(c(1,which(tuneResults$bestScore > 0.78))),]
  
  write.csv2(format(tuneResults,digits=6), "submissions/tuneResults.csv", row.names=F) # 'internal' format
  
  tryCatch({
    write.table(format(tuneResults,digits=6), "submissions/tuneResults_copy_win.csv", quote=F, sep=",", dec=".", row.names=F)
  }, warning = function(e) { print("Cannot write copy of tune results. File open? Skipping.")})
  tryCatch({
    write.table(format(tuneResults,digits=6), "submissions/tuneResults_copy_mac.csv", quote=F, sep=";", dec=",", row.names=F)
  }, warning = function(e) { print("Cannot write copy of tune results. File open? Skipping.")})
  
  if (exists("settings")) {
    rm("settings")
  }
}
