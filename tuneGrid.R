# Driver to find model tuning parameters

if (!file.exists("submissions")){
  dir.create(file.path(".", "submissions"))
}

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
  newResults$R <- R.Version()[['version.string']]
  newResults$OS <- R.Version()[['platform']]
  newResults <- newResults[-1]
  
  if(file.exists("submissions/tuneResults.csv")) {
    # Try use fread instead - things are now all strings, it seems. write w/o quotes.
    print("Tune results dim before and after unique")
    tuneResults <- read.csv2("submissions/tuneResults.csv", stringsAsFactors=F)
    print(dim(tuneResults))
    tuneResults <- unique(tuneResults)
    print(dim(tuneResults))
    
    # merge in LB score from files
    isValidSplit <- function(x) { return (length(x) == 8 && x[4] == "LB") }
    scoresLB <- sapply(strsplit(list.files("submissions","subm_.*"), "_|[.]"), 
                       function(x) { return (ifelse(isValidSplit(x), as.double(paste("0.",x[5],sep="")), NA))})
    scoresVal <- sapply(strsplit(list.files("submissions","subm_.*"), "_|[.]"), 
                        function(x) { return (ifelse(isValidSplit(x), as.double(paste("0.",x[3],sep="")), NA))})
    submKey <- sapply(strsplit(list.files("submissions","subm_.*"), "_|[.]"), 
                      function(x) { return (ifelse(isValidSplit(x), paste(x[2],x[3],x[6],x[7],sep="_"), NA))})
    lbDS <- data.frame(submKey, scoresVal, scoresLB, stringsAsFactors=F)
    names(lbDS) <- c('fileKey','Val', 'LB')
    lbDS <- filter(lbDS, !is.na(LB)) %>% arrange(desc(LB))

    if (class(tuneResults$when) == "character") {
      tuneResults$fileKey <- gsub("\\.|\\ ","_",
                                  paste(format(tuneResults$bestScore,digits=6, nsmall=6), 
                                        gsub("\\:|-","",tuneResults$when), sep="_"))
    } else {
      stop("Unexpected type for when field - is it a date?")

      tuneResults$fileKey <- gsub("\\.","_",
                                  paste(format(tuneResults$bestScore,digits=6, nsmall=6),
                                        format(tuneResults$when, format="%Y%m%d_%H%M%S"), sep="_"))
    }    
    if ("LB" %in% names(tuneResults)) {
      joinedWithLB <- left_join(select(tuneResults, -LB), lbDS, by="fileKey")
    } else {
      joinedWithLB <- left_join(tuneResults, lbDS, by="fileKey")
    }
    tuneResults$LB <- joinedWithLB$LB #ifelse(is.na(joinedWithLB$LB), 0.0, joinedWithLB$LB)

    # trickery to add new empty row to tuneResults
    temprow <- matrix(c(rep.int(NA,length(tuneResults))),nrow=1,ncol=length(tuneResults))
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(tuneResults)
    tuneResults <- rbind(tuneResults,newrow)
    
    # merge in the new results
    for (i in 1:length(newResults)) {
      if (length(tuneResults[[names(newResults)[i]]]) > 0) {
        if (class(newResults[[i]]) == "logical") { # fix types - which got screwed historically
          tuneResults[[names(newResults)[i]]] = as.logical(gsub("^\\s+|\\s+$", "", tuneResults[[names(newResults)[i]]]))
        } else if (class(newResults[[i]]) == "integer") {
          tuneResults[[names(newResults)[i]]] = as.integer(tuneResults[[names(newResults)[i]]])
        } else if (class(newResults[[i]]) == "numeric") {
          tuneResults[[names(newResults)[i]]] = as.numeric(tuneResults[[names(newResults)[i]]])
        } else {
          tuneResults[[names(newResults)[i]]] = as.character(tuneResults[[names(newResults)[i]]])
        }
      }
      tuneResults[[names(newResults)[i]]] [nrow(tuneResults)] <- newResults[1,i]
    }
    
    tuneResults <- arrange(tuneResults, desc(bestScore))
  } else {
    tuneResults <- newResults
  }
    
  # merge in LB score from files
  # for (fName in list.files("submissions","subm_.*")) { print (fName) } and strsplit
  
  cat("New results (round", r, "):", fill=T)
  print(newResults)
  print("Best results:")
  print(tuneResults[1,])
  
  # only keep > .79 and the current best (just in case there's no great scores yet)
  tuneResults <- tuneResults[unique(c(1,which(tuneResults$bestScore > 0.79))),]

  # Plot LB vs bestScore
  if ("LB" %in% names(tuneResults)) {
    print(ggplot(filter(tuneResults, !is.na(LB), !useSmallSample), 
                 aes(x=bestScore, y=LB, colour=max_depth, size=bestRound)) + 
            geom_point() + geom_smooth(method="lm",se=T,fullrange=T) +
            scale_colour_gradient(low="red",high="lightgreen")+
            geom_abline(intercept = 0, slope = 1, colour="darkgrey", size=1, linetype="dashed"))  
  }
  
  print("************Written tune results:")
  print(dim(tuneResults))
  
  write.csv2(tuneResults, "submissions/tuneResults.csv", row.names=F) # 'internal' format
  #format(tuneResults,digits=6, nsmall=6)
  
  tryCatch({
    write.table(format(tuneResults,digits=6, nsmall=6), "submissions/tuneResults_copy_win.csv", quote=F, sep=",", dec=".", row.names=F)
  }, warning = function(e) { print("Cannot write copy of tune results. File open? Skipping.")})
  tryCatch({
    write.table(format(tuneResults,digits=6, nsmall=6), "submissions/tuneResults_copy_mac.csv", quote=F, sep=";", dec=",", row.names=F)
  }, warning = function(e) { print("Cannot write copy of tune results. File open? Skipping.")})
  
  if (exists("settings")) {
    rm("settings")
  }
}

