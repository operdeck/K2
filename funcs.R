library(scales)
library(sm)

# Creates a binning object from a vector of values and outcomes, grouping
# the values with a frequency above the threshold in distinct bins, the
# rest in a residual bin.
#
# Result is a dataframe with
#  binindex = bin index (1:N)
#  val = value
#  cases = number of cases
#  avgoutcome = average behaviour
#  freq = number of cases as percentage
# Sorted by increasing avgoutcome
createSymBin2 <- function(val, outcome, threshold = 0.001) 
{
  if (!is.logical(outcome) && !is.integer(outcome)) {
    stop("expects a logical or integer as 2nd argument")
  }
  df <- data.frame(val, outcome)
  setnames(df, c('val','outcome'))
  total <- length(outcome)
  # summarize frequency by value with average outcome
  g <- group_by(df, val) %>% 
    dplyr::summarise(cases = n(), avgoutcome = mean(outcome), freq = cases/total, t = freq>=threshold) %>% 
    arrange(avgoutcome)
  # NA/Missing can occur in the dataset - consider those like residuals
  if (any(is.na(g$val))) {
    g$t[is.na(g$val)]=FALSE
  }
  g$val <- as.character(g$val)
  # select only values that have frequency above threshold
  result <- filter(g, t)
  # calculate outcome for all other values, or if there are none, the overall average
  nRemainingCases <- total - sum(result$cases)
  if (nRemainingCases > 0) {
    residualOutcome <- # TODO not always correct! Avg for residual or overal, when??
      sum( (filter(g, !t) %>% mutate( sumavgoutcome = avgoutcome*cases ))$sumavgoutcome ) / nRemainingCases
  } else {
    residualOutcome <- mean(outcome, na.rm=TRUE)
  }
  # bind a 'residual' row for values with frequency below threshold
  result <- rbind(result, c(NA,nRemainingCases,residualOutcome,nRemainingCases/total))
  
  if (nrow(result) > 1) {
    result$binindex <- rank(result$avgoutcome) #seq(1:nrow(result))
    return(select( result, -t))
  } else {
    setnames(result, c('val','cases','avgoutcome','freq'))
    result$binindex <- c(1)
    return(result)
  }
}

# Apply sym binning to a vector of values, returning a vector of bin indices
# Values not in the binning will get the 'residual' bin index
applySymBinIndex <- function(binning, values) 
{
  if (nrow(binning) == 1) {
    return (rep(binning$binindex[nrow(binning)], length(values)))  
  } else {
    df <- data.frame(as.character(values), stringsAsFactors = F)
    setnames(df, c('val'))
    r <- left_join(df, binning[seq(1:(nrow(binning)-1)),], by="val")
    #print(r)
    return ( ifelse(is.na(r$binindex), binning$binindex[nrow(binning)], r$binindex) )
  }
}

# Apply sym binning to a vector of values, returning a vector of recoded values
# Values not in the binning will get the 'residual' recoding
applySymBin <- function(binning, values) 
{
  if (nrow(binning) == 1) {
    return (rep(binning$avgoutcome[nrow(binning)], length(values)))  
  } else {
    df <- data.frame(as.character(values), stringsAsFactors = F)
    setnames(df, c('val'))
    r <- left_join(df, binning[seq(1:(nrow(binning)-1)),], by="val")
    #print(r)
    return ( ifelse(is.na(r$avgoutcome), binning$avgoutcome[nrow(binning)], r$avgoutcome) )
  }
}

# Plot symbolic data analysis for one set of vectors
sb.plotOne <- function(binning, 
                       ds_dev, ds_val, ds_tst,
                       fieldName, outcomeName,
                       plotFolder = NULL)
{
  ds_dev_bins <- applySymBinIndex(binning, ds_dev[[fieldName]])
  df_dev <- data.frame( ds_dev_bins, ds_dev[,outcomeName])
  names(df_dev) <- c('binindex','beh')
  
  ds_val_bins <- applySymBinIndex(binning, ds_val[[fieldName]])
  df_val <- data.frame( ds_val_bins, ds_val[,outcomeName])
  names(df_val) <- c('binindex','beh')
  
  ds_tst_bins <- applySymBinIndex(binning, ds_tst[[fieldName]])
  df_tst <- data.frame( ds_tst_bins )
  names(df_tst) <- c('binindex')
  
  rs_dev <- group_by(df_dev, binindex) %>% dplyr::summarise( dev_f=n()/nrow(df_dev), dev_beh=mean(beh,na.rm=T) )
  rs_val <- group_by(df_val, binindex) %>% dplyr::summarise( val_f=n()/nrow(df_val), val_beh=mean(beh,na.rm=T) )
  rs_tst <- group_by(df_tst, binindex) %>% dplyr::summarise( tst_f=n()/nrow(df_tst) )
  
  df_summarized <- left_join(left_join(left_join(select(binning, binindex, val), 
                                                 rs_dev, by="binindex"),
                                       rs_val, by="binindex"),
                             rs_tst, by="binindex")
  
  cat("Summary symbin",fieldName,fill=T)
  print(df_summarized)
  
  df_summarized <- filter(df_summarized, binindex < 100) ## just for viewing
  
  # Barchart with frequencies
  df_plot1 <- gather(df_summarized, dataset, frequency, dev_f, val_f, tst_f)
  df_plot1$val <- reorder(as.character(factor(df_plot1$val)),df_plot1$binindex)
  try({
    plot1 <- ggplot(df_plot1, 
                    aes(x=val, y=frequency, fill=dataset))+
      geom_bar(stat="identity",position="dodge")+
      xlab(paste(fieldName,"(symbin)"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(labels=percent)
    print(plot1)
    if (!is.null(plotFolder)) {
      ggsave(paste(plotFolder,'/plot_',fieldName,'_bin_freq.png',sep=""))
    }
  })
  
  # Linegraph with average outcomes
  df_plot2 <- gather(df_summarized, dataset, outcome, dev_beh, val_beh)
  df_plot2$val <- reorder(as.character(factor(df_plot2$val)),df_plot2$binindex)
  try({
    plot2 <- ggplot(df_plot2, 
                    aes(x=val, y=outcome, colour=dataset, group=dataset))+
      geom_line()+geom_point()+
      xlab(paste(fieldName,"(symbin)"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(plot2)
    if (!is.null(plotFolder)) {
      ggsave(paste(plotFolder,'/plot_',fieldName,'_bin_beh.png',sep=""))
    }
  })
  
  return (df_summarized)
}


createNumBin <- function(vec_dev, vec_val, vec_tst, beh_dev, beh_val, n)
{
  vec_all <- c(vec_dev, vec_val, vec_tst)
  ds_min <- min(vec_all, Inf, na.rm=T)
  ds_max <- max(vec_all, -Inf, na.rm=T)
  if (length(unique(vec_all)) < n) {
    intervals <- c(ds_min, sort(unique(vec_all)))
  } else {
    intervals <- unique(c(ds_min, quantile(vec_all, 
                                           (1:n)/n, na.rm=T, names=F), ds_max))
  }
  if (length(intervals) == 1) {
    intervals <- rep(intervals[1],2) # make sure there are two rows
  }
  binning <- data.frame(intervals,"NA",0,0,0,stringsAsFactors=F)
  names(binning) <- c('boundary','interval','dev_n','val_n','tst_n')
  for (i in 1:(length(intervals)-1)) {
    inclLower <- (i == 1)
    if (inclLower) {
      binning$interval[i] <- paste("[",sprintf("%.2f",binning$boundary[i]),",",sprintf("%.2f",binning$boundary[i+1]),"]",sep="")
      binning$dev_n[i] <- 
        sum(vec_dev >= binning$boundary[i] & vec_dev <= binning$boundary[i+1],na.rm=T)
      binning$val_n[i] <- 
        sum(vec_val >= binning$boundary[i] & vec_val <= binning$boundary[i+1],na.rm=T)
      binning$tst_n[i] <- 
        sum(vec_tst >= binning$boundary[i] & vec_tst <= binning$boundary[i+1],na.rm=T)
      binning$dev_beh[i] <- 
        mean(beh_dev[vec_dev >= binning$boundary[i] & vec_dev <= binning$boundary[i+1]],na.rm=T)
      binning$val_beh[i] <- 
        mean(beh_val[vec_val >= binning$boundary[i] & vec_val <= binning$boundary[i+1]],na.rm=T)
    } else {    
      binning$interval[i] <- paste("(",sprintf("%.2f",binning$boundary[i]),",",sprintf("%.2f",binning$boundary[i+1]),"]",sep="")
      binning$dev_n[i] <- 
        sum(vec_dev > binning$boundary[i] & vec_dev <= binning$boundary[i+1],na.rm=T)
      binning$val_n[i] <- 
        sum(vec_val > binning$boundary[i] & vec_val <= binning$boundary[i+1],na.rm=T)
      binning$tst_n[i] <- 
        sum(vec_tst > binning$boundary[i] & vec_tst <= binning$boundary[i+1],na.rm=T)
      binning$dev_beh[i] <- 
        mean(beh_dev[vec_dev > binning$boundary[i] & vec_dev <= binning$boundary[i+1]],na.rm=T)
      binning$val_beh[i] <- 
        mean(beh_val[vec_val > binning$boundary[i] & vec_val <= binning$boundary[i+1]],na.rm=T)
    }
  }
  binning$dev_n[nrow(binning)] <- sum(is.na(vec_dev))
  binning$val_n[nrow(binning)] <- sum(is.na(vec_val))
  binning$tst_n[nrow(binning)] <- sum(is.na(vec_tst))
  binning$dev_f <- binning$dev_n/sum(binning$dev_n)
  binning$val_f <- binning$val_n/sum(binning$val_n)
  binning$tst_f <- binning$tst_n/sum(binning$tst_n)
  return(binning)
}

#b <- numbinner(train_dev$VAR_0288,train_val$VAR_0288,test$VAR_0288,train_dev$target,train_val$target,2)

applyNumBin <- function(b, vec) {
  result <- rep(nrow(b),length(vec))
  for (i in 1:(nrow(b)-1)) {
    if (i == 1) {
      result[vec >= b$boundary[i] & vec <= b$boundary[i+1]] <- i 
    } else {
      if (i == (nrow(b)-1)) {
        result[vec > b$boundary[i]] <- i 
      } else {
        result[vec > b$boundary[i] & vec <= b$boundary[i+1]] <- i 
      }
    }
  }
  result <- b$dev_beh[result]
  return(result)
}

plotNumBin <- function(binz, plotFolder=NULL) 
{
  print(binz)
  # Barchart with frequencies
  binz$interval <- factor(binz$interval, levels=binz$interval)
  df_plot1 <- gather(binz, dataset, frequency, dev_f, val_f, tst_f)
  try({
    plot1 <- ggplot(df_plot1, 
                    aes(x=interval, y=frequency, fill=dataset))+
      geom_bar(stat="identity",position="dodge")+
      xlab(fieldName)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      scale_y_continuous(labels=percent)
    print(plot1)
    if (!is.null(plotFolder)) {
      ggsave(paste(trainDataFolder,'/plot_',fieldName,'_nb_freq.png',sep=""))
    }
  })
  
  # Linegraph with average outcomes
  df_plot2 <- gather(binz, dataset, outcome, dev_beh, val_beh)
  try({
    plot2 <- ggplot(df_plot2, 
                    aes(x=interval, y=outcome, colour=dataset, group=dataset))+
      geom_line()+geom_point()+
      xlab(fieldName)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(plot2)
    if (!is.null(plotFolder)) {
      ggsave(paste(plotFolder,'/plot_',fieldName,'_nb_beh.png',sep=""))
    }
  })
}

# replace NAs in data set with mean - should do knnImpute
imputeNAs <- function(ds, verbose=F) {
  ds <- as.data.frame(ds)
  cat("Imputing",sum(!complete.cases(ds)),"rows from total of",nrow(ds), fill=TRUE)
  for (colNo in which(sapply(ds, is.numeric))) {
    aCol <- select(ds, colNo)
    if (any(is.na(aCol))) {
      m <- colMeans(aCol,  na.rm = TRUE)
      if (verbose) {
        cat("   imputing", colnames(ds) [colNo], sum(!complete.cases(aCol)),"NAs with mean", m, fill=TRUE)
      }
      ds[!complete.cases(aCol), colNo] <- m
    }
  }
  return(ds)
}

# Kaggle's LogLoss evaluation function
logLoss <- function(act, pred)
{
  eps <- 1e-15
  if (!is.logical(act)) {
    stop("Logloss expects a logical as first argument")
  }
  ll <- -1*sum(act*log(pmax(eps,pred)) + (1-act)*log(pmax(eps,1-pred)))/length(pred)
  return(ll)
}

# check if column values are all dates
isDate <- function(vec) { 
  all( grepl( "^\\d{2}[A-Z]{3}\\d{2}", vec[nzchar(vec)]) ) # check date fmt "12OCT13" (or empty)
}

# check if column values are all booleans
isBoolean <- function(vec) { 
  all( grepl( "^true$|^false$", vec[nzchar(vec)]) )
}

# DA for one vector - assuming the dataframes all contain only one field
dataAnalysisOne <- function(dfDev, dfVal, dfTest, fldName, 
                            generatePlots=F, plotFolder=NULL)
{
  dsFull <- rbind(dfDev, dfVal)
  
  dataMetrics <- nearZeroVar(dsFull, saveMetrics=TRUE)
  dataMetrics$className <- lapply(dsFull,class)
  dataMetrics$isSymbolic <- dataMetrics$className %in% c("factor", "character")
  dataMetrics$isNumeric <- dataMetrics$className %in% c("integer", "numeric", "logical")
  dataMetrics$nDistinct <- dataMetrics$percentUnique * nrow(dsFull) / 100
  dataMetrics$nNA <- sapply(dsFull, function(vec) { return (sum(is.na(vec))) })
  symbolicFldNames <- rownames(dataMetrics) [dataMetrics$isSymbolic]
  if (length(symbolicFldNames) > 0) {
    dataMetrics$isDate <- rownames(dataMetrics) %in% symbolicFldNames[ sapply(symbolicFldNames, function(colName) { isDate(dsFull[[colName]]) } ) ]
    dataMetrics$isBoolean <- rownames(dataMetrics) %in% symbolicFldNames[ sapply(symbolicFldNames, function(colName) { isBoolean(dsFull[[colName]]) } ) ]
  } else {
    dataMetrics$isDate <- F
    dataMetrics$isBoolean <- F
  }  
  # Get AUC estimates for all predictors
  dataMetrics$Overlap <- NA
  dataMetrics$ksTest <- NA
  
#   dataMetrics$AUC_raw_dev <- NA
#   dataMetrics$AUC_raw_val <- NA
#   dataMetrics$AUC_rec_dev <- NA
#   dataMetrics$AUC_rec_val <- NA
  
  if (nrow(dataMetrics) != 1) {
    print(dataMetrics)
    stop("STOP: expected one row in data metrics frame")
  }
  
  u1 <- unique(dsFull[[1]])
  u2 <- unique(dfTest[[1]])
  dataMetrics$Overlap[1] <- length(intersect(u1,u2)) / length(union(u1,u2))
  
  if (dataMetrics$isNumeric[1] && dataMetrics$nDistinct[1] > 1) {
    # K-S test for similarity test/train distributions and checking overlap distincts test/train
    
    ksMetric <- suppressWarnings(ks.test(
      density(dsFull[[1]], na.rm=T)[["y"]], 
      density(dfTest[[1]], na.rm=T)[["y"]]) [["statistic"]])
    if (generatePlots) {
      try({
        plotFrame <- data.frame(c(dfDev[[1]], dfVal[[1]], dfTest[[1]]), 
                                factor(c(rep("dev",nrow(dfDev)),
                                         rep("val",nrow(dfVal)),
                                         rep("test",nrow(dfTest)))))
        names(plotFrame) <- c('values','dataset')
        densityPlot <-
          ggplot(plotFrame, aes(x=values)) + geom_density(aes(group=dataset, colour=dataset))+
            xlab(paste(fldName, "K-S score:", ksMetric, 
                       " Overlap:", dataMetrics$Overlap[1]))
        print(densityPlot)
        if (!is.null(plotFolder)) {
          ggsave(paste(plotFolder,'/plot_',fldName,'_density.png',sep=""))
        }
      })
    }
    
    dataMetrics$ksTest[1] <- ksMetric
  
    if (F) {
      # identify outliers (Ivar special code)
      freq <- table(dsFull[[1]])
      minObs  <- 100 # TODO: tune
      q <- quantile(freq, probs=c(0.25,0.75))
      limit <- q[2] + 3 * (q[2]-q[1]) # use the interquartile range for outliers
      limit <- max(limit, minObs) # at least nObs
      special <- freq[freq > limit]
      specialTot <- sum(special)
      cat("limit:", limit, "** special:", names(special), "freqs", special, 
          "tot:", specialTot, "\n")
    }    
  }
#   
#   if (dataMetrics$nDistinct[1] > 1) {
#     if (dataMetrics$isNumeric[1]) {
#       # fit a mini regression model?
# #       lm.model <- lm(target ~ ., data=dfDev[, c("target",fldName)])
# #       pf_dev <- data.frame( dfDev[[1]] )
# #       pf_val <- data.frame( dfVal[[1]])
# #       names(pf_dev) <- c(fldName)
# #       names(pf_val) <- c(fldName)
# #       dataMetrics$AUC_raw_dev[1] <- auc(vDevTarget, predict.lm(lm.model, pf_dev))
# #       dataMetrics$AUC_raw_val[1] <- auc(vValTarget, predict.lm(lm.model, pf_val))
#       dataMetrics$AUC_raw_dev[1] <- auc(vDevTarget, dfDev[[1]])
#       dataMetrics$AUC_raw_val[1] <- auc(vValTarget, dfVal[[1]])
#       sb <- createSymBin2(dfDev[[1]], vDevTarget, threshold)
#       #print(sb) # debug...
#       dataMetrics$AUC_rec_dev[1] <- auc(vDevTarget, applySymBin(sb, dfDev[[1]]))
#       dataMetrics$AUC_rec_val[1] <- auc(vValTarget, applySymBin(sb, dfVal[[1]]))
#     } else {
#       sb <- createSymBin2(dfDev[[1]], vDevTarget, threshold)
#       dataMetrics$AUC_rec_dev[1] <- auc(vDevTarget, applySymBin(sb, dfDev[[1]]))
#       dataMetrics$AUC_rec_val[1] <- auc(vValTarget, applySymBin(sb, dfVal[[1]]))
#     }
#   } else {
#     dataMetrics$AUC_rec_dev[1] <- dataMetrics$AUC_rec_val[1] <- 0.50
#   }
  
  return(dataMetrics)
}

# Data analysis on a train set (already split in dev/val)
dataAnalysis <- function(dfDev, dfVal, dfTest, generatePlots=F, plotFolder=NULL)
{
  metrics <- NULL
  for (fldName in names(dfTest)) {
    cat("Basic DA for: ", fldName, fill=T)
    metricOne <- dataAnalysisOne(dfDev[fldName], dfVal[fldName], dfTest[fldName], # data frames
                                 fldName, 
                                 generatePlots, plotFolder)
    #print(metricOne)
    if (is.null(metrics)) {
      metrics <- metricOne
    } else {
      metrics <- rbind(metrics, metricOne)
    }
  }
  return(metrics)
}
