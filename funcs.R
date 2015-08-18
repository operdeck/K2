library(scales)

# Threshold for volume % in symbin
defaultSymBinSizeThreshold = 0.001

# TODO
# Creates a binning object from a vector of values and outcomes, grouping
# the values with a frequency above the threshold in distinct bins, the
# rest in a residual bin.
# Result is a dataframe with
#  binindex = bin index (1:N)
#  val = value
#  cases = number of cases
#  avgoutcome = average behaviour
#  freq = number of cases as percentage
# Sorted by increasing avgoutcome
createSymBin2 <- function(ds, fieldName, outcomeName, threshold = defaultSymBinSizeThreshold) 
{
  val = ds[,fieldName]
  outcome = ds[,outcomeName]
 
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
  # select only values that have frequency above threshold
  result <- filter(g, t)
  # calculate outcome for all other values, or if there are none, the overall average
  nRemainingCases <- total - sum(result$cases)
  if (nRemainingCases > 0) {
    residualOutcome <-
      sum( (filter(g, !t) %>% mutate( sumavgoutcome = avgoutcome*cases ))$sumavgoutcome ) / nRemainingCases
  } else {
    residualOutcome <- mean(outcome, na.rm=TRUE)
  }
  # bind a 'residual' row for values with frequency below threshold
  result <- rbind(result, c(NA,nRemainingCases,residualOutcome,nRemainingCases/total))
  result$binindex <- seq(1:nrow(result))
  if (nrow(result) > 1) {
    return(select( result, -t))
  } else {
    setnames(result, c('val','cases','avgoutcome','freq'))
    return(result)
  }
}

# Apply sym binning to a vector of values, returning a vector of bin indices
# Values not in the binning will get the 'residual' bin index
applySymBinIndex <- function(binning, values) 
{
  df <- data.frame(values)
  setnames(df, c('val'))
  r <- left_join(df, binning[seq(1:(nrow(binning)-1)),], by="val")
  #print(r)
  return ( ifelse(is.na(r$binindex), binning$binindex[nrow(binning)], r$binindex) )
}

# Apply sym binning to a vector of values, returning a vector of recoded values
# Values not in the binning will get the 'residual' recoding
applySymBin <- function(binning, values) 
{
  df <- data.frame(values)
  setnames(df, c('val'))
  r <- left_join(df, binning[seq(1:(nrow(binning)-1)),], by="val")
  #print(r)
  return ( ifelse(is.na(r$avgoutcome), binning$avgoutcome[nrow(binning)], r$avgoutcome) )
}

# Plot symbolic data analysis for one set of vectors
sb.plotOne <- function(binning, 
                       ds_dev, ds_val, ds_tst,
                       fieldName, outcomeName,
                       plotFolder = NA)
{
  ds_dev_bins <- applySymBinIndex(binning, ds_dev[,fieldName])
  df_dev <- data.frame( ds_dev_bins, ds_dev[,outcomeName])
  names(df_dev) <- c('binindex','beh')
  
  ds_val_bins <- applySymBinIndex(binning, ds_val[,fieldName])
  df_val <- data.frame( ds_val_bins, ds_val[,outcomeName])
  names(df_val) <- c('binindex','beh')
  
  ds_tst_bins <- applySymBinIndex(binning, ds_tst[,fieldName])
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
    if (!is.na(plotFolder)) {
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
    if (!is.na(plotFolder)) {
      ggsave(paste(plotFolder,'/plot_',fieldName,'_bin_beh.png',sep=""))
    }
  })
  
  return (df_summarized)
}


numbinner <- function(vec_dev, vec_val, vec_tst, beh_dev, beh_val, n)
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

nb.plotAll <- function(df_dev, df_val, df_tst, outcomeName, nbins=10, plotFolder=NA) 
{
  beh_dev <- df_dev[,outcomeName]
  beh_val <- df_val[,outcomeName]
  for (fieldName in names(df_dev))
  {
    vec_dev <- df_dev[,fieldName]
    if (is.numeric(vec_dev)) {
      vec_val <- df_val[,fieldName]
      vec_tst <- df_tst[,fieldName]
      binz <- numbinner(vec_dev, vec_val, vec_tst, beh_dev, beh_val, nbins)
      cat("Summary num",fieldName,fill=T)
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
        if (!is.na(plotFolder)) {
          ggsave(paste(trainDataFolder,'/plot_',fieldName,'_raw_freq.png',sep=""))
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
        if (!is.na(plotFolder)) {
          ggsave(paste(plotFolder,'/plot_',fieldName,'_raw_beh.png',sep=""))
        }
      })
    }
  }
}

# replace NAs in test set with mean - should do knnImpute
imputeNAs <- function(ds) {
  ds <- as.data.frame(ds)
  cat("Imputing",sum(!complete.cases(ds)),"cases from total of",nrow(ds), fill=TRUE)
  for (colNo in which(sapply(ds, is.numeric))) {
    aCol <- select(ds, colNo)
    if (any(is.na(aCol))) {
      m <- colMeans(aCol,  na.rm = TRUE)
      cat("   imputing", colnames(ds) [colNo], sum(!complete.cases(aCol)),"NAs with mean", m, fill=TRUE)
      ds[!complete.cases(aCol), colNo] <- m
    }
  }
  return(ds)
}

# Kaggle's evaluation function
logLoss <- function(act, pred)
{
  eps <- 1e-15
  if (!is.logical(act)) {
    stop("Logloss expects a logical as first argument")
  }
  ll <- -1*sum(act*log(pmax(eps,pred)) + (1-act)*log(pmax(eps,1-pred)))/length(pred)
  return(ll)
}


