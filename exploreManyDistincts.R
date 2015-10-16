# This script basically clusters the related text fields using 
# a stringdistmatrix and then clustering on that
# From: https://www.kaggle.com/cauldnz/springleaf-marketing-response/clustering-the-text-fields/code

library(data.table)
#A bunch of ideas from http://amunategui.github.io/stringdist/

# Load train and test data
cat("reading the train and test data\n")
train <- fread("data/train-2.csv", na.strings = c("","NA","-1","9999","999")) # from Bluefool
test <- fread("data/test-2.csv", na.strings = c("","NA","-1","9999","999"))

library(tm)
library(RCurl)
library(qdap)
library(stringdist)

# corpus <- Corpus(VectorSource(raw493))
# 
# corpus.mapped <- corpus
# corpus.mapped <- tm_map(corpus.mapped,removeNumbers)
# corpus.mapped <- tm_map(corpus.mapped,removePunctuation)
# corpus.mapped <- tm_map(corpus.mapped, content_transformer(tolower))
# corpus.mapped <- tm_map(corpus.mapped,removeWords, words = c("licensed","registered","certified","professional"))
# #corpus.mapped <- tm_map(corpus.mapped,stemDocument,language="english")
# 
# inspect(corpus.mapped)
# 
# #unique.0493 <- unlist(sapply(corpus.mapped, `[`, "content"))
# #unique.0493 <- unique(raw493)

getTextCluster <- function ( values )
{
  uniques <- unique(values)
  uniques[is.na(uniques)] <- ""
  string.distances <- stringdistmatrix(uniques,uniques,method = "jw")
  is.na(string.distances) <- do.call(cbind,lapply(string.distances, is.infinite))
  rownames(string.distances) <- uniques
  hc <- hclust(as.dist(string.distances))
  dfClust <- data.frame(uniques, cutree(hc, k=200))
  names(dfClust) <- c('modelname','cluster')
#   plot(table(dfClust$cluster))
  t <- table(dfClust$cluster)
  t <- cbind(t,t/length(dfClust$cluster))
  t <- t[order(t[,2], decreasing=TRUE),]
  p <- data.frame(factorName=rownames(t), binCount=t[,1], percentFound=t[,2])
  dfClust <- merge(x=dfClust, y=p, by.x = 'cluster', by.y='factorName', all.x=T)
  dfClust <- dfClust[rev(order(dfClust$binCount)),]
  names(dfClust) <-  c('cluster','modelname')
  mapping <- dfClust[c('cluster','modelname')]
  return(mapping)
}

map493 <- getTextCluster(c(unique(train$VAR_0493),unique(test$VAR_0493)))
names(map493) <- c("VAR_0493_cluster", "VAR_0493")
print(summary(map493))

map404 <- getTextCluster(c(unique(train$VAR_0404),unique(test$VAR_0404)))
names(map404) <- c("VAR_0404_cluster", "VAR_0404")
print(summary(map404))
