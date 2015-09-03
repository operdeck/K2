library(data.table)
require(bit64)
library(dplyr)

macWD <- "~/Documents/science/kaggle/springleaf/K2"
winWD <- "D:/usr/science/kaggle/springleaf/K2"
if (file.exists(macWD)) {
  setwd(macWD)
} else {
  setwd(winWD)
}

stateCodes <- fread("./additionalData/USStates.csv", header = F, sep = ",")
setnames(stateCodes,c('ST','STNAME'))
setkey(stateCodes, 'STNAME')

cityInfo <- fread("./additionalData/SUB-EST2011_ALL.csv", header = T, sep = ",", drop=c('CENSUS2010POP'))
setkey(cityInfo, 'STNAME')

combi <- stateCodes[cityInfo] # join data.table

train_geo <- fread("./data/train-2.csv", header = T, sep = ",", integer64="double") %>% 
  select(ID, VAR_0200, VAR_0274)
test_geo <- fread("./data/test-2.csv", header = T, sep = ",", integer64="double") %>% 
  select(ID, VAR_0200, VAR_0274)
all_geo <- rbind(train_geo, test_geo) %>% arrange(VAR_0200, VAR_0274)

matchFound <- 0
noMatchFound <- 0
matchReused <- 0
all_geo$xtraSize <- NA
for (r in 1:nrow(all_geo)) {
  city <- all_geo$VAR_0200[r]
  state <- all_geo$VAR_0274[r]

  if (city == "" || state == "") { # TODO: also "-1"
    cat("NO MATCH MISSING INFO: ", city, "(", state, ")", matchFound, noMatchFound, matchReused, fill=T)
    noMatchFound <- noMatchFound+1
    all_geo$xtraSize[r] <- NA
  } else {
    if (r > 1 && city == all_geo$VAR_0200[r-1] && state == all_geo$VAR_0274[r-1]) {
      all_geo$xtraSize[r] <- all_geo$xtraSize[r-1]
      matchReused <- matchReused+1
    } else {
      found <- grep(paste("^",city,"$",sep=""), combi$NAME,ignore.case=TRUE) # exact case insense
      if (length(found) == 0) {
        found <- grep(paste("^",city," city$",sep=""), combi$NAME,ignore.case=TRUE) # w 'city' suffix
      }
      if (length(found) == 0) {
        found <- grep(paste("^",city," town$",sep=""), combi$NAME,ignore.case=TRUE) # w 'town' suffix
      }
      if (length(found) == 0) {
        found <- grep(paste("^",city," ",sep=""), combi$NAME,ignore.case=TRUE) # starts with
      }
      matches <- filter(combi[found], ST==state) %>% 
        select(ST,STNAME,NAME,POPESTIMATE2011)
    
      if (nrow(matches) > 0) {
        #cat("Matching: ", city, "(", state, ")", fill=T)
        #print(matches[1])
        matchFound <- matchFound+1
        all_geo$xtraSize[r] <- matches$POPESTIMATE2011[1]
      } else {
        # Unmatched are probably small cities
        cat("NO MATCH: ", city, "(", state, ")", matchFound, noMatchFound, matchReused, fill=T)
        noMatchFound <- noMatchFound+1
        all_geo$xtraSize[r] <- 0
      }
    }
  }
}
cat("No matches:", noMatchFound, fill=T)
cat("Matches:", matchFound, fill=T)
cat("Re-used:", matchReused, fill=T)
print(dim(all_geo))
write.table(all_geo, 
            "./additionalData/cityInfo.csv", sep=",", row.names=F)
