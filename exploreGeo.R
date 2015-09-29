library(data.table)
library(dplyr)
library(stringdist)

train <- fread( "data/train_small.csv",header = T, sep = ",",
                stringsAsFactors=F,integer64="double",data.table=F )
test <- fread( "data/test_small.csv",header = T, sep = ",",
               stringsAsFactors=F,integer64="double",data.table=F)

# TODO: geo Interactions between zip and place may be interesting. For example: "zip code count per place" 
# or "place count per zip code" may provide a measure of population density or geographic size.
# zip VAR_0241 [zip], VAR_0242 is a 4-digit zip code add-on, VAR_0241,VAR_0200, VAR_0237 
# 0241: guess: Zip-Code + Zip4 + HouseNumber;  first 5 digits of VAR_0212 look like post code
geo <- data.frame(train$VAR_0200, # place
                  train$VAR_0274, # seems like state but isnt, probably
                  train$VAR_0212, # "zip+4" and house nr perhaps
                  train$VAR_0241, # zip
                  train$VAR_0242, 
                  train$VAR_0237, # state
                  train$VAR_0342)

# there are data mistakes, eg same zip & state, minor difference in city
#4    ZEPHERYHILLS             FL          33541
#5          ZEPHRY             FL          33542
#6     ZEPHRYHILLS             FL          33540
#7     ZEPHRYHILLS             FL          33541
#8     ZEPHRYHILLS             FL          33542
# and
#1      ZELIENOPLE             PA          16063
#2      ZELIONOPLE             PA          16063
#train <- train[train$VAR_0241 %in% c("42223"),]
reviewDupes <- group_by(train, VAR_0200, VAR_0237, VAR_0241) %>% 
  summarise(freq = n()) %>%
  mutate(stateZip = paste(VAR_0241, VAR_0237, sep="_"),
         fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_")) %>%
  distinct() %>% 
  ungroup() %>%
  arrange(stateZip, desc(freq))
potentialDupes <- group_by(reviewDupes, stateZip) %>% 
  dplyr::summarise(nSameStateZip = n(),
                   altName = first(VAR_0200), 
                   altID = first(fullGeoID)) %>% filter(nSameStateZip > 1)
dupes <- mutate(left_join(potentialDupes, reviewDupes, by="stateZip"), 
                dist=stringdist(altName, VAR_0200)) %>% filter(dist >= 1 & dist <= 2)




# Proxy for city size. 
# Join with main by paste of VAR_0200 + VAR_0237. Drop 0200 individually.
# we could use both train & test
dataset <- train # rbind(train, test)
zipcodesByCity <- group_by(unique(mutate(dataset, xtraCityState = paste(VAR_0200, VAR_0237, sep="_")) %>%
                                    group_by(xtraCityState, VAR_0241) %>%
                                    select(xtraCityState, VAR_0241) %>%
                                    ungroup() %>%
                                    arrange(VAR_0241)), xtraCityState) %>%
  summarise(xtraProxyCitySize = n()) %>% ungroup() %>% arrange(desc(xtraProxyCitySize))

print(head(zipcodesByCity,100))


# Proxy for population
# see also http://www.unitedstateszipcodes.org for interesting stats
# join with main by zip (0241)
# can use both train + test here
countByZip <- group_by(geo, train.VAR_0241) %>%
  summarise(xtraProxyPopulation = n()) %>%
  arrange(desc(xtraProxyPopulation))

# zip codes do have some intrinsic meaning: https://en.wikipedia.org/wiki/ZIP_code
# extract first 2-3 digits (state/region)
# just useful - count by n() will proxy population, use as symbolic might be useful (symbin??)
geo$xtraZip2 <- substr(geo$train.VAR_0241, 1, 2)
geo$xtraZip3 <- substr(geo$train.VAR_0241, 1, 3)

