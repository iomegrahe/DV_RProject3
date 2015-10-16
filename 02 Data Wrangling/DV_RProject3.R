require("jsonlite")
require("RCurl")
require(tidyr)
require(dplyr)
require(ggplot2)

MEDIANHOMERENTS <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from MEDIANHOMERENTS"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#MEDIANHOMERENTS
summary(MEDIANHOMERENTS)
head(MEDIANHOMERENTS)

MEDIANHOMEVALUES <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from MEDIANHOMEVALUES"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#MEDIANHOMEVALUES
summary(MEDIANHOMEVALUES)
head(MEDIANHOMEVALUES)

# Change the USER and PASS below to be your UTEid
STARBUCKS <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from STARBUCKS"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#STARBUCKS
summary(STARBUCKS)
head(STARBUCKS)

#tbl_df(STARBUCKS)
#View(STARBUCKS)

DAIRYQUEEN <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select * from DAIRYQUEEN"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521/PDBF15DV.usuniversi01134.oraclecloud.internal', USER='cs329e_jso464', PASS='orcl_jso464', MODE='native_mode', MODEL='model', returnDimensions = 'False', returnFor = 'JSON'), verbose = TRUE), ))
#DAIRYQUEEN
summary(DAIRYQUEEN)
head(DAIRYQUEEN)


combinedone <- dplyr::inner_join(MEDIANHOMERENTS, MEDIANHOMEVALUES, by=c("CITY", "ST"))

combinedone %>% select(SIZERANK.x, RENTS, HOMEVAL) %>% mutate(HOMEVAL_PERCENT = cume_dist(HOMEVAL), RENTS_PERCENT = cume_dist(RENTS)) %>% filter(HOMEVAL_PERCENT >= .80) %>% ggplot(aes(x = SIZERANK.x, y = RENTS_PERCENT)) + geom_point(size=1.5)
#This shows that in the top 20% most expensive cities, The most expensive rents are in the biggest cities

combinedone %>% select(SIZERANK.x, RENTS, HOMEVAL) %>% mutate(HOMEVAL_PERCENT = cume_dist(HOMEVAL), RENTS_PERCENT = cume_dist(RENTS)) %>% filter(RENTS_PERCENT >= .90) %>% ggplot(aes(x = SIZERANK.x, y = HOMEVAL_PERCENT)) + geom_point(size=1.5)
#However it's very interesting to see that a substantial number of the highest-rent homes (top 10%) are not, in fact, in cities with the highest home prices. This signifies that in some cases at the top, rent may not be driven by home-value, instead it is noticeably driven by size of the city. 

leftjoinedone <- dplyr::left_join(STARBUCKS, MEDIANHOMERENTS, by=c("CITY", "ST"))

leftjoinedone %>% select(STORE_NAME, CITY, ST, SIZERANK, RENTS) %>% mutate(RENTS_PERCENT = cume_dist(RENTS)) %>% filter(!is.na(RENTS), !is.na(SIZERANK), SIZERANK <= 5000) %>% ggplot(aes(x = SIZERANK, y = RENTS_PERCENT)) + geom_jitter(size=1, alpha=1,position=position_jitter(height=.01))
#Starbucks is most prominent in the biggest cities - in most cases it doesn't even care about having a location in the smaller cities. Within those big cities there is a noticeable tendency toward the big cities with low rent, which seems to suggest that Starbucks, rather than being marketed towards high-income earners, is instead marketed towards low-income earners in big cities to try and make them feel rich. Stellar marketing.


fulljoinedone <- dplyr::full_join(STARBUCKS, DAIRYQUEEN, by="ZIP") %>% mutate(CITY = ifelse(!is.na(CITY.x), paste(CITY.x), paste(CITY.y))) %>% mutate(ST = ifelse(!is.na(ST.x), paste(ST.x), paste(ST.y))) %>% mutate(KEY = ifelse(is.na(STORE_NAME.x), "DQ", ifelse(is.na(STORE_NAME.y), "SBUX", "BOTH"))) %>% filter(!is.na(CITY), !is.na(ST))

leftjoinedtwo <- dplyr::left_join(fulljoinedone, MEDIANHOMERENTS, by=c("CITY", "ST"))

leftjoinedtwo %>% select(KEY, SIZERANK, RENTS) %>% mutate(RENTS_PERCENT = cume_dist(RENTS)) %>% filter(!is.na(RENTS), !is.na(SIZERANK)) %>% ggplot(aes(x = SIZERANK, y = RENTS_PERCENT)) + geom_jitter(size=1, alpha=1,position=position_jitter(height=.01), aes(colour = KEY))
#While we tend to see Starbucks stores in cities with both high and low rent, the majority of DairyQueen stores are in cities with low rent. They are also not as heavily weighted towards the larger cities as Starbucks is.

