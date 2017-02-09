#### Include stuff from laptop here


library(lubridate)
library(reshape2)
library(magrittr)
library(readr)
library(data.table)
#senti_combined <- readRDS("./data/senti_combined.rds")

senti_combined <- readRDS("./data/senti_combined_full.rds")

senti_combined$party <- as.factor(senti_combined$party)

senti_combined$party_group <- as.factor(senti_combined$party_group)

## Recoding senti_combined$party_group into senti_combined$party_group
senti_combined$party_group <- as.character(senti_combined$party_group)
senti_combined$party_group[senti_combined$party_group == "#N/A"] <- "Other"
senti_combined$party_group[senti_combined$party_group == "#REF!"] <- "Other"
senti_combined$party_group <- factor(senti_combined$party_group)

summary(senti_combined)


senti_combined$element_id <- NULL
senti_combined$id <- NULL
senti_combined$proper_id <- NULL
senti_combined$proper_name <- NULL
senti_combined$party <- NULL
senti_combined$sentence <- NULL
senti_combined$element_id2 <- NULL
senti_combined$word_count <- NULL
senti_combined$sd <- NULL

summary(senti_combined)

names(senti_combined)

senti_combined2 <- melt(senti_combined, id.vars = c("speech_date", "party_group",  "debate_type"))

summary(senti_combined2)

senti_combined2$debate_type <- as.factor(senti_combined2$debate_type)

senti_combined3 <- aggregate(value~speech_date + party_group + debate_type + variable, data=senti_combined2, FUN=mean)

senti_combined3$year <- lubridate::year(senti_combined3$speech_date)

class(senti_combined3)

class(senti_combined3$speech_date)


Baldwin2 <- subset(senti_combined3, speech_date >= "1935-11-14" & speech_date <= "1937-05-28")
Chamberlain1 <- subset(senti_combined3, speech_date >= "1937-05-28" & speech_date <= "1939-09-03")
Chamberlain2 <- subset(senti_combined3, speech_date >= "1939-09-03" & speech_date <= "1940-05-10")
Churchill1 <- subset(senti_combined3, speech_date >= "1940-05-10" & speech_date <= "1945-05-22")
Churchill2 <- subset(senti_combined3, speech_date >= "1945-05-23" & speech_date <= "1945-06-25")
Atlee1 <- subset(senti_combined3, speech_date >= "1945-06-26" & speech_date <= "1950-02-22")
Atlee2 <- subset(senti_combined3, speech_date >= "1950-02-23" & speech_date <= "1951-10-26")
Churchill3 <- subset(senti_combined3, speech_date >= "1951-10-26" & speech_date <= "1955-04-05")
Eden1 <- subset(senti_combined3, speech_date >= "1955-04-06" & speech_date <= "1955-05-28")
Eden2 <- subset(senti_combined3, speech_date >= "1955-05-29" & speech_date <= "1957-01-10")
Macmillan1 <- subset(senti_combined3, speech_date >= "1957-01-10" & speech_date <= "1959-10-08")
Macmillan2 <- subset(senti_combined3, speech_date >= "1959-10-09" & speech_date <= "1963-10-18")
DouglasHome <- subset(senti_combined3, speech_date >= "1963-10-19" & speech_date <= "1964-10-15")
Wilson1 <- subset(senti_combined3, speech_date >= "1964-10-16" & speech_date <= "1966-03-31")
Wilson2 <- subset(senti_combined3, speech_date >= "1966-04-01" & speech_date <= "1970-06-18")
Heath <- subset(senti_combined3, speech_date >= "1970-06-19" & speech_date <= "1974-03-03")
Wilson3 <- subset(senti_combined3, speech_date >= "1974-03-04" & speech_date <= "1976-04-04")
Callaghan <- subset(senti_combined3, speech_date >= "1976-04-05" & speech_date <= "1979-05-03")
Thatcher1 <- subset(senti_combined3, speech_date >= "1979-05-04" & speech_date <= "1983-06-08")
Thatcher2 <- subset(senti_combined3, speech_date >= "1983-06-09" & speech_date <= "1987-06-10")
Thatcher3 <- subset(senti_combined3, speech_date >= "1987-06-11" & speech_date <= "1990-11-27")
Major1 <- subset(senti_combined3, speech_date >= "1990-11-28" & speech_date <= "1992-04-09")
Major2 <- subset(senti_combined3, speech_date >= "1992-04-10" & speech_date <= "1997-05-01")
Blair1 <- subset(senti_combined3, speech_date >= "1997-05-02" & speech_date <= "2001-06-06")
Blair2 <- subset(senti_combined3, speech_date >= "2001-06-07"& speech_date <= "2005-05-04")
Blair3 <- subset(senti_combined3, speech_date >= "2005-05-05"& speech_date <= "2007-06-26")
Brown <- subset(senti_combined3, speech_date >= "2007-06-27" & speech_date <= "2010-05-10")
Cameron1 <- subset(senti_combined3, speech_date >= "2010-05-11" & speech_date <= "2015-05-07")
Cameron2 <- subset(senti_combined3, speech_date >= "2015-05-08" & speech_date <= "2016-07-12")
May <- subset(senti_combined3, speech_date >= "2016-07-13" & speech_date <= "2016-12-31")


  Baldwin2$ministry <- "Baldwin2"
  Baldwin2$government <- ifelse(Baldwin2$party_group == "Conservative",
                                "Government", "Opposition")
  
  Chamberlain1$ministry <- "Chamberlain1"
  Chamberlain1$government <- ifelse(Chamberlain1$party_group == "Conservative",
                                    "Government", "Opposition")
  
  Chamberlain2$ministry <- "Chamberlain2"
  Chamberlain2$government <- ifelse(Chamberlain2$party_group == "Conservative",
                                    "Government", "Opposition")
  
  Churchill1$ministry <- "Churchill1"
  Churchill1$government <- ifelse(Churchill1$party_group == "Conservative",
                                  "Government", "Opposition")
  
  Churchill2$ministry <- "Churchill2"
  Churchill2$government <- ifelse(Churchill2$party_group == "Conservative",
                                  "Government", "Opposition")
  
  Atlee1$ministry <- "Atlee1"
  Atlee1$government <- ifelse(Atlee1$party_group == "Labour",
                              "Government", "Opposition")
  
  Atlee2$ministry <- "Atlee2"
  Atlee2$government <- ifelse(Atlee2$party_group == "Labour",
                              "Government", "Opposition")
  
  Churchill3$ministry <- "Churchill3"
  Churchill3$government <- ifelse(Churchill3$party_group == "Conservative",
                                 "Government", "Opposition")
  
  Eden1$ministry <- "Eden1"
  Eden1$government <- ifelse(Eden1$party_group == "Conservative",
                             "Government", "Opposition")
  
  Eden2$ministry <- "Eden2"
  Eden2$government <- ifelse(Eden2$party_group == "Conservative",
                             "Government", "Opposition")
  
  Macmillan1$ministry <- "Macmillan1"
  Macmillan1$government <- ifelse(Macmillan1$party_group == "Conservative",
                                  "Government", "Opposition")
  
  Macmillan2$ministry <- "Macmillan2"
  Macmillan2$government <- ifelse(Macmillan2$party_group == "Conservative",
                                  "Government", "Opposition")
  
  DouglasHome$ministry <- "DouglasHome"
  DouglasHome$government <- ifelse(DouglasHome$party_group == "Conservative",
                                   "Government", "Opposition")
  
  Wilson1$ministry <- "Wilson1"
  Wilson1$government <- ifelse(Wilson1$party_group == "Labour",
                               "Government", "Opposition")
  
  Wilson2$ministry <- "Wilson2"
  Wilson2$government <- ifelse(Wilson2$party_group == "Labour",
                               "Government", "Opposition")
  
  Heath$ministry <- "Heath"
  Heath$government <- ifelse(Heath$party_group == "Conservative",
                             "Government", "Opposition")
  
  Wilson3$ministry <- "Wilson3"
  Wilson3$government <- ifelse(Wilson3$party_group == "Labour",
                               "Government", "Opposition")
  
  Callaghan$ministry <- "Callaghan"
  Callaghan$government <- ifelse(Callaghan$party_group == "Labour",
                                     "Government", "Opposition")
  
  Thatcher1$ministry <- "Thatcher1"
  Thatcher1$government <- ifelse(Thatcher1$party_group == "Conservative",
                                 "Government", "Opposition")
  
  Thatcher2$ministry <- "Thatcher2"
  Thatcher2$government <- ifelse(Thatcher2$party_group == "Conservative",
                                 "Government", "Opposition")
  
  Thatcher3$ministry <- "Thatcher3"
  Thatcher3$government <- ifelse(Thatcher3$party_group == "Conservative",
                                 "Government", "Opposition")
  
  Major1$ministry <- "Major1"
  Major1$government <- ifelse(Major1$party_group == "Conservative",
                              "Government", "Opposition")
  
  Major2$ministry <- "Major2"
  Major2$government <- ifelse(Major2$party_group == "Conservative",
                              "Government", "Opposition")
  

Blair1$ministry <- "Blair1"
Blair1$government <- ifelse(Blair1$party_group == "Labour",
                                  "Government", "Opposition")

Blair2$ministry <- "Blair2"
Blair2$government <- ifelse(Blair2$party_group == "Labour",
                            "Government", "Opposition")

Blair3$ministry <- "Blair3"
Blair3$government <- ifelse(Blair3$party_group == "Labour",
                            "Government", "Opposition")

Brown$ministry <- "Brown"
Brown$government <- ifelse(Brown$party_group == "Labour",
                            "Government", "Opposition")

Cameron1$ministry <- "Cameron1"
Cameron1$government <- ifelse(Cameron1$party_group == "Conservative" |
                             Cameron1$party_group == "Liberal Democrat" ,
                           "Government", "Opposition")

Cameron2$ministry <- "Cameron2"
Cameron2$government <- ifelse(Cameron2$party_group == "Conservative",
                           "Government", "Opposition")

May$ministry <- "May"
May$government <- ifelse(May$party_group == "Conservative",
                           "Government", "Opposition")



senti_combined <- rbind(Blair1, Blair2, Blair3, Brown, Cameron1, Cameron2,May, 
                        Baldwin2, Chamberlain1, Chamberlain2, Churchill1, Churchill2,
                        Atlee1, Atlee2, Churchill3, Eden1, Eden2, Macmillan1, Macmillan2, 
                        DouglasHome, Wilson1, Wilson2, Heath, Wilson3, Callaghan, 
                        Thatcher1, Thatcher2, Thatcher3, Major1, Major2)

rm(Blair1, Blair2, Blair3, Brown, Cameron1, Cameron2, May, Baldwin2,
   Chamberlain1, Chamberlain2, Churchill1, Churchill2,Atlee1, Atlee2,
   Churchill3, Eden1, Eden2, Macmillan1, Macmillan2, DouglasHome,
   Wilson1, Wilson2, Heath, Wilson3, Callaghan,Thatcher1, Thatcher2,
   Thatcher3, Major1, Major2, senti_combined2, senti_combined3)

names(senti_combined)

names(senti_combined)[5] <- "sentiment"
names(senti_combined)[4] <- "senti_type"

senti_combined$ministry <- as.factor(senti_combined$ministry)

senti_combined$government <- as.factor(senti_combined$government)

summary(senti_combined)


all_average <- tapply(senti_combined$sentiment, list(senti_combined$debate_type, 
                                                 senti_combined$year, 
                                                 senti_combined$senti_type), mean)

summary(all_average)

all_average <- melt(all_average,
                    variable.name = c("debate_type","year", "senti_type"),
                    value.names = "sentiment", 
                    measure.vars = "sentiment")

summary(all_average)
names(all_average)[1] <- "debate_type"
names(all_average)[2] <- "year"
names(all_average)[3] <- "senti_type"
names(all_average)[4] <- "sentiment"
summary(all_average)


party_average <- tapply(senti_combined$sentiment, list(senti_combined$debate_type, 
                                                   senti_combined$year, 
                                                   senti_combined$party_group,
                                                   senti_combined$senti_type), mean)

summary(party_average)

party_average <- melt(party_average,
                      variable.name = c("type","party_group","year", "senti_type"), 
                      value.names = "sentiment", 
                      measure.vars = "sentiment")

summary(party_average)

names(party_average)[1] <- "debate_type"
names(party_average)[2] <- "year"
names(party_average)[3] <- "party_group"
names(party_average)[4] <- "senti_type"
names(party_average)[5] <- "sentiment"

summary(party_average)


government_average <- tapply(senti_combined$sentiment, list(senti_combined$debate_type,
                                                            senti_combined$year,
                                                            senti_combined$government,
                                                            senti_combined$senti_type), mean)

summary(government_average)

government_average <- melt(government_average,
                      variable.name = c("debate_type","government","year", "senti_type"), 
                      value.names = "sentiment", 
                      measure.vars = "sentiment")

summary(government_average)

names(government_average)[1] <- "debate_type"
names(government_average)[2] <- "year"
names(government_average)[3] <- "government"
names(government_average)[4] <- "senti_type"
names(government_average)[5] <- "sentiment"

summary(government_average)

summary(senti_combined)

party_average <- data.table(party_average)

all_average <- data.table(all_average)

government_average <- data.table(government_average)

senti_combined <- data.table(senti_combined)


write_rds(party_average, "./data/party_average.rds")

write_rds(all_average, "./data/all_average.rds")

write_rds(government_average, "./data/government_average.rds")

write_rds(senti_combined, "./data/senti_combined.rds")
