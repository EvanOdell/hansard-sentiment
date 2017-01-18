

#### SENTI PREP and Creation

library(readr)
library(magrittr)
library(plyr)
library(dplyr)
library(tm)
library(tidytext)
library(data.table)
library(syuzhet)
library(sentimentr)

#write_rds(debate, "debate.rds")

system.time(
  debate <- readRDS("debate.rds")
)

debate_no_na <- subset(debate, speakername != "NA")

summary(debate_no_na)

#Import the matched files

#names(debate)

Matched2000_2009 <- read_csv("~/Documents/hansard1936-2016/Matched2000-2009.csv")

Matched2010_2016 <- read_csv("~/Documents/hansard1936-2016/Matched2010-2016.csv")

Matched2000_2009$speakerid <- as.character(Matched2000_2009$speakerid)

Matched2000_2009$proper_id <- as.character(Matched2000_2009$proper_id)

Matched2010_2016$speakerid <- as.character(Matched2010_2016$speakerid)

Matched2010_2016$proper_id <- as.character(Matched2010_2016$proper_id)

match <- bind_rows(Matched2000_2009,Matched2010_2016)

rm(Matched2000_2009,Matched2010_2016)

system.time(
  debate2 <- debate_no_na %>% 
    left_join(match, by= "id")
)



#system.time(
#  write_csv(debate2, "debate2.csv")
#)


system.time(
  hansard_sample <- debate2[sample(nrow(debate2), (nrow(debate2)/100)), ]
  #hansard_sample <- debate2[sample(nrow(debate2), (nrow(debate2)/20)), ]
)



write_rds(hansard_sample, "hansard_samples.rds")

rm(debate, debate2, debate_no_na)

#disability_sample <- readRDS("disability_sample.rds")

#hansard_sample <- readRDS("hansard_sample.rds")


debate_sentences <- hansard_sample %>%
  select(speech, id, speech_date, name, proper_id, party, party_group) %>% 
  unnest_tokens(sentence, speech, token = "regex", pattern = "\\.")
debate_sentences$debate_type <- "All Debate"

bing <- as_key(syuzhet:::bing)
afinn <- as_key(syuzhet:::afinn)
nrc <- data.frame(
  words = rownames(syuzhet:::nrc),
  polarity = syuzhet:::nrc[, "positive"] - syuzhet:::nrc[, "negative"],
  stringsAsFactors = FALSE
) %>%
{as_key(.[.[["polarity"]] != 0, ])}


debate_sentences <- setDT(debate_sentences, keep.rownames = TRUE)[]
names(debate_sentences)[1] <- "element_id"

write_rds(debate_sentences, "debate_sentences.rds")

#debate_afinn <- debate_afinn[,c("id","afinn_vector",)]

system.time(
  afinn_vector <- sentiment_by(debate_sentences$sentence, by = NULL, group.names, afinn)
)

system.time(
  nrc_vector <- sentiment_by(debate_sentences$sentence, by = NULL, group.names, nrc)
)

#write_rds(debate_sentences, "debate_sentences.rds")

system.time(
  bing_vector <- sentiment_by(debate_sentences$sentence, by = NULL, group.names, bing)
)

system.time(
  sentiword_vector <- sentiment_by(debate_sentences$sentence, by = NULL, group.names, lexicon::hash_sentiword)
)

system.time(
  hu_vector <- sentiment_by(debate_sentences$sentence, by = NULL, group.names)
)


names(afinn_vector)[4] <- "afinn_sentiment"

names(nrc_vector)[4] <- "nrc_sentiment"

names(bing_vector)[4] <- "bing_sentiment"

names(sentiword_vector)[4] <- "sentiword_sentiment"

names(hu_vector)[4] <- "hu_sentiment"

hu_vector <- hu_vector[,c("hu_sentiment")]

sentiword_vector <- sentiword_vector[,c("sentiword_sentiment")]

bing_vector <- bing_vector[,c("bing_sentiment")]

nrc_vector <- nrc_vector[,c("nrc_sentiment")]

senti_vectors <- cbind(debate_sentences, afinn_vector, hu_vector, sentiword_vector, nrc_vector, bing_vector)

summary(senti_vectors)


### For disability sample party group, load table, create csv of just id and party, create party group, load c

disability_sample <- read_csv("~/Documents/hansard1936-2016/disability_sample.csv")

#disability_check <- disability_sample[c("id", "party")]

#write_csv(disability_check, "disability_check.csv")

disability_check <- read_csv("~/Documents/hansard1936-2016/disability_check.csv")

disability_check$party <- NULL

system.time(
  disability_sample <- disability_sample %>% 
    left_join(disability_check, by= "id")
)


disability_sentences <- disability_sample %>%
  select(speech, id, speech_date, name, proper_id, party, party_group) %>% 
  unnest_tokens(sentence, speech, token = "regex", pattern = "\\.")
disability_sentences$debate_type <- "Disability"

disability_sentences <- setDT(disability_sentences, keep.rownames = TRUE)[]
names(disability_sentences)[1] <- "element_id"

system.time(
  afinn_vector_dis <- sentiment_by(disability_sentences$sentence, by = NULL, group.names, afinn)
)

system.time(
  nrc_vector_dis <- sentiment_by(disability_sentences$sentence, by = NULL, group.names, nrc)
)


### Re-Run
Sys.setlocale(locale="C")
system.time(
  bing_vector_dis <- sentiment_by(disability_sentences$sentence, by = NULL, group.names, bing)
)

system.time(
  sentiword_vector_dis <- sentiment_by(disability_sentences$sentence, by = NULL, group.names, lexicon::hash_sentiword)
)

system.time(
  hu_vector_dis <- sentiment_by(disability_sentences$sentence, by = NULL, group.names)
)


names(afinn_vector_dis)[4] <- "afinn_sentiment"

names(nrc_vector_dis)[4] <- "nrc_sentiment"

names(bing_vector_dis)[4] <- "bing_sentiment"###Still needs to run

names(sentiword_vector_dis)[4] <- "sentiword_sentiment"

names(hu_vector_dis)[4] <- "hu_sentiment"

hu_vector_dis <- hu_vector_dis[,c("hu_sentiment")]

sentiword_vector_dis <- sentiword_vector_dis[,c("sentiword_sentiment")]

bing_vector_dis <- bing_vector_dis[,c("bing_sentiment")]###Still needs to run

nrc_vector_dis <- nrc_vector_dis[,c("nrc_sentiment")]

senti_vectors_dis <- cbind(disability_sentences, afinn_vector_dis,
                           hu_vector_dis, sentiword_vector_dis,
                           nrc_vector_dis, bing_vector_dis)

summary(senti_vectors_dis)
summary(senti_vectors)

names(senti_vectors)
names(senti_vectors_dis)

senti_combined <- rbind(senti_vectors, senti_vectors_dis)

names(senti_combined)[10] <- "element_id2"

summary(senti_combined)

senti_combined$party_group [is.na(senti_combined$party_group)] <- "Other"

write_rds(senti_combined, "senti_combined.rds")


library(lubridate)
library(reshape2)
library(magrittr)
library(readr)
#senti_combined <- readRDS("./data/senti_combined.rds")

senti_combined <- readRDS("senti_combined.rds")

senti_combined$party <- as.factor(senti_combined$party)

senti_combined$party_group <- as.factor(senti_combined$party_group)

senti_combined$party_group [is.na(senti_combined$party_group )] <- "Other"

senti_combined$sentences <- NULL
senti_combined$id <- NULL
senti_combined$colnum <- NULL
senti_combined$time <- NULL
senti_combined$url <- NULL
senti_combined$speaker <- NULL
senti_combined$proper_id <- NULL
senti_combined$party <- NULL
senti_combined$sentence <- NULL
senti_combined$element_id <- NULL
senti_combined$element_id2 <- NULL
senti_combined$name <- NULL
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

names(senti_combined)[5] <- "sentiment"
names(senti_combined)[4] <- "senti_type"

senti_combined$ministry <- as.factor(senti_combined$ministry)

senti_combined$government <- as.factor(senti_combined$government)

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

write_rds(party_average, "./data/party_average.rds")

write_rds(all_average, "./data/all_average.rds")

write_rds(government_average, "./data/government_average.rds")

write_rds(senti_combined, "./data/senti_combined.rds")
