
library(lubridate)
library(reshape2)
afinn_combined <- readRDS("afinn_combined.rds")

summary(afinn_combined)

afinn_combined$year <- year(afinn_combined$date)

afinn_combined$type <- as.factor(afinn_combined$type)

afinn_combined$party_group <- as.factor(afinn_combined$party_group)

afinn_combined <- melt(afinn_combined, id.vars = c("date", "year", "party_group", "type"))

afinn_combined <- data.frame(afinn_combined, stringsAsFactors = FALSE)

summary(afinn_combined)

afinn_combined$year <- as.character(afinn_combined$year)
afinn_combined$year <- as.numeric(afinn_combined$year)


names(afinn_combined)[6] <- "sentiment"

#afinn_combined <- subset(afinn_combined, is.na(party_group)==FALSE)


Baldwin
Chamberlain1
Chamberlain2
Churchill1
Churchill2
Atlee1
Atlee2
Churchill3
Eden
Macmillan
DouglasHome
Wilson1 <- subset(afinn_combined, date >= "1964-10-16" & date <= "1966-03-31")
Wilson2 <- subset(afinn_combined, date >= "1966-04-01" & date <= "1970-06-18")
Heath <- subset(afinn_combined, date >= "1970-06-19" & date <= "1974-03-03")
Wilson3 <- subset(afinn_combined, date >= "1974-03-04" & date <= "1976-04-04")
Callaghan <- subset(afinn_combined, date >= "1976-04-05" & date <= "1979-05-03")
Thatcher1 <- subset(afinn_combined, date >= "1979-05-04" & date <= "1983-06-08")
Thatcher2 <- subset(afinn_combined, date >= "1983-06-09" & date <= "1987-06-10")
Thatcher3 <- subset(afinn_combined, date >= "1987-06-11" & date <= "1990-11-27")
Major1 <- subset(afinn_combined, date >= "1990-11-28" & date <= "1992-04-09")
Major2 <- subset(afinn_combined, date >= "1992-04-10" & date <= "1997-05-01")
Blair1 <- subset(afinn_combined, date >= "1997-05-02" & date <= "2001-06-06")
Blair2 <- subset(afinn_combined, date >= "2001-06-07"& date <= "2005-05-04")
Blair3 <- subset(afinn_combined, date >= "2005-05-05"& date <= "2007-06-26")
Brown <- subset(afinn_combined, date >= "2007-06-27" & date <= "2010-05-10")
Cameron1 <- subset(afinn_combined, date >= "2010-05-11" & date <= "2015-05-07")
Cameron2 <- subset(afinn_combined, date >= "2015-05-08" & date <= "2016-07-12")
May <- subset(afinn_combined, date >= "2016-07-13" & date <= "2016-12-31")
allothers <- subset(afinn_combined, date < "2000-01-01")


summary(allothers)

allothers$ministry <- "Pre-Blair"
allothers$government <- "Pre-Blair"


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




test <- subset(Blair1, government==TRUE)
summary(test)


afinn_combined <- rbind(Blair1,Blair2,Blair3,Brown,Cameron1,Cameron2,May,allothers)

class(afinn_combined$year)



summary(afinn_combined)

afinn_combined$ministry <- as.factor(afinn_combined$ministry)

library(zoo)
library(data.table)

afinn_combined <- as.zoo(afinn_combined)

afinn_combined <- as.data.frame(afinn_combined)

afinn_combined$date <- as.Date(afinn_combined$date)

afinn_combined$year <- year(afinn_combined$date)

afinn_combined$year <- as.factor(afinn_combined$year)

afinn_combined$sentiment <- as.character(afinn_combined$sentiment)

afinn_combined$sentiment <- as.numeric(afinn_combined$sentiment)

summary(afinn_combined)

afinn_combined$year <- as.character(afinn_combined$year)

afinn_combined$year <- as.numeric(afinn_combined$year)

saveRDS(afinn_combined, "./data/afinn_combined.rds")




summary(afinn_combined3)

p6 <- ggplot(afinn_combined, aes(x=date, group = type, col = type))

p6 + geom_smooth(aes(y=sentiment, group = interaction(type,government),
                     linetype = government, col=type), size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  #scale_linetype_manual(values=govt_line) +
  #scale_colour_manual(values=senti_colour) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect())

summary(afinn_combined2$type)

qplot(date, sentiment, data=afinn_combined, geom='smooth', col=party_group) +  
  geom_smooth(aes(group = interaction(type,party_group)))


output$sentiplot<-renderPlot({
  
  senti_set <- sentiDataSet()
  
  p6 <- ggplot(afinn_combined, aes(x=date,y=sentiment, col = party_group))
  
  p6 + geom_smooth(aes(group = interaction(type,party_group), linetype = type, col=party_group), size=1.5, formula=y ~ log(x)) +
    scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
    scale_y_continuous(name="Sentiment Score") + 
    scale_linetype_manual(values=senti_line) +
    scale_colour_manual(values=senti_colour) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
          text = element_text(size=14),
          legend.position="bottom", legend.background = element_rect())
  
})











afinn_combined <- subset(afinn_combined, is.na(party_group)==FALSE)

p6 <- ggplot(afinn_combined, aes(x=speech_date, group = party_group, col = party_group))

p6 + geom_smooth(aes(y=sentiment, linetype = party_group, col=party_group), size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        legend.position="bottom", legend.background = element_rect())






### hansard senti

library(stringr)
library(tm)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidytext)
library(wordcloud)
library(magrittr)
library(readr)
library(zoo)
library(lubridate)
library(hms)
library(chron)
library(dtplyr)
library(reshape2)

save.image("analysis.RData")

hansard_sample <- debate[sample(nrow(debate), (nrow(debate)/20)), ]

saveRDS(hansard_sample, "hansard_sample.rds")

debate_words <- hansard_sample %>%
  select(proper_id, speakerid, date, time, speakername, speech) %>%
  unnest_tokens(word, speech) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))
debate_words


disability_words <- disability_sample %>%
  select(proper_id, speakerid, date, time, speakername, speech) %>%
  unnest_tokens(word, speech) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))
disability_words


saveRDS(debate_words, "debate_words.rds")

AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)
AFINN

BING <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(word, bing_score = score)
BING

NRC <- sentiments %>%
  filter(lexicon == "NRC") %>%
  select(word, nrc_score = score)
NRC

afinn_sentiment_all <- debate_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(Date, Name) %>%
  summarize(sentiment = mean(afinn_score))

bing_sentiment_all <- debate_words %>%
  inner_join(BING, by = "word") %>%
  group_by(Date, Name) %>%
  summarize(sentiment = mean(bing_score))

nrc_sentiment_all <- debate_words %>%
  inner_join(NRC, by = "word") %>%
  group_by(Date, Name) %>%
  summarize(sentiment = mean(nrc_score))

afinn_combined <- disability_words %>%
  inner_join(AFINN, by = "word") %>%
  group_by(Date, Name) %>%
  summarize(sentiment = mean(afinn_score))

bing_sentiment_disability <- disability_words %>%
  inner_join(BING, by = "word") %>%
  group_by(Date, Name) %>%
  summarize(sentiment = mean(bing_score))

nrc_sentiment_disability <- disability_words %>%
  inner_join(NRC, by = "word") %>%
  group_by(Date, Name) %>%
  summarize(sentiment = mean(nrc_score))


afinn_combined$sentiType <- "AFINN"

afinn_sentiment_all$sentiType <- "AFINN"

bing_sentiment_disability$sentiType <- "Bing"

bing_sentiment_all$sentiType <- "Bing"

nrc_sentiment_disability$sentiType <- "NRC"

nrc_sentiment_all$sentiType <- "NRC"



afinn_combined <- rbind(afinn_combined, afinn_sentiment_all)

bing_combined <- rbind(bing_sentiment_disability, bing_sentiment_all)

nrc_combined <- rbind(nrc_sentiment_disability, nrc_sentiment_all)

saveRDS(afinn_combined, "afinn_combined.rds")

saveRDS(bing_combined, "bing_combined.rds")

saveRDS(nrc_combined, "nrc_combined.rds")

senti_combined(afinn_combined,
               afinn_sentiment_all,
               bing_sentiment_disability,
               bing_sentiment_all,
               nrc_sentiment_disability,
               nrc_sentiment_all)


saveRDS(senti_combined, "senti_combined.rds")


else if(input$display_options == "All" & input$party_options == "Government"){
  
  senti_data <- afinn_combined[afinn_combined$year >= input$senti_year2[1]
                               & afinn_combined$year <= input$senti_year2[2]
                               & afinn_combined$type == input$debate_type
                               & afinn_combined$government == "Government",]
  
} else if(input$display_options == "All" & input$party_options == "Opposition"){
  
  senti_data <- afinn_combined[afinn_combined$year >= input$senti_year2[1]
                               & afinn_combined$year <= input$senti_year2[2]
                               & afinn_combined$type == input$debate_type
                               & afinn_combined$government == "Opposition",]
  
} else if(input$display_options != "All" & input$party_options == "Government"){
  
  senti_data <- afinn_combined[afinn_combined$year >= input$senti_year2[1]
                               & afinn_combined$year <= input$senti_year2[2]
                               & afinn_combined$type == input$debate_type
                               & afinn_combined$government == "Government",]
  
} else if(input$display_options != "All" & input$party_options == "Opposition"){
  
  senti_data <- afinn_combined[afinn_combined$year >= input$senti_year2[1]
                               & afinn_combined$year <= input$senti_year2[2]
                               & afinn_combined$type == input$debate_type
                               & afinn_combined$government == "Opposition",]
  
} else if(input$display_options != "All" & input$party_options == "Government"){
  
  senti_data <- afinn_combined[afinn_combined$year >= input$senti_year2[1]
                               & afinn_combined$year <= input$senti_year2[2]
                               & afinn_combined$type == input$debate_type
                               & afinn_combined$government == "Government",]
  
} else if(input$display_options != "All" & input$party_options == "Opposition"){
  
  senti_data <- afinn_combined[afinn_combined$year >= input$senti_year2[1]
                               & afinn_combined$year <= input$senti_year2[2]
                               & afinn_combined$type == input$debate_type
                               & afinn_combined$government == "Opposition",]
  
}

