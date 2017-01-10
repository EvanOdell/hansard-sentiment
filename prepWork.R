
library(lubridate)
library(reshape2)
senti_combined <- readRDS("senti_combined.rds")

summary(senti_combined)

senti_combined$year <- year(senti_combined$date)

senti_combined$type <- as.factor(senti_combined$type)

senti_combined$party_group <- as.factor(senti_combined$party_group)

senti_combined <- melt(senti_combined, id.vars = c("date", "year", "party_group", "type"))

senti_combined <- data.frame(senti_combined, stringsAsFactors = FALSE)

summary(senti_combined)

senti_combined$year <- as.character(senti_combined$year)
senti_combined$year <- as.numeric(senti_combined$year)


names(senti_combined)[6] <- "sentiment"

#senti_combined <- subset(senti_combined, is.na(party_group)==FALSE)


Baldwin2 <- subset(senti_combined, date >= "1935-11-14" & date <= "1937-05-28")
Chamberlain1 <- subset(senti_combined, date >= "1937-05-28" & date <= "1939-09-03")
Chamberlain2 <- subset(senti_combined, date >= "1939-09-03" & date <= "1940-05-10")
Churchill1 <- subset(senti_combined, date >= "1940-05-10" & date <= "1945-05-22")
Churchill2 <- subset(senti_combined, date >= "1945-05-23" & date <= "1945-06-25")
Atlee1 <- subset(senti_combined, date >= "1945-06-26" & date <= "1950-02-22")
Atlee2 <- subset(senti_combined, date >= "1950-02-23" & date <= "1951-10-26")
Churchill3 <- subset(senti_combined, date >= "1951-10-26" & date <= "1955-04-05")
Eden1 <- subset(senti_combined, date >= "1955-04-06" & date <= "1955-05-28")
Eden2 <- subset(senti_combined, date >= "1955-05-29" & date <= "1957-01-10")
Macmillan1 <- subset(senti_combined, date >= "1957-01-10" & date <= "1959-10-08")
Macmillan2 <- subset(senti_combined, date >= "1959-10-09" & date <= "1963-10-18")
DouglasHome <- subset(senti_combined, date >= "1963-10-19" & date <= "1964-10-15")
Wilson1 <- subset(senti_combined, date >= "1964-10-16" & date <= "1966-03-31")
Wilson2 <- subset(senti_combined, date >= "1966-04-01" & date <= "1970-06-18")
Heath <- subset(senti_combined, date >= "1970-06-19" & date <= "1974-03-03")
Wilson3 <- subset(senti_combined, date >= "1974-03-04" & date <= "1976-04-04")
Callaghan <- subset(senti_combined, date >= "1976-04-05" & date <= "1979-05-03")
Thatcher1 <- subset(senti_combined, date >= "1979-05-04" & date <= "1983-06-08")
Thatcher2 <- subset(senti_combined, date >= "1983-06-09" & date <= "1987-06-10")
Thatcher3 <- subset(senti_combined, date >= "1987-06-11" & date <= "1990-11-27")
Major1 <- subset(senti_combined, date >= "1990-11-28" & date <= "1992-04-09")
Major2 <- subset(senti_combined, date >= "1992-04-10" & date <= "1997-05-01")
Blair1 <- subset(senti_combined, date >= "1997-05-02" & date <= "2001-06-06")
Blair2 <- subset(senti_combined, date >= "2001-06-07"& date <= "2005-05-04")
Blair3 <- subset(senti_combined, date >= "2005-05-05"& date <= "2007-06-26")
Brown <- subset(senti_combined, date >= "2007-06-27" & date <= "2010-05-10")
Cameron1 <- subset(senti_combined, date >= "2010-05-11" & date <= "2015-05-07")
Cameron2 <- subset(senti_combined, date >= "2015-05-08" & date <= "2016-07-12")
May <- subset(senti_combined, date >= "2016-07-13" & date <= "2016-12-31")



summary(allothers)

allothers$ministry <- "Pre-Blair"
allothers$government <- "Pre-Blair"

  Baldwin2$ministry <- "Baldwin2"
  Baldwin2$government <- 
  
  Chamberlain1$ministry <- "Chamberlain1"
  Chamberlain1$government <- 
  
  Chamberlain2$ministry <- "Chamberlain2"
  Chamberlain2$government <- 
  
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




test <- subset(Blair1, government==TRUE)
summary(test)



senti_combined <- rbind(Blair1, Blair2, Blair3, Brown, Cameron1, Cameron2,May, 
                        Baldwin2, Chamberlain1, Chamberlain2, Churchill1, Churchill2,
                        Atlee1, Atlee2, Churchill3, Eden1, Eden2, Macmillan1, Macmillan2, 
                        DouglasHome, Wilson1, Wilson2, Heath, Wilson3, Callaghan, 
                        Thatcher1, Thatcher2, Thatcher3, Major1, Major2)


class(senti_combined$year)

summary(senti_combined)

senti_combined$ministry <- as.factor(senti_combined$ministry)




test <- subset(senti_combined, is.na(party_group)==FALSE)

all_average <- tapply(test$sentiment, list(test$type, test$year), mean)

all_average <- melt(all_average,
                    variable.name = c("type","year"), 
                    value.names = "sentiment", 
                    measure.vars = "sentiment")

party_average <- tapply(test$sentiment, list(test$type, test$party_group, test$year), mean)

party_average <- melt(party_average,
                      variable.name = c("type","party_group","year"), 
                      value.names = "sentiment", 
                      measure.vars = "sentiment")

government_average <- tapply(test$sentiment, list(test$type, test$government, test$year), mean)

government_average <- melt(government_average,
                      variable.name = c("type","government","year"), 
                      value.names = "sentiment", 
                      measure.vars = "sentiment")

summary(government_average)

write_rds(party_average, "./data/party_average.rds")

write_rds(all_average, "./data/all_average.rds")

write_rds(government_average, "./data/government_average.rds")




library(zoo)
library(data.table)

senti_combined <- as.zoo(senti_combined)

senti_combined <- as.data.frame(senti_combined)

senti_combined$date <- as.Date(senti_combined$date)

senti_combined$year <- year(senti_combined$date)

senti_combined$year <- as.factor(senti_combined$year)

senti_combined$sentiment <- as.character(senti_combined$sentiment)

senti_combined$sentiment <- as.numeric(senti_combined$sentiment)

summary(senti_combined)

senti_combined$year <- as.character(senti_combined$year)

senti_combined$year <- as.numeric(senti_combined$year)

saveRDS(senti_combined, "./data/senti_combined.rds")




summary(senti_combined3)

p6 <- ggplot(senti_combined, aes(x=date, group = type, col = type))

p6 + geom_smooth(aes(y=sentiment, group = interaction(type,government),
                     linetype = government, col=type), size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  #scale_linetype_manual(values=govt_line) +
  #scale_colour_manual(values=senti_colour) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect())

summary(senti_combined2$type)

qplot(date, sentiment, data=senti_combined, geom='smooth', col=party_group) +  
  geom_smooth(aes(group = interaction(type,party_group)))


output$sentiplot<-renderPlot({
  
  senti_set <- sentiDataSet()
  
  p6 <- ggplot(senti_combined, aes(x=date,y=sentiment, col = party_group))
  
  p6 + geom_smooth(aes(group = interaction(type,party_group), linetype = type, col=party_group), size=1.5, formula=y ~ log(x)) +
    scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
    scale_y_continuous(name="Sentiment Score") + 
    scale_linetype_manual(values=senti_line) +
    scale_colour_manual(values=senti_colour) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
          text = element_text(size=14),
          legend.position="bottom", legend.background = element_rect())
  
})











senti_combined <- subset(senti_combined, is.na(party_group)==FALSE)

p6 <- ggplot(senti_combined, aes(x=speech_date, group = party_group, col = party_group))

p6 + geom_smooth(aes(y=sentiment, linetype = party_group, col=party_group), size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        legend.position="bottom", legend.background = element_rect())



