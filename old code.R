
###OLD CODE



looks <- readRDS("./data/average_sentiment.rds")

summary(all_average)

summary(senti_combined)

senti_combined <- readRDS("./data/senti_combined.rds")

senti_set <- senti_combined[senti_combined$year >= 1937 
                             & senti_combined$year <= 2016
                             & senti_combined$senti_type == "sentiword_sentiment",]


p6 <- ggplot(senti_set, aes(x=speech_date, col = debate_type, linetype=debate_type))

p6 + geom_smooth(aes(y=sentiment, linetype = debate_type, 
                     col=debate_type), size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  scale_linetype_manual(values=debate_line) +
  scale_colour_manual(values=senti_colour) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect()) +
  guides(col=guide_legend(title=NULL),linetype=guide_legend(title=NULL))



p7 <- ggplot(senti_set, aes(x=speech_date, y=sentiment, 
                            col= interaction(debate_type, government),
                            linetype = interaction(debate_type, government)))

p7 + geom_smooth(size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "2 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  scale_linetype_manual(values=govt_type_line) +
  scale_colour_manual(values=gov_type_colour) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect()) +
  guides(col=guide_legend(title=NULL),linetype=guide_legend(title=NULL))




p8 <- ggplot(senti_combined, aes(x=year, y=sentiment, group = debate_type, col = debate_type, fill=debate_type, width=.75))

p8 + geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name="Year", breaks=seq(1935,2020, by=5)) + 
  scale_y_continuous(name="Sentiment Score") + 
  #scale_colour_manual(values=senti_colour) + 
  #scale_fill_manual(values=senti_colour) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect())

summary(government_average)


senti_data <- senti_combined[senti_combined$senti_type == "afinn_sentiment",]
                             #& senti_combined$debate_type == "All Debate",]
                             #& senti_combined$debate_type == "Disability",]
                             #& senti_combined$government == input$gov_options,]


senti_bar_all <- all_average[all_average$senti_type == "afinn_sentiment",]


p8 <- ggplot(senti_bar_all, aes(x=year, y=sentiment, group = debate_type, col = debate_type, fill=debate_type, width=.75))

p8 + geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name="Year", breaks=seq(1935,2020, by=5)) + 
  scale_y_continuous(name="Sentiment Score") + 
  scale_colour_manual(values=senti_colour) + 
  scale_fill_manual(values=senti_colour) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect())+ 
  guides(fill=guide_legend(title=NULL))



debate_line <- c("All Debate" = "dotted",
                "Disability" = "solid")

govt_line <- c("Government" = "solid",
               "Opposition" = "dotted")

govt_type_line <- c("All Debate.Government" = "dotted",
                    "All Debate.Opposition" = "dotted",
                    "Disability.Government" = "solid",
                    "Disability.Opposition" = "solid")

govt_colour <- c("Government" = "red",
               "Opposition" = "purple")

gov_type_colour <- c("All Debate.Government" = "#c65999",
                     "All Debate.Opposition" = "#7aa456",
                     "Disability.Government" = "#777acd",
                     "Disability.Opposition" = "#c96d44")

senti_colour <- c("All Debate" = "red",
                  "Disability" = "purple")

party_colour <- c("Liberal Democrat" = "#FDBB30",
                  "Conservative" = "#0087DC",
                  "Labour" = "#DC241f",
                  "Other" = "grey")


party_type_colour <- c("All Debate.Conservative" = "#cc5643",
                       "Disability.Conservative" = "#c18b40",
                       "All Debate.Labour" = "#7ea342",
                       "Disability.Labour" = "#4aab83",
                       "All Debate.Liberal Democrat" = "#698ece",
                       "Disability.Liberal Democrat" = "#8068cd",
                       "All Debate.Other" = "#c459b5",
                       "Disability.Other" = "#c45d83")

party_type_line <- c("All Debate.Conservative" = "#cc5643",
                       "Disability.Conservative" = "#c18b40",
                       "All Debate.Labour" = "#7ea342",
                       "Disability.Labour" = "#4aab83",
                       "All Debate.Liberal Democrat" = "#698ece",
                       "Disability.Liberal Democrat" = "#8068cd",
                       "All Debate.Other" = "#c459b5",
                       "Disability.Other" = "#c45d83")


ggplot(senti_data, aes(x=year, y=sentiment, fill=factor(interaction(debate_type, government)))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
 scale_colour_manual()




p8 <- ggplot(senti_data, aes(x=year, y=sentiment, group = interaction(debate_type, government),
                                 fill = interaction(debate_type, government), width=.75))

p8 + geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name="Year", breaks=seq(1935,2020, by=5)) + 
  scale_y_continuous(name="Sentiment Score") + 
  scale_fill_manual(values=gov_type_colour) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect()) + 
  guides(fill=guide_legend(title=NULL))


look2 <- subset(government_average, year >=2000)

look2$year <- as.numeric(look2$year)





class(senti_combined$year)

p8 <- ggplot(look2, aes(x=year, y=sentiment, group = interaction(debate_type, government),
                                 fill = interaction(debate_type, government), width=.75))

p8 + geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name="Year", breaks=seq(2000,2020, by=2)) + 
  scale_y_continuous(name="Sentiment Score") + 
  scale_colour_manual(values=gov_type_colour) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect()) + 
  guides(fill=guide_legend(title=NULL))

look <- subset(party_average, year >=2000)


look <- party_average[party_average$senti_type == "afinn_sentiment"
                      & party_average$year >= 2000,]
#& senti_combined$debate_type == "All Debate",]
#& senti_combined$debate_type == "Disability",]
#& senti_combined$government == input$gov_options,]

p9 <- ggplot(look, aes(x= year, y=sentiment, group = interaction(debate_type, party_group),
                             fill = interaction(debate_type, party_group), width=.75))

p9 + geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name="Year", breaks=seq(2000,2020, by=2)) +
  scale_y_continuous(name="Sentiment Score") + 
  scale_fill_manual(values=party_type_colour) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect()) + 
  guides(fill=guide_legend(title=NULL))


  
  
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()  
p + annotate("text", x = 2:5, y = 25, label = "Some text")
  
p6 <- ggplot(senti_data, aes(x=speech_date))

p6 + geom_smooth(aes(y=sentiment, group = interaction(debate_type, government),
                     linetype = debate_type, col = government), size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  
  scale_linetype_manual(values=debate_line) +
  scale_colour_manual(values=govt_colour) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect())




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



look <- subset(senti_combined, debate_type=="All Debate")

summary(look)

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
    scale_linetype_manual(values=debate_line) +
    scale_colour_manual(values=senti_colour) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
          text = element_text(size=14),
          legend.position="bottom", legend.background = element_rect())
  
})

deb_sent3 <- subset(nrc, is.na(party_group)==FALSE, )

deb_sent3$year <- lubridate::year(deb_sent3$speech_date)

deb_sent3$year <- as.character(deb_sent3$year)

deb_sent3$year <- as.factor(deb_sent3$year)

senti_combined <- subset(senti_combined, is.na(party_group)==FALSE)


library(ggplot2)

p6 <- ggplot(senti_combined, aes(x=speech_date, group = party_group, col = party_group))

p6 + geom_smooth(aes(y=sentiment, linetype = party_group, col=party_group), size=1.5, formula=y ~ log(x)) +
  scale_x_date(date_breaks = "5 year",date_labels = "%Y", name = "Date") + 
  scale_y_continuous(name="Sentiment Score") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        legend.position="bottom", legend.background = element_rect())


look2 <-look

look <- subset(look2, variable=="afinn_vector")

look$year <- as.character(look$year)

look$year <- as.factor(look$year)

p7 <- ggplot(look, aes(x=year, y = value, width=.75))

p7 + geom_boxplot(aes(fill=party_group)) +
  scale_x_discrete(name="Year", breaks=seq(1995,2020, by=5)) + 
  scale_y_continuous(name="Sentiment Score") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1), 
        text = element_text(size=14),
        legend.position="bottom", legend.background = element_rect())



afinn <- subset(senti_combined2, variable == "afinn_vector")
bing <- subset(senti_combined2, variable == "bing_vector")
nrc <- subset(senti_combined2, variable == "nrc_vector")


deb_sent3 <- cbind(afinn,bing,nrc)

g <- seq(start(deb_sent3), end(deb_sent3), by = 1)
z.na <-na.locf(deb_sent3, xout = g)
rng <- range(time(deb_sent3))

deb_sent3 <- merge(deb_sent3, zoo(, seq(rng[1], rng[2], by = 'day')))
