

library(reshape2)


disability_with_sample <- melt(disability_with_sample, id.vars=c('Date','Year'))

disability_with_sample <-data.frame(disability_with_sample,stringsAsFactors = FALSE)


summary(disability_with_sample)

names(disability_with_sample)[3] <- "Type"

disability_with_sample$Year <- as.character(disability_with_sample$Year)
disability_with_sample$Year <- as.numeric(disability_with_sample$Year)


names(disability_with_sample)[4] <- "Sentiment"


disability_with_sample$Year <- as.numeric(disability_with_sample$Year)


## Recoding disability_with_sample$variable into disability_with_sample$variable
disability_with_sample$variable <- as.character(disability_with_sample$variable)
disability_with_sample$variable[disability_with_sample$variable == "debate_agg"] <- "All Debate"
disability_with_sample$variable[disability_with_sample$variable == "dis_hans_agg"] <- "Disability"


disability_with_sample$variable <- as.factor(disability_with_sample$variable)

saveRDS(disability_with_sample,'./data/disability_with_sample.rds')
