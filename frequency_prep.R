
library(plyr)
library(dplyr)
library(ggplot2)
library(hms)
library(zoo)
library(dtplyr)
library(scales)
library(stringr)
library(reshape2)
library(xts)
library(data.table)
library(RColorBrewer)

disability_sample_full <- readRDS("./data/disability_sample_full.rds")

names(disability_sample_full)[1] <- 'proper_id'

summary(disability_sample_full)

disability_sample_full$speakername <- as.factor(disability_sample_full$speakername)
disability_sample_full$Reason <- as.factor(disability_sample_full$Reason)

disability_sample_full$status_id <- as.factor(disability_sample_full$status_id)
disability_sample_full$CurrentStatus <- as.factor(disability_sample_full$CurrentStatus)

disability_sample_full$Name <- as.factor(disability_sample_full$Name)
disability_sample_full$MemberFrom <- as.factor(disability_sample_full$MemberFrom)

disability_sample_full$House <- as.factor(disability_sample_full$House)
disability_sample_full$party_id <- as.factor(disability_sample_full$party_id)

disability_sample_full$party <- as.factor(disability_sample_full$party)
disability_sample_full$Party_Name <- as.factor(disability_sample_full$Party_Name)

disability_sample_full$Gender <- as.factor(disability_sample_full$Gender)
disability_sample_full$DisplayAs <- as.factor(disability_sample_full$DisplayAs)

disability_sample_full$ListAs <- as.factor(disability_sample_full$ListAs)
disability_sample_full$time <- as.hms(disability_sample_full$time)
disability_sample_full$Date <- as.Date(disability_sample_full$Date)

class(disability_sample_full$date)


disability_sample_full$count_dis_person <- str_count(disability_sample_full$speech, 'disabled person') +
  str_count(disability_sample_full$speech, 'disabled person')

disability_sample_full$count_dis_child <- str_count(disability_sample_full$speech, 'disabled child')

disability_sample_full$count_dis_people_with <- str_count(disability_sample_full$speech, 'person with a disability') +
  str_count(disability_sample_full$speech, 'people with a disability') +
  str_count(disability_sample_full$speech, 'people with disabilities') +
  str_count(disability_sample_full$speech, 'person with disabilities')

disability_sample_full$count_dis_women <- str_count(disability_sample_full$speech, 'disabled women') + 
  str_count(disability_sample_full$speech, 'disabled woman')

disability_sample_full$count_dis_men <- str_count(disability_sample_full$speech, 'disabled men') + 
  str_count(disability_sample_full$speech, 'disabled man')

disability_sample_full$count_dis_child_with <- str_count(disability_sample_full$speech, 'children with disabilities') +
  str_count(disability_sample_full$speech, 'child with disabilities') +
  str_count(disability_sample_full$speech, 'children with a disability') +
  str_count(disability_sample_full$speech, 'child with a disability')

disability_sample_full$count_dis_any_with <- str_count(disability_sample_full$speech, 'with disabilities') + 
  str_count(disability_sample_full$speech, 'with a disability')

disability_sample_full$count_ind_living <- str_count(disability_sample_full$speech, 'independent living')

disability_sample_full$count_wheelchair <- str_count(disability_sample_full$speech, 'wheelchair')

disability_sample_full$count_paralympic <- str_count(disability_sample_full$speech, 'paralympic')

disability_sample_full$count_afflict <- str_count(disability_sample_full$speech, 'afflicted')

disability_sample_full$count_spastic <- str_count(disability_sample_full$speech, 'spastic')

disability_sample_full$count_sub_normal <- str_count(disability_sample_full$speech, 'sub-normal')+
str_count(disability_sample_full$speech, 'sub normal') +
str_count(disability_sample_full$speech, 'subnormal')

disability_sample_full$count_amputee <- str_count(disability_sample_full$speech, 'amputee')

disability_sample_full$count_retard <- str_count(disability_sample_full$speech, 'retard')

disability_sample_full$count_cripple <- str_count(disability_sample_full$speech, 'cripple')

disability_sample_full$count_dis_with_any_else <- disability_sample_full$count_dis_any_with - 
  (disability_sample_full$count_dis_people_with + disability_sample_full$count_dis_child_with)

disability_sample_full$count_disability <- str_count(disability_sample_full$speech, 'disability')

disability_sample_full$count_disability <- disability_sample_full$count_disability  - str_count(disability_sample_full$speech, 'with a disability')

disability_sample_full$disabled_blank <- disability_sample_full$count_dis_person + disability_sample_full$count_dis_child + 
                                  disability_sample_full$count_dis_women + disability_sample_full$count_dis_men

disability_sample_full$with_disability <- disability_sample_full$count_dis_people_with + disability_sample_full$count_dis_child_with +
                                  disability_sample_full$count_dis_with_any_else


disability_sample_full$combined_disability <- disability_sample_full$with_disability + disability_sample_full$disabled_blank

summary(disability_sample_full)

agg_data2 <- aggregate(cbind(count_disability, count_dis_person, count_dis_child,
                             count_dis_people_with, count_dis_women, count_dis_men, 
                             count_dis_child_with, count_dis_any_with, count_ind_living, 
                             count_wheelchair, count_paralympic,count_afflict, count_spastic,
                             count_sub_normal,count_amputee,count_retard,count_cripple,
                             count_dis_with_any_else,with_disability,disabled_blank,
                             combined_disability)~date, 
                       data=disability_sample_full, FUN=sum)

base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

summary(agg_data2)

##Making ZOOs
disabled_blank_zoo <- zoo(agg_data2$disabled_blank, order.by=agg_data2$date)

with_disability_zoo <- zoo(agg_data2$with_disability, order.by=agg_data2$date)

combined_disability_zoo <- zoo(agg_data2$combined_disability, order.by=agg_data2$date)

disability_zoo <- zoo(agg_data2$count_disability, order.by=agg_data2$date)

dis_person_zoo <- zoo(agg_data2$count_dis_person, order.by=agg_data2$date)

dis_child_zoo <- zoo(agg_data2$count_dis_child, order.by=agg_data2$date)

dis_people_with_zoo <- zoo(agg_data2$count_dis_people_with, order.by=agg_data2$date)

dis_women_zoo <- zoo(agg_data2$count_dis_women, order.by=agg_data2$date)

dis_men_zoo <- zoo(agg_data2$count_dis_men, order.by=agg_data2$date)

dis_child_with_zoo <- zoo(agg_data2$count_dis_child_with, order.by=agg_data2$date)

dis_any_with_zoo <- zoo(agg_data2$count_dis_any_with, order.by=agg_data2$date)

ind_living_zoo <- zoo(agg_data2$count_ind_living, order.by=agg_data2$date)

wheelchair_zoo <- zoo(agg_data2$count_wheelchair, order.by=agg_data2$date)

paralympic_zoo <- zoo(agg_data2$count_paralympic, order.by=agg_data2$date)

spastic_zoo <- zoo(agg_data2$count_spastic, order.by=agg_data2$date)

sub_normal_zoo <- zoo(agg_data2$count_sub_normal, order.by=agg_data2$date)

amputee_zoo <- zoo(agg_data2$count_amputee, order.by=agg_data2$date)

retard_zoo <- zoo(agg_data2$count_retard, order.by=agg_data2$date)

dis_with_any_else_zoo <- zoo(agg_data2$count_dis_with_any_else, order.by=agg_data2$date)

all_zoo <- cbind(disability_zoo,dis_person_zoo,dis_child_zoo,dis_people_with_zoo,
                 dis_women_zoo,dis_men_zoo ,dis_child_with_zoo,ind_living_zoo,
                 wheelchair_zoo,paralympic_zoo,spastic_zoo,
                 sub_normal_zoo,amputee_zoo,retard_zoo,
                 dis_with_any_else_zoo)


class(all_zoo)

summary(all_zoo)

g <- seq(start(all_zoo), end(all_zoo), by = 1)
z.na <-na.locf(all_zoo, xout = g)
rng <- range(time(all_zoo))

all_zoo <- merge(all_zoo, zoo(, seq(rng[1], rng[2], by = 'day')))

summary(all_zoo)

all_zoo_test <- as.data.frame(all_zoo)

setDT(all_zoo_test, keep.rownames = TRUE)[]

names(all_zoo_test)[1] <- 'Date'

all_zoo_test$Date <- as.Date(all_zoo_test$Date)

summary(all_zoo_test)

all_zoo_test$sitting<-0

all_zoo_test$sitting[all_zoo_test$Date %in% dates$Date] <- 1

all_zoo_test$sitting <- as.logical(all_zoo_test$sitting)

nrow(all_zoo_test)

sitting_yes <- subset(all_zoo_test, sitting==TRUE)

sitting_no <- subset(all_zoo_test, sitting==FALSE)

nrow(sitting_no) + nrow(sitting_yes)

summary(sitting_yes)

sitting_yes[is.na(sitting_yes)] <- 0

all_zoo_merged <- rbind(sitting_no,sitting_yes)

all_zoo_merged$sitting <-NULL

all_zoo_merged <- read.zoo(all_zoo_merged)

class(all_zoo_merged)

summary(all_zoo_merged)

all_zoo_roll <- rollapply(all_zoo_merged, 365.25, FUN=mean, na.rm=TRUE, align = 'right')

summary(all_zoo_roll)

all_zoo_gg <- as.data.frame(all_zoo_roll)

setDT(all_zoo_gg, keep.rownames = TRUE)[]

head(all_zoo_gg)

summary(all_zoo_gg)

names(all_zoo_gg)[1] <- 'Date'

all_zoo_gg <- melt(all_zoo_gg, id.vars='Date')

all_zoo_gg <-data.frame(all_zoo_gg,stringsAsFactors = FALSE)

all_zoo_gg$Date <- as.Date(all_zoo_gg$Date)

class(all_zoo_gg$variable)


fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}

all_zoo_gg$variable <- factor(all_zoo_gg$variable,
                              levels=c("disability_zoo", "dis_person_zoo", "dis_men_zoo",
                                       "dis_women_zoo", "dis_child_zoo", "dis_people_with_zoo",
                                       "dis_child_with_zoo", "dis_with_any_else_zoo",
                                       "ind_living_zoo", "wheelchair_zoo", "paralympic_zoo",
                                       "spastic_zoo", "sub_normal_zoo", "amputee_zoo",
                                       "retard_zoo"),
                              labels=c("Disability Other","Disabled Person", "Disabled Men","Disabled Women",
                                       "Disabled Children", "People With Disability",
                                       "Children With Disability", "Any With Disability",
                                       "Independent Living", "Wheelchair", "Paralympic",
                                       "Spastic", "Sub-Normal", "Amputee","Retard"))





###Best and most acurate plot
p3 <- ggplot(all_zoo_gg, aes(x=Date, group = variable, col = variable))
##Need to remove days that Parliament did not sit
##Need to drop scientific notation
p3 + geom_smooth(aes(y=value, linetype = variable, col=variable), size=1.5, formula=y ~ log(x), se=FALSE) +
  coord_cartesian(xlim = c(as.Date('1935-01-01'), as.Date('2016-11-24'))) + 
  scale_linetype_manual(values=c( 'dotdash', 'dashed', 'solid', 'dotdash', 'longdash', 'solid',
                                  'solid', 'dashed', 'dotdash', 'longdash', 'twodash',
                                  'solid', 'dashed', 'dotted', 'solid')) +
  scale_color_manual(values=c("#006109",
                               "#8a2093",
                               "#00d38d",
                               "#ff50a0",
                               "#5b5700",
                               "#7f8cff",
                               "#fa7422",
                               "#134eae",
                               "#d70039",
                               "#97bcff",
                               "#8a3a1b",
                               "#936996",
                               "#ff9176",
                               "#9b2535",
                               "#ff7798")) + 
  scale_x_date(date_breaks = '5 year',date_labels = "%Y") +
  theme_bw() +
  scale_y_continuous(trans=log_trans(5),breaks = base_breaks(),name='Average Mentions per Day (Logarithmic Scale)', labels = fmt_dcimals(3)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ggtitle('Average of Mentions of different words and & phrases in Hansard debate per annum, 1936-2016')

summary(all_zoo_gg)

saveRDS(all_zoo_gg, "all_zoo_gg.rds")


disability_vs_disabled_zoo <- cbind(disabled_blank_zoo,with_disability_zoo,combined_disability_zoo)

class(disability_vs_disabled_zoo)

summary(disability_vs_disabled_zoo)

g <- seq(start(disability_vs_disabled_zoo), end(disability_vs_disabled_zoo), by = 1)
z.na <-na.locf(disability_vs_disabled_zoo, xout = g)
rng <- range(time(disability_vs_disabled_zoo))

disability_vs_disabled_zoo <- merge(disability_vs_disabled_zoo, zoo(, seq(rng[1], rng[2], by = 'day')))

summary(disability_vs_disabled_zoo)

disability_vs_disabled_zoo_test <- as.data.frame(disability_vs_disabled_zoo)

setDT(disability_vs_disabled_zoo_test, keep.rownames = TRUE)[]

names(disability_vs_disabled_zoo_test)[1] <- 'Date'

disability_vs_disabled_zoo_test$Date <- as.Date(disability_vs_disabled_zoo_test$Date)

summary(disability_vs_disabled_zoo_test)

disability_vs_disabled_zoo_test$sitting<-0

disability_vs_disabled_zoo_test$sitting[disability_vs_disabled_zoo_test$Date %in% dates$Date] <- 1

disability_vs_disabled_zoo_test$sitting <- as.logical(disability_vs_disabled_zoo_test$sitting)

nrow(disability_vs_disabled_zoo_test)

sitting_yes <- subset(disability_vs_disabled_zoo_test, sitting==TRUE)

sitting_no <- subset(disability_vs_disabled_zoo_test, sitting==FALSE)

nrow(sitting_no) + nrow(sitting_yes)

summary(sitting_yes)

sitting_yes[is.na(sitting_yes)] <- 0

disability_vs_disabled_zoo_merged <- rbind(sitting_no,sitting_yes)

rm(sitting_no,sitting_yes)

disability_vs_disabled_zoo_merged$sitting <-NULL

disability_vs_disabled_zoo_merged <- read.zoo(disability_vs_disabled_zoo_merged)

class(disability_vs_disabled_zoo_merged)

summary(disability_vs_disabled_zoo_merged)

disability_vs_disabled_zoo_roll <- rollapply(disability_vs_disabled_zoo_merged, 365.25, FUN=mean, na.rm=TRUE, align='right')

summary(disability_vs_disabled_zoo_roll)

disability_vs_disabled_zoo_gg <- as.data.frame(disability_vs_disabled_zoo_roll)

setDT(disability_vs_disabled_zoo_gg, keep.rownames = TRUE)[]

head(disability_vs_disabled_zoo_gg)

summary(disability_vs_disabled_zoo_gg)

names(disability_vs_disabled_zoo_gg)[1] <- 'Date'

disability_vs_disabled_zoo_gg <- melt(disability_vs_disabled_zoo_gg, id.vars='Date')

disability_vs_disabled_zoo_gg <-data.frame(disability_vs_disabled_zoo_gg,stringsAsFactors = FALSE)

disability_vs_disabled_zoo_gg$Date <- as.Date(disability_vs_disabled_zoo_gg$Date)

class(disability_vs_disabled_zoo_gg$variable)

disability_vs_disabled_zoo_gg$variable <- factor(disability_vs_disabled_zoo_gg$variable,
                                                 levels=c("disabled_blank_zoo", "with_disability_zoo","combined_disability_zoo"),
                                                 labels=c("Disabled", "With Disability","Combined"))


p4 <- ggplot(disability_vs_disabled_zoo_gg, aes(x=Date, group = variable, col = variable))
##Need to remove days that Parliament did not sit
##Need to drop scientific notation
p4 + geom_smooth(aes(y=value, linetype = variable, col=variable), size=1.5, se=FALSE) +
  coord_cartesian(xlim = c(as.Date('1935-01-01'), as.Date('2016-11-24'))) + 
  scale_linetype_manual(values=c( 'solid', 'solid', 'dashed')) +
  scale_color_manual(values=c("#72426b",
                               "#3d8f00",
                               "#f12551")) + 
  scale_x_date(date_breaks = '5 year',date_labels = "%Y") +
  theme_bw() +
  scale_y_continuous(name='Average Mentions per Day') + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ggtitle('"Disabled" vs "With Disability" in Hansard debate, 1936-2016')#+
 # guides(fill = guide_legend(title = "NULL"))



summary(disability_vs_disabled_zoo_gg)



