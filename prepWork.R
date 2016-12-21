library(lubridate)

all_zoo_gg$Year <- year(all_zoo_gg$Date)
names(all_zoo_gg)[2] <- "Term"



saveRDS(all_zoo_gg, "disability_hansard.rds")


