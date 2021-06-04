library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records
drivers <- tuesdata$drivers

View(records)
View(drivers)

fastest_times <- records %>% group_by(track) %>% 
  slice(which.max(time))

luigi_times <- records %>% filter(track == "Luigi Raceway")
luigi_times <- luigi_times[0:50,]
luigi_times_only <- luigi_times$time
time_jump <- for (n in luigi_times_only) {
  (n - n+1)
}

luigi_times_only[1] - luigi_times_only[2]
