library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records
drivers <- tuesdata$drivers

View(records)
View(drivers)

fastest_times <- records %>% group_by(track, type) %>% 
  slice(which.min(time))

luigi_times <- records %>% filter(track == "Luigi Raceway")
luigi_times <- luigi_times[0:50,c(4,8)]
luigi_times_only <- luigi_times$time
empty_list = 1

for(i in 1:50)
{
  print(luigi_times_only[i] - luigi_times_only[i+1])
}

luigi_times_only[1] - luigi_times_only[2]
