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

for(i in 1:147)
{
if(records$type[i] == "Three Lap" & records$shortcut[i] == "No"){
  records$dummy[i] = 1
} else if
  (records$type[i] == "Single Lap" & records$shortcut[i] == "No"){
  records$dummy[i] = 2
} else if
  (records$type[i] == "Three Lap" & records$shortcut[i] == "Yes"){
    records$dummy[i] = 3
  } else {
  records$dummy[i] = 4
  }
}

luigi_times <- luigi_times[0:147,c(4,8)]
luigi_times_only <- luigi_times$time
empty_list = 1

for(i in 1:147)
{
  print(luigi_times_only[i] - luigi_times_only[i+1])
}

luigi_times_only[1] - luigi_times_only[2]
