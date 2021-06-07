# ---- DATA FROM TIDY TUESDAY! AVAIL HERE: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md

library(tidyverse)
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-05-25')
records <- tuesdata$records
drivers <- tuesdata$drivers

View(records)
View(drivers)


# ---- dataframe of the fastest times on each course! 
# ---- filtered by track and type - fastest time for each course and type
fastest_times <- records %>% group_by(track, type) %>% 
  slice(which.min(time))


# ---- filter out all courses but luigis for testing's sake, initialize
# ---- dummy variable
luigi_times <- records %>% filter(track == "Luigi Raceway")
records$dummy = 0


# ---- run thru the dataframe and assign dummies according to criteria
for(i in 1:nrow(luigi_times))
{
if(luigi_times$type[i] == "Three Lap" & luigi_times$shortcut[i] == "No"){
  luigi_times$dummy[i] = 1
} else if
  (luigi_times$type[i] == "Single Lap" & luigi_times$shortcut[i] == "No"){
    luigi_times$dummy[i] = 2
} else if
  (luigi_times$type[i] == "Three Lap" & luigi_times$shortcut[i] == "Yes"){
    luigi_times$dummy[i] = 3
  } else {
    luigi_times$dummy[i] = 4
  }
}


# ---- run a loop that creates a column with time differences between runs

for(i in 1:nrow(luigi_times))
{
  if
    (i == 1 | i == nrow(luigi_times)){
      luigi_times$time_diff[i] <-luigi_times$time[i]
    }
  else if
    (luigi_times$dummy[i] != luigi_times$dummy[i-1]){
      luigi_times$time_diff[i] <- paste(luigi_times$time[i],", START")
    }
  else if
    (luigi_times$dummy[i] != luigi_times$dummy[i+1]){
      luigi_times$time_diff[i] <- paste((luigi_times$time[i]),", FIN")
    }
  else {
    luigi_times$time_diff[i] <- luigi_times$time[i] - luigi_times$time[i+1]
  }
}

luigi_times$time_diff <- lapply(luigi_times$time_diff, round, 5)
