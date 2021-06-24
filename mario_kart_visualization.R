# ---- DATA FROM TIDY TUESDAY! AVAIL HERE: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md
setwd("C:/Users/Work/Desktop/RStudio_viz/mario_kart_viz")
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(patchwork)
font_add("Mesmerize", "C:/Users/Work/Downloads/mesmerize-sc-bk.ttf")
showtext_auto()


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
luigi_times$time_diff = 0


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

records$time_diff = 0
for(i in 1:nrow(records))
{
    if
    (i == 1){
      records$time_diff[i] <- 0 
    }
  else {
    records$time_diff[i] <- records$time[i] - records$time[i+-1]
  }
}
records$time_diff <- records$time_diff * -1

tot_decreases <- ggplot(data = records, aes(x=time_diff)) +
  geom_histogram() +
  theme_minimal(base_family = "Mesmerize") +
  labs(
    title = "Dist. of Time Saves",
    subtitle = "Unsurprisingly, most records are broken by less than 1 second.
    There are a few notable exceptions, though!",
    x = "Time Save (in seconds)",
    y = "# Runs"
  )

tot_decreases

# ---- run a loop that creates a column with time differences between runs.
# ---- make sure to account for the differences in categories. used if
# ---- to check if dummy is the same as the dummy before it. if it is, cool.
# ---- if not, send a 0.

for(i in 1:nrow(luigi_times))
{
  if
    (i == 1){
      luigi_times$time_diff[i] <- 0 
    }
  else if
    (luigi_times$dummy[i] != luigi_times$dummy[i-1]){
      luigi_times$time_diff[i] <- 0
    }
  else {
    luigi_times$time_diff[i] <- luigi_times$time[i] - luigi_times$time[i+-1]
  }
}

luigi_times$time_diff <- luigi_times$time_diff * -1

# ---- THEME PULLED FROM @EMILMALTA on twitter! i hope it's not impolite to use other people's themes. 
theme_update(
  plot.title.position = "plot",
  plot.title = element_text(size = 48),
  plot.subtitle = element_text(margin = margin(b = 20)),
  plot.caption.position = "plot",
  plot.background = element_rect("#eae7e6", color = NA),
  plot.margin = margin(20,20,20,20),
  legend.position = "none",
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "#dfdfdf", color = "white"),
  axis.title.x = element_text(size = 24, margin = margin(20, b = 20))
)

luigi_logo <- png::readPNG('luigi_logo3.png')
luigi_logo <-  grid::rasterGrob(luigi_logo, interpolate=TRUE)

time_progression <- ggplot(data = records[1:50,], aes(x=date,
                                                      y=time,
                                                      color = time)) +
  annotation_custom(grob = luigi_logo, ymin = 124, ymax = 132) +
  scale_color_gradient(high = "#296a29", low = "#8cef95", 
                       labels = c("2:13","1:57"),
                       breaks = c(132, 117.8)) +
  geom_point(shape = 21, size = 3, stroke = 1) + 
  geom_line(size = 1) + 
  theme_minimal(base_family = "Mesmerize") + 
  theme(text = element_text(size=42))+
  labs(
    title = "Mario Kart 64 Luigi Raceway",
    subtitle = "3 Lap Glitchless Speedrun Progression. 
    Current record is Dan's 1:57.77, set on Nov. 6, 2020!",
    legend = element_blank(),
    y = "Time",
    x = "Date of Record Break") +
  scale_y_continuous(breaks=c(130,125,120,117.5),
                     labels=c("2:10", "2:05", "2:00", "1:57")) 

time_progression

ggsave(here::here("luigi_1.png"), device = "png", 
       type = "cairo",dpi = 300)


decrease_distribution <- ggplot(data = luigi_times, aes(x=time_diff)) +
  geom_histogram(binwidth = .4, fill = "#296a29", color = "#296a29") +
  theme_minimal(base_family = "Mesmerize") +
  theme(text = element_text(size=42)) +
  labs(
    title = "Distribution of Time Saves on Luigi Raceway",
    subtitle = "Unsurprisingly, most records are broken by less than 1 second.
    There are a few notable exceptions, though!",
    x = "Time Save (in seconds)",
    y = "# Runs"
  )

decrease_distribution

ggsave(here::here("luigi_2.png"), device = "png", 
       type = "cairo",dpi = 300)


new_df <- aggregate(record_duration ~ player, luigi_times, sum)
new_df <- new_df %>% arrange(-record_duration)
new_df$player <- factor(new_df$player, levels = new_df$player)

top_5 <- ggplot(data = new_df[1:5,], aes(x=player, y = record_duration,  fill = record_duration)) +
  theme_minimal(base_family = "Mesmerize") +
  theme(text = element_text(size=42)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(high = "#296a29", low = "#8cef95",
                      labels = c("9000","3300"),
                      breaks = c(9000, 3300)) +
  labs(
    title = "Cumulative record-holding days by player.",
    subtitle = "These 5 players, when combined, have held records for 28903 days!",
    x = 'Player Name',
    y = "# Days Record Held, Total"
  )

top_5

ggsave(here::here("luigi_3.png"), device = "png", 
       type = "cairo",dpi = 300)
