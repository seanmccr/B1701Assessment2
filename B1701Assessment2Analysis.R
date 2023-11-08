# ----- B1701 Assessment Two | 08.11.2023 -----

# Loading in our dataset 

WWC23Data <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 3/R Studio Tutorial Work/ECData.csv")

# Loading in libraries we may need

library(ggplot2)
library(devtools)
library(tidyverse)
library(R.utils)
library(StatsBombR)
library(tibble)


shots_goals <- ECData %>%
  group_by(team.name) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
          goals = sum(shot.outcome.name=="Goal", na.rm = TRUE)) 

shots_goals$ConversionRate <- mutate("shots" / "goals")

