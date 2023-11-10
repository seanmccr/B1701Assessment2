# ----- B1701 Assessment Two | 08.11.2023 -----

# Loading in and check our dataset 

WWC23Data <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 3/R Studio Tutorial Work/ECData.csv")
head(WWC23Data)

# Loading in libraries we may need

library(ggplot2)
library(devtools)
library(tidyverse)
library(R.utils)
library(StatsBombR)
library(tibble)

# Code to change the variable type to 'character'
WWC23Data$position.name <- as.character(WWC23Data$position.name)

# Code to create filtered dataset
FilteredWWC23 <- WWC23Data %>%
  select(1:25, shot.outcome.id, shot.outcome.name, match_id) %>% # Selects the variables we want in our new dataset
  filter(position.id %in% c(22, 23, 24, 25)) %>% # Filters for the positions we want using the variable, which is the 4 striker roles
  filter(type.name == "Shot") %>% # Filters for the action we want, which is a shot
  filter(period != 5) # Removes any observations which are from a penalty shoot-out, which is represented by '5' in the 'period' variable
  

# Create a dataset using WWC23Data
PlayerPositions <- WWC23Data %>%
  select(player.name, position.name) %>%
  distinct() %>%
  group_by(player.name) %>%
  mutate(position_var = paste0("Position ", row_number())) %>%
  ungroup() %>%
  spread(key = position_var, value = position.name) %>%
  rename(PlayerName = `player.name`) %>%
  select(PlayerName, `Position 1`, `Position 2`, `Position 3`, `Position 4`, `Position 5`, 
         `Position 6`, `Position 7`)

# Replace `NA` with 'NA' where positions are not available
PlayerPositions[is.na(PlayerPositions)] <- 'NA'


# Filtering the PlayerPositions dataset to include only rows 
# where at least one of the 'Position' variables contains the specified positions
StrikerPositions <- c('Striker', 'Center Forward', 'Right Center Forward', 'Left Center Forward')

StrikerDataset <- PlayerPositions %>%
  filter(if_any(starts_with("Position"), ~ .x %in% StrikerPositions))

  
  
  

# Creating new dataset 'PlayerStats' with the specified variables
PlayerStats <- FilteredWWC23 %>%
  group_by(player.name) %>%
  summarize(Goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
            ShotsOnTarget = sum(shot.outcome.name %in% c("Goal", "Saved", "Saved to Post"), na.rm = TRUE),
            ShotsOffTarget = sum(!shot.outcome.name %in% c("Goal", "Saved", "Saved to Post"), na.rm = TRUE)
  ) %>%
  mutate(
    GoalConversion = Goals / (ShotsOnTarget + ShotsOffTarget)
  ) %>%
  rename(PlayerName = player.name) # Renaming 'player.name' to 'PlayerName'







