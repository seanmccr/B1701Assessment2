# ----- B1701 Assessment Two | 08.11.2023 -----

# FILTERING AND PROCESSING 
# Loading in and check our dataset 

WWC23Data <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 3/R Studio Tutorial Work/ECData.csv")
head(WWC23Data)

# Loading in libraries and installing developer tools we may need:

if (!require("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics")

devtools::install_github("FCrSTATS/SBpitch")

library(ggplot2)
library(devtools)
library(tidyverse)
library(R.utils)
library(StatsBombR)
library(tibble)
library(janitor)
library(soccermatics)
library(SBpitch)


# Code to change the variable type to 'character'
WWC23Data$position.name <- as.character(WWC23Data$position.name)

# ----- CODE TO CREATE A STRIKER DATASET -----
# Create a Strikerdataset using WWC23Data
convertToSeconds <- function(minute, second) {
  minute * 60 + second
}
# Calculate the duration in seconds and determine if each position per player passes the 60 minute threshold
WWC23DataTimeFilt <- WWC23Data %>%
  group_by(player.name, position.name) %>%
  mutate(
    startSecond = convertToSeconds(min(minute), min(second)),
    endSecond = convertToSeconds(max(minute), max(second)),
    durationSeconds = endSecond - startSecond
  ) %>%
  ungroup() %>%
  mutate(
    playedOver60Minutes = durationSeconds >= (60 * 60)
  )

# Create the 'PlayerPositions' dataset, excluding positions where playedOver60Minutes is FALSE
PlayerPositions <- WWC23DataTimeFilt %>%
  filter(playedOver60Minutes) %>% # Excluding positions played less than 60 minutes
  select(player.name, position.name) %>%
  distinct() %>%
  arrange(player.name, position.name) %>%
  group_by(player.name) %>%
  mutate(position_var = paste0("Position ", row_number())) %>%
  ungroup() %>%
  spread(key = position_var, value = position.name) %>%
  rename(PlayerName = `player.name`) %>%
  select(PlayerName, `Position 1`, `Position 2`, `Position 3`, `Position 4`)

# Replace `NA` with 'NA' where positions are not available
PlayerPositions[is.na(PlayerPositions)] <- 'NA'

# Filtering the PlayerPositions dataset to include only rows 
# where at least one of the 'Position' variables contains the specified positions
StrikerPositions <- c('Striker', 'Center Forward', 'Right Center Forward', 'Left Center Forward')

StrikerDataset <- PlayerPositions %>%
  filter(if_any(starts_with("Position"), ~ .x %in% StrikerPositions))

# FILTER FOR MINUTES PLAYED >= 60. THIS ELIMINATES PLAYERS WHO DID NOT FEATURE, FEATURED MINIMALLY OR PLAYERS WHO DID NOT PLAY A PARTICULAR POSITION FOR LONG
# I.E PLAYERS BEING MOVED OUT OF POSITION (MILLIE BRIGHT AT RCF IN THE FINAL FOR 15 MINUTES)


shotmapxgcolors <- c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#f4ef95", "#FDE960", "#FCDC5F", "#F5B94D", "#F0983E", "#ED8A37", "#E66424", "#D54F1B", "#DC2608", "#BF0000", "#7F0000", "#5F0000")
shape_mapping <- c("Right Foot" = 21, "Left Foot" = 22, "Head" = 23)


# Creating our 'create_Pitch' function to design the parameters
create_Pitch <- function() {
  ggplot() +
    # Outer lines
    geom_rect(aes(xmin = 0, xmax = 120, ymin = 0, ymax = 80), color = "black", fill = NA) +
    # Halfway line
    geom_segment(aes(x = 60, y = 0, xend = 60, yend = 80), color = "black") +
    # Center circle
    annotate("path",
             x = 60 + 9.15 * cos(seq(0, 2 * pi, length.out = 100)),
             y = 40 + 9.15 * sin(seq(0, 2 * pi, length.out = 100)),
             colour = "black") +
    # Center spot
    annotate("point", x = 60, y = 40, color = "black", size = 2) +
    # Left Penalty Box
    geom_rect(aes(xmin = 0, xmax = 18, ymin = 18, ymax = 62), color = "black", fill = NA) +
    # Right Penalty Box
    geom_rect(aes(xmin = 102, xmax = 120, ymin = 18, ymax = 62), color = "black", fill = NA) +
    # Left 6-yard Box
    geom_rect(aes(xmin = 0, xmax = 6, ymin = 30, ymax = 50), color = "black", fill = NA) +
    # Right 6-yard Box
    geom_rect(aes(xmin = 114, xmax = 120, ymin = 30, ymax = 50), color = "black", fill = NA) +
    # Goal lines (with increased size for emphasis)
    geom_segment(aes(x = 0, y = 33, xend = 0, yend = 47), color = "black", size = 1.25) +
    geom_segment(aes(x = 120, y = 33, xend = 120, yend = 47), color = "black", size = 1.25) +
    theme_void() +
    theme(aspect.ratio = 80 / 120) # Keep aspect ratio consistent for a standard soccer pitch
}



# ----- CODE FOR SHOTS -----

# Creating new dataset 'PlayerStats' that includes ALL players from the 'StrikerDataset'
FilteredSoG <- WWC23Data %>%
  inner_join(StrikerDataset, by = c("player.name" = "PlayerName")) %>%
  filter(period != 5,
         type.name == "Shot") %>%
  group_by(player.name) %>%
  summarize(Goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
            ShotsOnTarget = sum(shot.outcome.name %in% c("Goal", "Saved", "Saved to Post"), na.rm = FALSE),
            ShotsOffTarget = sum(!shot.outcome.name %in% c("Goal", "Saved", "Saved to Post"), na.rm = FALSE),
            TotalShots = n(),
            GoalConversion =  Goals / TotalShots
  ) %>%
  ungroup() %>%
  select(player.name, Goals, ShotsOnTarget, ShotsOffTarget, TotalShots, GoalConversion)

# ONLY 66 PLAYERS ARE RETURNED AS SOME STRIKERS PLAYERS DID REGISTER ANY SHOTS 
# CUT OFF: TOP 25 - CHECK OFF PLAYERS WITH THEIR ACTUAL POSITIONS + VALUE AND WAGES

# Code to provide a graphic, showing where shots were taken on the pitch, the xG of the shot (represented by colour)
# and an arrow detailing the direction and length of the shot. 

PlayerShotPlots <- function(WWC23Data) {
  player_shots <-  WWC23Data[which(WWC23Data$type.name == "Shot"), ]
  
  ShotPlots <- ggplot() +
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(), line = element_blank()) + # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6, x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+ # add centre spot
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    geom_point(data = player_shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name), size = 4, alpha = 0.8) + #3
    geom_segment(data = player_shots, aes(x = location.x, y = location.y, xend = shot.end_location.x, yend = shot.end_location.y, colour = shot.statsbomb_xg), size = 1, alpha = 0.8) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption= element_text(size=13,family="serif", hjust=0.5, vjust=0.5),
          plot.subtitle = element_text(size = 18, family="serif", hjust = 0.5),
          axis.text.y = element_blank(), legend.position = "top",
          legend.title = element_text(size=22, family="serif"),
          legend.text = element_text(size=20, family="serif"),
          legend.margin = margin(c(20, 10, -85, 50)),
          legend.key.size = unit(1, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 26, family="serif", colour = "black", hjust = 0.5),
          legend.direction = "horizontal",
          axis.ticks = element_blank(), aspect.ratio = c(65/100),
          plot.background = element_rect(fill = "white"), strip.text.x = element_text(size=12,family="serif")) +
    labs(title = paste("Player Shot Map, WWC23:", unique(WWC23Data[["player.name"]]))) + #4
    scale_fill_gradientn(colors = shotmapxgcolors, limits = c(0, 1), name = "Shot xG:", 
                         breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) + #5
    scale_shape_manual(values = shape_mapping, name = "Body Part:") + #6 
    scale_colour_gradientn(colors = shotmapxgcolors, limits = c(0, 1)) +
    guides(fill = guide_colourbar(title.position = "top"), title = NULL, shape = guide_legend(override.aes = list(size = 5, fill = "black")), colour = FALSE) + #7 
    coord_flip(xlim = c(85, 125), ylim = c(0, 80)) #8
  return(ShotPlots)
}
# Plot for each player in the StrikerDataset
players <- unique(StrikerDataset$PlayerName)
players_data_list <- lapply(players, function(x) 
{ 
  subset(WWC23Data, player.name == x)
})

SDShotPlots <- lapply(players_data_list, PlayerShotPlots)


# ----- CODE FOR GOAL CONTRIBUTIONS-----

# Code for a table of players, goals, xG and assists
FilteredGandA <- WWC23Data %>%
  inner_join(StrikerDataset, by = c("player.name" = "PlayerName")) %>%
  filter(period != 5) %>%
  group_by(player.name) %>%
  summarize(Goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
            xG = sum(shot.statsbomb_xg, na.rm = TRUE),
            Assists = sum(pass.goal_assist, na.rm = TRUE)
  ) 
  
# Repeating code for creation of GOAL plots per player
PlayerGoalPlots <- function(WWC23Data) {
  player_goals <-  WWC23Data[which(WWC23Data$shot.outcome.name == "Goal"), ]
  
  GoalPlots <- ggplot() +
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(), line = element_blank()) + # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6, x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+ # add centre spot
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    geom_point(data = player_goals, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name), size = 4, alpha = 0.8) + #3
    geom_segment(data = player_goals, aes(x = location.x, y = location.y, xend = shot.end_location.x, yend = shot.end_location.y, colour = shot.statsbomb_xg), size = 1, alpha = 0.8) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption= element_text(size=13,family="serif", hjust=0.5, vjust=0.5),
          plot.subtitle = element_text(size = 18, family="serif", hjust = 0.5),
          axis.text.y = element_blank(), legend.position = "top",
          legend.title = element_text(size=22, family="serif"),
          legend.text = element_text(size=20, family="serif"),
          legend.margin = margin(c(20, 10, -85, 50)),
          legend.key.size = unit(1, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 26, family="serif", colour = "black", hjust = 0.5),
          legend.direction = "horizontal",
          axis.ticks = element_blank(), aspect.ratio = c(65/100),
          plot.background = element_rect(fill = "white"), strip.text.x = element_text(size=12,family="serif")) +
    labs(title = paste("Player Goal Map, WWC23:", unique(WWC23Data[["player.name"]]))) + #4
    scale_fill_gradientn(colors = shotmapxgcolors, limits = c(0, 1), name = "Shot xG:", 
                         breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) + #5
    scale_shape_manual(values = shape_mapping, name = "Body Part:") + #6 
    scale_colour_gradientn(colors = shotmapxgcolors, limits = c(0, 1)) +
    guides(fill = guide_colourbar(title.position = "top"), title = NULL, shape = guide_legend(override.aes = list(size = 5, fill = "black")), colour = FALSE) + #7 
    coord_flip(xlim = c(85, 125), ylim = c(0, 80)) #8
  return(GoalPlots)
}
# Plot for each player in the StrikerDataset
players <- unique(StrikerDataset$PlayerName)
players_data_list <- lapply(players, function(x) 
{ 
  subset(WWC23Data, player.name == x)
})

SDGoalPlots <- lapply(players_data_list, PlayerGoalPlots)



# Repeating code for creation of ASSIST plots per player
PlayerAssistPlots <- function(WWC23Data) {
  player_assists <-  WWC23Data[which(WWC23Data$pass.goal_assist == TRUE), ]
  
  AssistPlots <- ggplot() +
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(), line = element_blank()) + # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6, x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+ # add centre spot
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    geom_point(data = player_assists, aes(x = location.x, y = location.y, shape = pass.body_part.name), size = 4, alpha = 0.8) + #3
    geom_segment(data = player_assists, aes(x = location.x, y = location.y, xend = pass.end_location.x, yend = pass.end_location.y), size = 1, alpha = 0.8) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption= element_text(size=13,family="serif", hjust=0.5, vjust=0.5),
          plot.subtitle = element_text(size = 18, family="serif", hjust = 0.5),
          axis.text.y = element_blank(), legend.position = "top",
          legend.title = element_text(size=22, family="serif"),
          legend.text = element_text(size=20, family="serif"),
          legend.margin = margin(c(20, 10, -85, 50)),
          legend.key.size = unit(1, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 26, family="serif", colour = "black", hjust = 0.5),
          legend.direction = "horizontal",
          axis.ticks = element_blank(), aspect.ratio = c(65/100),
          plot.background = element_rect(fill = "white"), strip.text.x = element_text(size=12,family="serif")) +
    labs(title = paste("Player Assist Map, WWC23:", unique(WWC23Data[["player.name"]]))) + #5
    scale_shape_manual(values = shape_mapping, name = "Body Part:") + #6 
    guides(fill = guide_colourbar(title.position = "top"), title = NULL, shape = guide_legend(override.aes = list(size = 5, fill = "black")), colour = FALSE) + #7 
    coord_flip(xlim = c(85, 125), ylim = c(0, 80)) #8
  return(AssistPlots)
}
# Plot for each player in the StrikerDataset
players <- unique(StrikerDataset$PlayerName)
players_data_list <- lapply(players, function(x) 
{ 
  subset(WWC23Data, player.name == x)
})

SDAssistPlots <- lapply(players_data_list, PlayerAssistPlots)


# ----- CODE FOR COMMUNICATION SKILLS -----

FilteredComms <- WWC23Data %>%
  inner_join(StrikerDataset, by = c("player.name" = "PlayerName")) %>%
  group_by(player.name) %>%
  summarize(MiscommunicatedPasses = sum(pass.miscommunication, na.rm = TRUE)) %>%
  rename(PlayerName = player.name)

# ----- CODE FOR PASSES -----

# Create a dataset called 'WWC23PassesFiltered'
# where 'type.name' is 'Pass'
WWC23PassesFiltered <- WWC23Data %>%
  filter(type.name == "Pass")

# Code to filter, measure length and group passes by player name, covering several metrics
FilteredPasses <- WWC23PassesFiltered %>%
  inner_join(StrikerDataset, by = c("player.name" = "PlayerName")) %>%
  group_by(player.name) %>%
  summarize(
    TotalPassesAttempted = n(),
    TotalPassesCompleted = sum(is.na(pass.outcome.name), na.rm = TRUE),
    TPCompPerc = TotalPassesCompleted / TotalPassesAttempted * 100,
    ShortPass = sum(pass.length < 15 & type.name == "Pass", na.rm = TRUE),
    SPComp = sum(pass.length < 15 & is.na(pass.outcome.name), na.rm = TRUE),
    SPCompPerc = SPComp / ShortPass * 100,
    MediumPass = sum(pass.length >= 15 & pass.length < 30 & type.name == "Pass", na.rm = TRUE),
    MPComp = sum(pass.length >= 15 & pass.length < 30 & is.na(pass.outcome.name), na.rm = TRUE),
    MPCompPerc = MPComp / MediumPass * 100,
    LongPass = sum(pass.length >= 30 & type.name == "Pass", na.rm = TRUE),
    LPComp = sum(pass.length >= 30 & is.na(pass.outcome.name), na.rm = TRUE),
    LPCompPerc = LPComp / LongPass * 100
  ) %>%
  ungroup()



# Code for passes, broken into successful and unsuccessul
DianiPassesSucUnsuc <- WWC23PassesFiltered %>%
  filter(player.name == "Kadidiatou Diani" & type.name == "Pass") %>%
  mutate(
    pass_successful = is.na(pass.outcome.name), # If 'pass.outcome.name' is NA, it's a successful pass
    pass_type = case_when(
      pass_successful ~ "Successful",
      TRUE ~ "Unsuccessful"
    )
  )

create_Pitch() +
  geom_segment(data = DianiPassesSucUnsuc, aes(x = location.x, y = location.y,
                                       xend = pass.end_location.x, yend = pass.end_location.y, color = pass_type),
               lineend = "round", size = 0.3, arrow = arrow(length = unit(0.03, "inches"))) +
  scale_color_manual(values = c("Successful" = "black", "Unsuccessful" = "red"), name = "Pass Outcome") +
  labs(title = "Passes by Kadidiatou Diani", subtitle = "WWC23") +
  scale_y_reverse() +
  coord_fixed(ratio = 80/120)


# Code for passes made, coloured for the different types
DianiPasses <- WWC23PassesFiltered %>%
  filter(player.name == "Kadidiatou Diani" & type.name == "Pass") %>%
  mutate(
    pass_successful = if_else(is.na(pass.outcome.name), "Successful", "Unsuccessful"),
    pass_type = case_when(
      pass.goal_assist == TRUE ~ "Goal Assist",
      pass.shot_assist == TRUE ~ "Shot Assist",
      pass.cross == TRUE ~ "Cross",
      pass.through_ball == TRUE ~ "Through ball",
      pass.switch == TRUE ~ "Switch",
      pass.cut_back == TRUE ~ "Cut Back",
      TRUE ~ "Other"
    )
  )

create_Pitch() +
  geom_segment(data = DianiPasses, aes(x = location.x, y = location.y,
                                       xend = pass.end_location.x, yend = pass.end_location.y,
                                       color = pass_type), lineend = "round", size = 0.3,
               arrow = arrow(length = unit(0.03, "inches"))) +
  scale_color_manual(values = c("Goal Assist" = "green", "Shot Assist" = "blue", 
                                "Cross" = "purple", "Through ball" = "orange", "Switch" = "yellow", "Cut Back" = "red",
                                "Other" = "black"), 
                     name = "Pass Type") +
  labs(title = "Kadidiatou Diani, All Passes by Type", subtitle = "WWC23") +
  scale_y_reverse() +
  coord_fixed(ratio = 80/120)


# Code to create a plot of the density of passes made 
# Creating our dataset with the player we want
DianiPassesDensity <- WWC23Data %>%
  filter(player.name == "Kadidiatou Diani" & type.name == "Pass")
DianiPassesDensitymap <- create_Pitch() + 
  stat_density_2d(
    data = DianiPassesDensity,
    aes(x = location.x, y = location.y, fill = ..level.., alpha = ..level..),
    geom = "polygon",
    bins = 50
  ) +
  scale_fill_viridis_c(name = "Pass Density") +
  scale_alpha(range = c(0.1, 0.7), guide = FALSE) +
  scale_y_reverse() +
  coord_fixed(ratio = 80 / 120) +
  labs(title = "Pass Heatmap for Kadidiatou Diani", subtitle = "Density of Pass Locations")

DianiPassesDensitymap


# ----- CODE FOR SHOTS INSIDE THE BOX -----

# Code to determine whether the co-ordinates of a shot fall within the penalty box parameters
isShotInPenaltyBox <- function(x, y) {
  isInLeftBox <- x >= 0 & x <= 18 & y >= 18 & y <= 62
  isInRightBox <- x >= 102 & x <= 120 & y >= 18 & y <= 62
  return(isInLeftBox | isInRightBox)
}

# Code providing the shots, goals and conversion rates of a striker, along with those ONLY inside the penalty box, where they can be compared 
FilteredPBoxShot <- WWC23Data %>%
  inner_join(StrikerDataset, by = c("player.name" = "PlayerName")) %>%
  filter(type.name == "Shot", period != 5) %>%
  mutate(InPenaltyBox = mapply(isShotInPenaltyBox, location.x, location.y)) %>%
  group_by(player.name) %>%
  summarize(
    PenaltyBoxShots = sum(InPenaltyBox, na.rm = TRUE),
    TotalShots = n(),
    PenaltyBoxGoals = sum(InPenaltyBox & shot.outcome.name == "Goal", na.rm = TRUE),
    TotalGoals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
    PenaltyBoxConversionRate = PenaltyBoxGoals / PenaltyBoxShots,
    OverallConversionRate = TotalGoals / TotalShots
  ) %>%
  ungroup() %>%
  select(player.name, PenaltyBoxShots, TotalShots, PenaltyBoxGoals, TotalGoals, 
         PenaltyBoxConversionRate, OverallConversionRate)


# Repeating code to provide shot plots for each player in the StrikerDataset that is only inclusive of penalty box shots
PlayerPenaltyBoxShotPlots <- function(WWC23Data) {
  penalty_box_shots <- WWC23Data[WWC23Data$type.name == "Shot" & mapply(isShotInPenaltyBox, WWC23Data$location.x, WWC23Data$location.y), ]
  
  PenaltyBoxShotPlots <- ggplot() +
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
    theme(rect = element_blank(), line = element_blank()) + # add penalty spot right
    annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6, x=60+10*cos(seq(0,2*pi,length.out=2000)),
             y=40+10*sin(seq(0,2*pi,length.out=2000)))+ # add centre spot
    annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
    annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
             y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
    geom_point(data = penalty_box_shots, aes(x = location.x, y = location.y, fill = shot.statsbomb_xg, shape = shot.body_part.name), size = 4, alpha = 0.8) + #3
    geom_segment(data = penalty_box_shots, aes(x = location.x, y = location.y, xend = shot.end_location.x, yend = shot.end_location.y, colour = shot.statsbomb_xg), size = 1, alpha = 0.8) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption= element_text(size=13,family="serif", hjust=0.5, vjust=0.5),
          plot.subtitle = element_text(size = 18, family="serif", hjust = 0.5),
          axis.text.y = element_blank(), legend.position = "top",
          legend.title = element_text(size=22, family="serif"),
          legend.text = element_text(size=20, family="serif"),
          legend.margin = margin(c(20, 10, -85, 50)),
          legend.key.size = unit(1, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 26, family="serif", colour = "black", hjust = 0.5),
          legend.direction = "horizontal",
          axis.ticks = element_blank(), aspect.ratio = c(65/100),
          plot.background = element_rect(fill = "white"), strip.text.x = element_text(size=12,family="serif")) +
    labs(title = paste("Player Penalty Box Shot Map, WWC23:", unique(WWC23Data[["player.name"]]))) + #4
    scale_fill_gradientn(colors = shotmapxgcolors, limits = c(0, 1), name = "Shot xG:", 
                         breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) + #5
    scale_shape_manual(values = shape_mapping, name = "Body Part:") + #6 
    scale_colour_gradientn(colors = shotmapxgcolors, limits = c(0, 1)) +
    guides(fill = guide_colourbar(title.position = "top"), title = NULL, shape = guide_legend(override.aes = list(size = 5, fill = "black")), colour = FALSE) + #7 
    coord_flip(xlim = c(85, 125), ylim = c(0, 80)) #8
  return(PenaltyBoxShotPlots)
}


players <- unique(StrikerDataset$PlayerName)
players_data_list <- lapply(players, function(x) {
  subset(WWC23Data, player.name == x)
})


SDPenaltyBoxShotPlots <- lapply(players_data_list, PlayerPenaltyBoxShotPlots)












