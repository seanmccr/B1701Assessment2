# ----- B1701 Assessment Two | 202383028 | 08.11.2023 -----

# ----- 1: Libraries, Packages and Developer Tools -----
##### 1.1: Loading in libraries, installing packages and developer tools we may need: #####

if (!require("devtools")) install.packages("devtools")
devtools::install_github("jogall/soccermatics")
devtools::install_github("FCrSTATS/SBpitch")

install.packages("ggpubr")
install.packages("readxl")
library(ggplot2)
library(devtools)
library(tidyverse)
library(R.utils)
library(StatsBombR)
library(tibble)
library(janitor)
library(soccermatics)
library(SBpitch)
library(stringr)
library(corrplot)
library(rstatix)
library(fmsb)
library(readxl)
library(ggpubr)

##### 1.2: Loading In Tools, Libraries and Data #####
WWC23Data <- read.csv("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 3/R Studio Tutorial Work/ECData.csv")
head(WWC23Data)
# Loading in the Transfer Values, Wages and Club for players in the 'StrikerDataset'
SDTransferValueClub <- read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Assessment 2/Player Club, Wages and Transfer Value.xlsx")
head(SDTransferValueClub)
# Loading in Player Ages
SDPlayerAges <- read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Assessment 2/Player Ages.xlsx")
head(SDPlayerAges)
# Loading in the Atletico Madrid Womens Player Dataset
AtletiPD <- read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Assessment 2/Atletico Madrid Data.xlsx")
head(AtletiPD)

# ----- 2: Creating, Processing and Filtering Datasets  -----
##### 2.1: Changes to the 'WWC23Data' dataset #####
# Code to change the variable type to 'character'
WWC23Data$position.name <- as.character(WWC23Data$position.name)

# Changing code in the 'SDTransferValueClub' dataset, which contains information related to contract dates, transfer values, wages and clubs of players in the 'StrikerDataset'.
# Change 'Transfer Value' to numerical and replace non-numerical values with NA
SDTransferValueClub$`Transfer Value` <- as.numeric(replace(SDTransferValueClub$`Transfer Value`, SDTransferValueClub$`Transfer Value` == "Unknown", NA))# Code to change data format
SDTransferValueClub$`Salary (Yearly, Estimated)` <- as.numeric(gsub("[^0-9]", "", SDTransferValueClub$`Salary (Yearly, Estimated)`))

# Code to replace 'Unknown' with NA
SDTransferValueClub$`Contract Till`[SDTransferValueClub$`Contract Till` == "Unknown"] <- NA
SDTransferValueClub$`Salary (Yearly, Estimated)`[SDTransferValueClub$`Salary (Yearly, Estimated)` == "Unknown"] <- NA

# Convert 'Contract Till' Variable to Date
SDTransferValueClub$`Contract Till` <- as.Date(SDTransferValueClub$`Contract Till`, format = "%d.%m.%Y")

# Remove rows 73 to 100 from 'SDTransferValueClub'
SDTransferValueClub <- SDTransferValueClub[-(73:100), ]

# Renaming variables
SDPlayerAges <- SDPlayerAges %>% rename(player.name = "Player Name")

##### 2.2: Changes to the 'AtletiPD' and 'AtletiPDFiltStat' dataset #####
# Code to rename numerous variables in the AtletiPD dataset
AtletiPD <- AtletiPD %>%
  rename(
    MatchesPlayed = MP,
    Pens = PK,
    Yellow = CrdY,
    Red = CrdR,
    Gls = names(AtletiPD)[9],
    Ast = names(AtletiPD)[10],
    GA = names(AtletiPD)[11],
    npG = names(AtletiPD)[12],
    xG = names(AtletiPD)[17],
    npxG = names(AtletiPD)[18],
    xAG = names(AtletiPD)[19],
    npxGxAG = names(AtletiPD)[20],
    Gls90 = names(AtletiPD)[24],
    Ast90 = names(AtletiPD)[25],
    GA90 = names(AtletiPD)[26],
    npG90 = names(AtletiPD)[27],
    npGA90 = 'G+A-PK',
    xG90 = names(AtletiPD)[29],
    xAG90 = names(AtletiPD)[30],
    xGxAG90 = names(AtletiPD)[31],
    npxG90 = names(AtletiPD)[32],
    npxGxAG90 = names(AtletiPD)[33]
  )

# Filtering the dataset to include only the players where Position is 'FW' or 'FW,MF'
AtletiPD <- AtletiPD %>% 
  filter(Pos == "FW" | Pos == " MF,FW")

# Code to create new dataset, selecting only the variables we want for analysis
AtletiPDFiltStat <- AtletiPD[, c(1:13, 17:23, 28, 33)]

# Code to add a variable to this data set, called Non-penalty Goals and Assists (npGA)
AtletiPDFiltStat <- AtletiPDFiltStat %>%
  mutate(npGA = npG + Ast)

# Renaming variables
AtletiPD <- AtletiPD %>%
  rename(
    Goals = Gls, 
    Assists = Ast,
    TotalMinutes = Min,
    npxGA90 = npxGxAG90
  )
# Adding a new variable
AtletiPD <- AtletiPD %>%
  mutate(npGA = (Goals + Assists) - Pens)

# Repeating this task for the AtletiPDFiltStat
AtletiPDFiltStat <- AtletiPDFiltStat %>%
  rename(
    Goals = Gls, 
    Assists = Ast,
    TotalMinutes = Min,
    npxGA90 = npxGxAG90,
    npxGA = npxGxAG
  )

# Adding a new variable
AtletiPDFiltStat <- AtletiPDFiltStat %>%
  mutate(npGA = (Goals + Assists) - Pens)

# ----- 3: Code for barplots and scatterplots of Atletico Madrid Womens Player Dataset -----
##### 3.1: Comparison of various Striker metrics #####

# Barplot of Goals and Assists
# Code for a stacked Barplot of Goals and Assists
AtletiBarPlot <- ggplot(AtletiPDFiltStat, aes(x = reorder(Player, -GA))) +
  geom_bar(aes(y = Goals + Assists, fill = factor("Goals")), stat = "identity", position = "stack") +
  geom_bar(aes(y = Assists, fill = factor("Assists")), stat = "identity", position = "stack") +
  scale_y_continuous(breaks = seq(0, max(AtletiPDFiltStat$GA), by = 1)) +
  labs(x = "Player", y = "Number of Goals & Assists", title = "Atletico Madrid Women Strikers: Goals Contributions", subtitle = "2022/23 Liga F Season") +
  scale_fill_manual(values = c("Goals" = "blue", "Assists" = "red"), labels = c("Goals", "Assists")) +
  theme_light() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = NULL))
AtletiBarPlot

# Code to show the Non-penalty Goals and Assists per 90 vs the Expected Non-penalty Goals and Assists per 90
AtletinpGA90npxGGA90 <-  ggplot(AtletiPDFiltStat, aes(x = npxGA90, y = npGA90, label = Player)) +
  geom_text(check_overlap = FALSE, hjust = 0.5, vjust = -0.5) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  labs(x = "Non-penalty Expected Goals + Assists per 90 minutes (npxGA90)", 
       y = "Non-penalty Goals + Assists per 90 minutes (npGA90)", 
       title = "Player Comparison of npGA90 vs npxGA90") +
  scale_x_continuous(breaks = seq(floor(min(AtletiPDFiltStat$npGA90)), ceiling(max(AtletiPDFiltStat$npGA90)), by = 0.1)) +
  theme_minimal() +
  theme(legend.position = "right")
AtletinpGA90npxGGA90

# Code to compare the Non-Penalty Goals and Assists vs the Expected Non-Penalty Goals and Assists
AtletinpGAnpxGGA <- ggplot(AtletiPDFiltStat, aes(x = npxGA, y = npGA, label = Player)) +
  geom_text(check_overlap = FALSE, hjust = 0.5, vjust = -0.5) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  labs(x = "Non-penalty Expected Goals + Assists (npxGA)", 
       y = "Non-penalty Goals + Assists (npGA)", 
       title = "Player Comparison of npxGA vs npGA") +
  theme_minimal() +
  theme(legend.position = "right")
AtletinpGAnpxGGA

# Code comparing npGA to 90 minutes played
AtletinpGA90 <- ggplot(AtletiPDFiltStat, aes(x = `90s`, y = npGA, label = Player)) +
  geom_text(check_overlap = FALSE, hjust = 0.5, vjust = -0.5) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  labs(x = "90 Minutes Played", 
       y = "Non-penalty Goals + Assists (npGA)", 
       title = "Player Comparison of 90 Minutes Played vs npGA") +
  theme_minimal() +
  theme(legend.position = "right")
AtletinpGA90

# Code showing the relationship between Progressive Pass Received vs Made. As they are strikers, it is expected that
# they will receive more progressive passes than they make.
# Identify outliers using the interquartile range (IQR) for both progressive passes received and played.
PrgR_Q1 <- quantile(AtletiPDFiltStat$PrgR, 0.25)
PrgR_Q3 <- quantile(AtletiPDFiltStat$PrgR, 0.75)
PrgR_IQR <- IQR(AtletiPDFiltStat$PrgR)

PrgP_Q1 <- quantile(AtletiPDFiltStat$PrgP, 0.25)
PrgP_Q3 <- quantile(AtletiPDFiltStat$PrgP, 0.75)
PrgP_IQR <- IQR(AtletiPDFiltStat$PrgP)

# Define the lower and upper bounds for outliers
PrgR_Lower <- PrgR_Q1 - 1.5 * PrgR_IQR
PrgR_Upper <- PrgR_Q3 + 1.5 * PrgR_IQR
PrgP_Lower <- PrgP_Q1 - 1.5 * PrgP_IQR
PrgP_Upper <- PrgP_Q3 + 1.5 * PrgP_IQR

# Filter out the outliers before plotting
AtletiPDFiltStat <- AtletiPDFiltStat %>%
  filter(PrgR >= PrgR_Lower & PrgR <= PrgR_Upper) %>%
  filter(PrgP >= PrgP_Lower & PrgP <= PrgP_Upper)

# Scatter plot for progressive passes made and received without outliers
AtletiPrgPMR <- ggplot(AtletiPDFiltStat, aes(x = PrgR, y = PrgP, label = Player)) +
  geom_text(check_overlap = TRUE, hjust = 0.5, vjust = -0.5) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  labs(x = "Progressive Passes - Received", 
       y = "Progressive Passes - Played", 
       title = "Player Comparison of Progressive Passes Played vs Received") +
  theme_minimal() +
  theme(legend.position = "right")
AtletiPrgPMR


# ----- 4: Code to create the 'StrikerDataset' -----

# Creating our conversion functioon
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

# Selecting the 2nd, 3rd, 4th, and 5th columns from 'SDTransferValueClub'
selected_columns <- SDTransferValueClub[, 2:5]
# Combining the selected columns with 'StrikerDataset'
StrikerDataset <- cbind(StrikerDataset, selected_columns)

##### 4.1: Creating our shotmap colours, pitch and shapes ##### 
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



# Blomqvist: 59
# Haug: 69
# Ueki: 60
# Telma: 70
# Katja:


# ----- 5: Coding for Shots at Goal KPI -----

# Creating new dataset 'PlayerStats' that includes ALL players from the 'StrikerDataset'
# Only 66 players are returned as some strikers did not register any shots
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

# Code to rank and weigh the variables that evaluate our Shots at Goal KPI
FilteredSoG <- FilteredSoG %>%
  mutate(
    GoalsRnk = rank(Goals, ties.method = "min"),
    ShotsOnTargetRnk= rank(ShotsOnTarget, ties.method = "min"),
    ShotsOffTargetRnk = rank(-ShotsOffTarget, ties.method = "min"),
    TotalShotsRnk = rank(TotalShots, ties.method = "min"),
    GoalConversionRnk = rank(GoalConversion, ties.method = "min"),
  ) %>%
  mutate(
    WeightedGoalsRnk = GoalsRnk * 0.15,
    WeightedShotsOnTargetRnk = ShotsOnTargetRnk * 0.25,
    WeightedShotsOffTargetRnk = ShotsOffTargetRnk * 0.25,
    WeightedTotalShotsRnk = TotalShotsRnk * 0.05,
    WeightedGoalConversionRnk = GoalConversionRnk * 0.30,
    AggregateSoGSum = WeightedGoalsRnk + WeightedShotsOnTargetRnk + WeightedShotsOffTargetRnk + WeightedTotalShotsRnk + WeightedGoalConversionRnk
  )

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


# ----- 6: Code for Goal Contributions KPI ----- 

# Code for a table of players, goals, xG and assists
FilteredGandA <- WWC23Data %>%
  inner_join(StrikerDataset, by = c("player.name" = "PlayerName")) %>%
  filter(period != 5) %>%
  group_by(player.name) %>%
  summarize(Goals = sum(shot.outcome.name == "Goal", na.rm = TRUE),
            xG = sum(shot.statsbomb_xg, na.rm = TRUE),
            Assists = sum(pass.goal_assist, na.rm = TRUE)
  ) 
  
# Code to add ranks and weightings for each of the variables
FilteredGandA <- FilteredGandA %>%
  mutate(
    GoalsRnk = rank(Goals, ties.method = "min"),
    xGRnk = rank(xG, ties.method = "min"),
    AssistsRnk = rank(Assists, ties.method = "min"),
  ) %>%
mutate(
  WeightedGoalsRnk = GoalsRnk * 0.450,
  WeightedxGRnk = xGRnk * 0.250,
  WeightedAssistsRnk = AssistsRnk * 0.300,
  AggregateGASum = WeightedGoalsRnk + WeightedxGRnk + WeightedAssistsRnk
)


# Repeating code for creation of Goal plots per player
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


# Repeating code for creation of Assist plots for each player
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

# ----- 7: Code for Pass success rates and locations KPI -----

# Create a dataset called 'WWC23PassesFiltered', and filter for when 'type.name' is 'Pass'
WWC23PassesFiltered <- WWC23Data %>%
  filter(type.name == "Pass")

# Code to filter, measure length and group passes by player name, covering several metrics
# along with the code to rank and then weigh these variables for our passing KPI
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
  ungroup() %>%
  mutate(
  TotalPassesAttemptedRnk = rank(TotalPassesAttempted, ties.method = "min"),
  TotalPassesCompletedRnk = rank(TotalPassesCompleted, ties.method = "min"),
  TPCompPercRnk = rank(TPCompPerc, ties.method = "min"),
  ShortPassRnk = rank(ShortPass, ties.method = "min"),
  SPCompRnk = rank(SPComp, ties.method = "min"),
  SPCompPercRnk = rank(SPCompPerc, ties.method = "min"),
  MediumPassRnk = rank(MediumPass, ties.method = "min"),
  MPCompRnk = rank(MPComp, ties.method = "min"),
  MPCompPercRnk = rank(MPCompPerc, ties.method = "min")
) %>%
  mutate(
    WeightedTotalPassesAttemptedRnk = TotalPassesAttemptedRnk * 0.10,
    WeightedTPCompPercRnk = TPCompPercRnk * 0.25,
    WeightedShortPassRnk = ShortPassRnk * 0.075,
    WeightedSPCompPercRnk = SPCompPercRnk * 0.25,
    WeightedMediumPassRnk = MediumPassRnk * 0.075,
    WeightedMPCompPercRnk = MPCompPercRnk * 0.25,
    AggregatePassSum = WeightedTPCompPercRnk + WeightedSPCompPercRnk + WeightedMPCompPercRnk + WeightedTotalPassesAttemptedRnk + WeightedShortPassRnk + WeightedMediumPassRnk
  )
 

# ----- 8: Code for Pass type KPI -----
# Code to create a dataset for the 'StrikersDataset' of the number and type of passes made with a designated type, along with weightsings
# and rankings for the overall KPI:
FilteredPassType <- WWC23PassesFiltered %>%
  inner_join(StrikerDataset, by = c("player.name" = "PlayerName")) %>%
  group_by(player.name) %>%
  summarize(
    ShotAssist = sum(pass.shot_assist, na.rm = TRUE),
    Cross = sum(pass.cross, na.rm = TRUE),
    ThroughBall = sum(pass.through_ball, na.rm = TRUE),
    Switch = sum(pass.switch, na.rm = TRUE),
    Cutback = sum(pass.cut_back, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    ShotAssistRnk = rank(ShotAssist, ties.method = "min"),
    ThroughBallRnk = rank(ThroughBall, ties.method = "min"),
    CutbackRnk = rank(Cutback, ties.method = "min"),
    AggregatePTSum = ShotAssistRnk + ThroughBallRnk + CutbackRnk
  ) %>%
  mutate(
    WeightedShotAssistRnk = ShotAssistRnk * 0.4,
    WeightedThroughBallRnk = ThroughBallRnk * 0.4,
    WeightedCutbackRnk = CutbackRnk * 0.2,
    AggregatePassTypeSum = WeightedShotAssistRnk + WeightedThroughBallRnk + WeightedCutbackRnk
  )


# Code for passes, broken into successful and unsuccessful, by individual players
createPitchPlot <- function(player_name, passesData, pitchFunction) {
  playerPasses <- passesData %>%
    filter(player.name == player_name & type.name == "Pass") %>%
    mutate(
      pass_successful = is.na(pass.outcome.name), # If 'pass.outcome.name' is NA, it's a successful pass
      pass_type = case_when(
        pass_successful ~ "Successful",
        TRUE ~ "Unsuccessful"
      )
    )
  
  pitchFunction() +
    geom_segment(data = playerPasses, aes(x = location.x, y = location.y,
                                          xend = pass.end_location.x, yend = pass.end_location.y, color = pass_type),
                 lineend = "round", size = 0.3, arrow = arrow(length = unit(0.03, "inches"))) +
    scale_color_manual(values = c("Successful" = "black", "Unsuccessful" = "red"), name = "Pass Outcome") +
    labs(title = paste("Passes by", player_name), subtitle = "WWC23") +
    scale_y_reverse() +
    coord_fixed(ratio = 80/120)
}

# Apply createPitchPlot for each player in StrikerDataset
playerNames <- unique(StrikerDataset$player.name)

# List to store each player's plot
SDPassPlots <- list()

for (player in playerNames) {
  SDPassPlots[[player]] <- createPitchPlot(player, WWC23PassesFiltered, create_Pitch)
}



# Code for the type of passes made, coloured for the different types
# Function to create passes type plots for each player
createPassTypesPlot <- function(player_name, passesData, pitchFunction) {
  playerPasses <- passesData %>%
    filter(player.name == player_name & type.name == "Pass") %>%
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
  
  pitchFunction() +
    geom_segment(data = playerPasses, aes(x = location.x, y = location.y,
                                          xend = pass.end_location.x, yend = pass.end_location.y,
                                          color = pass_type), 
                 lineend = "round", size = 0.3,
                 arrow = arrow(length = unit(0.03, "inches"))) +
    scale_color_manual(values = c("Goal Assist" = "green", "Shot Assist" = "blue",
                                  "Cross" = "purple", "Through ball" = "orange",
                                  "Switch" = "yellow", "Cut Back" = "red",
                                  "Other" = "grey50"), 
                       name = "Pass Type") +
    labs(title = paste("All Passes by Type for", player_name), subtitle = "WWC23") +
    scale_y_reverse() +
    coord_fixed(ratio = 80/120)
}

# Apply createPassTypesPlot for each player in StrikerDataset
playerNames <- unique(StrikerDataset$player.name)

# List to store each player's pass type plot
SDPassTypesPlots <- list()

for (player in playerNames) {
  SDPassTypesPlots[[player]] <- createPassTypesPlot(player, WWC23PassesFiltered, create_Pitch)
}


# Code to create a plot of the density of passes made 
# Function to create pass density plots for each player
createPassDensityPlot <- function(player_name, passesData, pitchFunction) {
  playerPassesDensity <- passesData %>%
    filter(player.name == player_name & type.name == "Pass")
  
  playerPassesDensitymap <- pitchFunction() + 
    stat_density_2d(
      data = playerPassesDensity,
      aes(x = location.x, y = location.y, fill = ..level.., alpha = ..level..),
      geom = "polygon",
      bins = 50
    ) +
    scale_fill_viridis_c(name = "Pass Density") +
    scale_alpha(range = c(0.1, 0.7), guide = FALSE) +
    scale_y_reverse() +
    coord_fixed(ratio = 80 / 120) +
    labs(title = paste("Pass Density Heatmap for ", player_name, sep = ""), subtitle = "WWC23")
  
  return(playerPassesDensitymap)
}

# Assuming StrikerDataset and WWC23Data are already loaded in the environment
# Apply createPassDensityPlot for each player in StrikerDataset
playerNames <- unique(StrikerDataset$player.name)

# List to store each player's pass density plot
SDPassDensityPlots <- list()

for (player in playerNames) {
  SDPassDensityPlots[[player]] <- createPassDensityPlot(player, WWC23PassesFiltered, create_Pitch)
}



# ----- 9: Code for Penalty Box Shots KPI -----

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
# Code to add my ranked and weighted variables
FilteredPBoxShot <- FilteredPBoxShot %>%
  mutate(
    PenaltyBoxGoalsRnk = rank(PenaltyBoxGoals, ties.method = "min"),
    TotalGoalsRnk = rank(TotalGoals, ties.method = "min"),
    PenaltyBoxConversionRateRnk = rank(PenaltyBoxConversionRate, ties.method = "min"),
    OverallConversionRateRnk = rank(OverallConversionRate, ties.method = "min"),
  ) %>%
  mutate(
    WeightedPenaltyBoxGoalsRnk = PenaltyBoxGoalsRnk * 0.35,
    WeightedTotalGoalsRnk = TotalGoalsRnk * 0.15,
    WeightedPenaltyBoxConversionRateRnk = PenaltyBoxConversionRateRnk * 0.35,
    WeightedOverallConversionRateRnk = OverallConversionRateRnk * 0.15,
    AggregatePBoxShotSum = WeightedPenaltyBoxGoalsRnk + WeightedTotalGoalsRnk + WeightedPenaltyBoxConversionRateRnk + WeightedOverallConversionRateRnk
  )


# Repeating code for penalty box shots
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




# ----- 10: Code to create finalised 'StrikerDataset' with KPIs, rankings, transfer information, domestic club, age and contract information where available -----
# Creating a new dataset with selected variables from multiple datasets
# Renaming 'PlayerName' column to 'player.name' in 'StrikerDataset'
StrikerDataset <- StrikerDataset %>% rename(player.name = PlayerName)

# Perform joins based on PlayerName to ensure matching rows across datasets
FilteredFinalSD <- StrikerDataset %>%
  left_join(FilteredGandA, by = "player.name") %>%
  left_join(FilteredSoG, by = "player.name") %>%
  left_join(FilteredPasses, by = "player.name") %>%
  left_join(FilteredPassType, by = "player.name") %>%
  left_join(FilteredPBoxShot, by = "player.name") %>%
  select(player.name, FiltGandASum = AggregateGASum, FiltSatGSum = AggregateSoGSum,
         FiltPassSum = AggregatePassSum, FiltPassTypeSum = AggregatePassTypeSum,
         FiltPBoxShotSum = AggregatePBoxShotSum) %>%
  tibble()

# Code to create the FilteredFinalSD dataset, where we then apply the weightings for each of the KPIs, determined by assessed relevance to the play style, position and needs of the time
FilteredFinalSD <- FilteredFinalSD %>%
  mutate(
    WeightedFiltGandASum = FiltGandASum * 0.3,
    WeightedFiltSatGSum = FiltSatGSum * 0.25,
    WeightedFiltPassSum = FiltPassSum * 0.15,
    WeightedFiltPassTypeSum = FiltPassTypeSum * 0.05,
    WeightedFiltPBoxShotSum = FiltPBoxShotSum * 0.25,
    FinalAggregateScore = WeightedFiltGandASum + WeightedFiltSatGSum + WeightedFiltPassSum + WeightedFiltPassTypeSum + WeightedFiltPBoxShotSum
  )

# Code to add 'FinalAggregateScore' to the StrikerDataset
StrikerDataset <- StrikerDataset %>%
  left_join(FilteredFinalSD %>% select(player.name, FinalAggregateScore), by = "player.name")

# Code to check the number of rows match
# Verifying that the number of rows in both datasets is the same
if (nrow(StrikerDataset) == nrow(SDPlayerAges)) {
  StrikerDataset$Age <- SDPlayerAges$Age
  
} else {
  stop("The number of rows in StrikerDataset and SDPlayerAges does not match.")
}


# ----- 11: Code to filter the finalised 'StrikerDataset' for our relevant conditions -----
# Code to filter the database for our set conditions; 28 or under, 120000 or under in value.
StrikerDatasetConditions <- StrikerDataset %>%
  filter(Age <= 28, (`Transfer Value` <= 120000 | is.na(`Transfer Value`)), !is.na(FinalAggregateScore))

# Sort StrikerDatasetConditions by 'FinalAggregateScore', and filter so it only includes the top 10 players by score
Top10Players <- StrikerDatasetConditions %>%
  arrange(-FinalAggregateScore) %>%
  slice(1:10)


# ----- 12: Code to create the minutes and matches played for each player ----- 
# Create a dataset called 'MatchesPlayed'
MatchesPlayed <- WWC23Data %>%
  group_by(player.name) %>%
  summarize(NumMatchesPlayed = n_distinct(match_id)) %>%
  ungroup()

# Calculate total minutes played for each player
TotalMinutesPlayed <- WWC23Data %>%
  group_by(match_id, player.name) %>%
  summarize(
    StartMinute = min(minute + second / 60, na.rm = TRUE),
    EndMinute = max(minute + second / 60, na.rm = TRUE)
  ) %>%
  group_by(player.name) %>%
  summarize(TotalMinutes = sum(EndMinute - StartMinute, na.rm = TRUE)) %>%
  ungroup()

# Combining both datasets
MatchesPlayed <- left_join(MatchesPlayed, TotalMinutesPlayed, by = "player.name")
#Filter out player names that are 'NA'
MatchesMinutesPlayed <- MatchesPlayed %>%
  filter(!is.na(player.name))

# Filter 'MatchesMinutesPlayed' to only include players present in 'StrikerDataset'
MatchesMinutesPlayed <- MatchesMinutesPlayed %>%
  semi_join(StrikerDataset, by = "player.name")


# ----- 13: Code to compare npGA90 against it's 'expected'/x equivalent for the top 5 selected players & the current Atletico Madrid forwards.
# Code to create the 'Top5AtletiComparison' dataset
Top5AtletiComparison <- MatchesMinutesPlayed %>%
  semi_join(Top10Players, by = "player.name") %>%
  left_join(FilteredGandA %>% select(player.name, Goals, xG, Assists), by = "player.name") %>%
  select(player.name, TotalMinutes, Goals, xG, Assists)

# Creating a new variable
Top5AtletiComparison <- Top5AtletiComparison %>%
  mutate(npG90 = ifelse(Goals > 0, (Goals / (TotalMinutes / 90)), 0))

# Create new variables 'npGA' and 'npGA90' in the 'Top5AtletiComparison' dataset
Top5AtletiComparison <- Top5AtletiComparison %>%
  mutate(
    npGA = Goals + Assists,
    npGA90 = ifelse(npGA > 0, (npGA / (TotalMinutes / 90)), 0)
  )
# New variables
Top5AtletiComparison <- Top5AtletiComparison %>%
  mutate(
    npxG90 = xG / (TotalMinutes / 90),
    npxGA90 = (xG + Assists) / (TotalMinutes / 90)
  ) 
# Renaming and filtering our identified 5 players
Top5AtletiComparisons2 <- Top5AtletiComparison %>%
  rename(Player = player.name) %>%
  filter(Player %in% c("Telma Raquel Velosa Encarnação", "Riko Ueki", "Sophie Roman Haug", "Rebecka Blomqvist", "Katja Snoeijs"))

# Code to find the common variable names in both datasets
common_vars <- intersect(names(Top5AtletiComparisons2), names(AtletiPD))

# Filter the AtletiPD dataset to only include the common variables
AtletiPD_filtered <- AtletiPD[, common_vars]

# Combine the datasets, including only the common variables
# Using rbind() since we are combining rows and we have made sure the columns match
Top5AtletiComparisonsFinal <- rbind(Top5AtletiComparisons2, AtletiPD_filtered)

# Code to create a scatterplot combining top 5 players with current players Atletico Madrid players
Top5AtletiCompFinalnpxGA90 <-  ggplot(Top5AtletiComparisonsFinal, aes(x = npxGA90, y = npGA90, label = Player)) +
  geom_text(check_overlap = FALSE, hjust = 0.5, vjust = -0.5) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  labs(x = "Non-penalty Expected Goals + Assists per 90 minutes (npxGA90)", 
       y = "Non-penalty Goals + Assists per 90 minutes (npGA90)", 
       title = "Player Comparison of npGA90 vs npxGA90") +
  scale_x_continuous(breaks = seq(floor(min(Top5AtletiComparisonsFinal$npGA90)), ceiling(max(Top5AtletiComparisonsFinal$npGA90)), by = 0.1)) +
  theme_minimal() +
  theme(legend.position = "right")
Top5AtletiCompFinalnpxGA90


# ----- 13: Code to run correlation of the 5 KPI's used to assess potential targets ----- 
# Correlation matrix code
FiltKPICorrelation <-cor(FilteredFinalSD[,c(2:6)],use="complete.obs")
corrplot(FiltKPICorrelation, method="color")

FiltKPICorr1 <- FilteredFinalSD %>% cor_test(FiltPBoxShotSum, FiltGandASum)
print(FiltKPICorr1)


# ----- 14: Code to create a radar plot of the 5 KPI scores for our identified 5 players -----
# Code to select our top 5 players from the FilteredFinalSD dataset
FilteredFinalSDTop5 <- FilteredFinalSD %>%
  semi_join(Top5AtletiComparisons2, by = c("player.name" = "Player"))

# Define the columns to normalize
columns_to_normalize <- c("FiltGandASum","FiltSatGSum","FiltPassSum","FiltPassTypeSum","FiltPBoxShotSum")

# Compute the maximum values for the specified columns
max_values <- apply(FilteredFinalSDTop5[,columns_to_normalize], 2, max,na.rm=TRUE)

# Normalize the specified columns (column values / max_values)
normalized_columns <- sweep(FilteredFinalSDTop5[, columns_to_normalize], 2, max_values, "/")
colnames(normalized_columns) <- paste('Norm', colnames(normalized_columns), sep = '_')

# Add the normalized columns back to the TopCyclist data frame
FilteredFinalSDTop5 <- cbind(FilteredFinalSDTop5, normalized_columns)



radar_data <- FilteredFinalSDTop5[,c("Norm_FiltGandASum", "Norm_FiltSatGSum", "Norm_FiltPassSum", "Norm_FiltPassTypeSum", "Norm_FiltPBoxShotSum", "player.name")]


radar_data <- data.frame(FiltGandASum = c(1, 0, radar_data$Norm_FiltGandASum),
                         FiltSatGSum = c(1, 0, radar_data$Norm_FiltSatGSum),
                         FiltPassSum = c(1, 0, radar_data$Norm_FiltPassSum),
                         FiltPassTypeSum = c(1, 0, radar_data$Norm_FiltPassTypeSum),
                         FiltPBoxShotSum = c(1,0, radar_data$Norm_FiltPBoxShotSum),
                         row.names=c("max","min",as.character(radar_data$player.name)))

radar_plot <- radarchart(
  radar_data)

# Code to add our legend in
legend(1.1,0,
       legend=rownames(radar_data)[3:nrow(radar_data)],
       col=1:(nrow(radar_data)-2),
       cex=0.8,
       lwd=3)



