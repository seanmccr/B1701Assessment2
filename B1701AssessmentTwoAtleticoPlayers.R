# ----- B1701 Assessment Two | 28.11.2023 -----

# ----- Loading In Tools, Libraries and Data -----
# Loading in the Atletico Madrid Womens Player Dataset
AtletiPD <- read_xlsx("/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Assessment 2/Atletico Madrid Data.xlsx")
head(AtletiPD)

# Loading in libraries and installing developer tools we may need:

install.packages("readxl")
if (!require("devtools")) install.packages("devtools")

devtools::install_github("jogall/soccermatics")
devtools::install_github("FCrSTATS/SBpitch")

install.packages("ggpubr")

library(ggplot2)
library(devtools)
library(tidyverse)
library(R.utils)
library(StatsBombR)
library(tibble)
library(janitor)
library(soccermatics)
library(SBpitch)
library(readxl)
library(ggpubr)

# ----- Creating, Processing and Filtering Dataset -----

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


# ----- Comparisons of Strikers -----

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
# they will receive more progressive passes than they make
# Code for Progressive Passes Received vs Made, with outliers eliminated (Banini, also a midfielder)
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









