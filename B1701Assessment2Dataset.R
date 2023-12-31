# ----- B1701 Assessment Two | 08.11.23 -----

# ----- Install to connect StatsBombR and R Studio -----
devtools::install_github("statsbomb/StatsBombR")

# ----- Install packages and load libraries -----
install.packages("devtools")
install.packages("tidyverse")
install.packages("R.utils")
install.packages("StatsBombR")
install.packages("/Users/seanmccrone/Desktop/SDMTools_1.1-221.2.tar.gz", repos=NULL, type="source")
library(ggplot2)
library(devtools)
library(tidyverse)
library(R.utils)
library(StatsBombR)
library(tibble)

# Use this Function to see what data sets are free to use
FreeCompetitions()

# This loads in all the Free Competitions data available and assigns
# this to a tibble named 'Comp'
Comp <- (FreeCompetitions())

# This creates a variable which filters the free data to a particular set, Womens WC 2023
MEC_id <- Comp %>%
  filter(competition_gender=="female",
         season_name==2023,
         competition_international==TRUE)
print(MEC_id)

# Use this to list all the free matches within the Subset (Womens WC 2023)
Matches <- (FreeMatches(MEC_id))

# Use this to load all the free match data into our Subset (Womens WC 2023)
ECData <- free_allevents(Matches)

# Use this to clean the 'ECData' subframe, and changes the dataframe to a tibble
ECData <- as_tibble(allclean(ECData))


# Our dataframe contains lists; you cannot save lists as csv, so we will delete the list data columns (they are all location data, which we still have as individual x and y coordinates)
list_variables <- ECData %>%
  select_if(is.list)
list_variables <- c(names(list_variables))

# Code for our updated ECData Subset with all variables structured as lists removed
ECData <- subset(ECData, select = !names(ECData) %in% list_variables)

write.csv(ECData, "/Users/seanmccrone/Desktop/MASTERS DEGREE/Course Material/B1701/Week 3/R Studio Tutorial Work/ECData.csv", row.names=FALSE)



