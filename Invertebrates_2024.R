###############################################################################################

# Project: Invertebrate Biodiversity in Farmland Hedgerow Study
# Author: Mary Buford Turnage
# Date Created: 2025-03-19
# Last Modified: 2025-03-19
# Version Control: GitHub Repo: https://github.com/MB-Turnage/Invertebrates2024.git

###############################################################################################

##### Data Cleaning ######

## Load necessary packages ##
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# install.packages("janitor") # If needed #
library(janitor)

# install.packages("openxlsx") # If needed #
library(openxlsx)

## Read in the master "Inverts_2024" dataset ##
master <- readxl::read_excel("C:/UCNZ/R/Invertebrates_2024/Invertebrates_2024_Data/Inverts_2024.xlsx")

## Check that all the sheet names are correct ##
sheet_names <- getSheetNames("C:\\UCNZ\\R\\Invertebrates_2024\\Invertebrates_2024_Data\\Inverts_2024.xlsx")
print(sheet_names) # i.e, "Pitfall_Traps_Original", "Pan Traps", etc

## Read in the sheets for Pitfall Traps, Pan Traps, and Light Traps ##
pan <- read_excel("C:/UCNZ/R/Invertebrates_2024/Invertebrates_2024_Data/Inverts_2024.xlsx", sheet = "Pan Traps")
light <- read_excel("C:/UCNZ/R/Invertebrates_2024/Invertebrates_2024_Data/Inverts_2024.xlsx", sheet = "Light Traps")
pitfall <- read_excel("C:/UCNZ/R/Invertebrates_2024/Invertebrates_2024_Data/Inverts_2024.xlsx", sheet = "Pitfall_Traps_Original ")

## Clean column names for easier reference - "Pitfall_Traps_Original " has a space in the Inverts_2024.xlxs ##
pitfall <- janitor::clean_names(pitfall)
pan <- clean_names(pan)
light <- clean_names(light)

## Replace the blanks with zeros ## 
pitfall[1:95, 6:50][is.na(pitfall[1:95, 6:50])] <- 0 
pan[1:33, 6:48][is.na(pan[1:33, 6:48])] <- 0 

## Light is in a different format, so checking if the count column is string ##
str(light)

## Converting abundance column to numeric ##
light$abundance <- as.numeric(light$abundance)

## Replacing blanks or NAs with zeros ##
light$abundance[is.na(light$abundance)] <- 0

## I could merge them into one dataset, but Pitfall and Pan traps are in a different format than Light traps, so will circle back to that ##


##### Initial Summary Statistics #####

## Light Traps ###

summary(light)

LightXLoc <- light %>% group_by(location) %>% summarize(total_count = sum(abundance))
LightXLocSpec <-light %>% group_by(location, species) %>% summarize(total_count = sum(abundance, na.rm = TRUE))
print(LightXLocSpec)

ggplot(LightXLocSpec, aes(x=species, y=total_count, fill=location))+
  geom_col(position="dodge") + # "dodge" separates bars by farm #
  labs(title = "Total Count of Insects by Farm and Species",
       x = "Species",
       y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate species labels for readability

# Lol this doesn't give much - I'm interested to see how this changes when light traps are added to the others
