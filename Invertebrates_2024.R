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

## Transform pan to long format ##

long_pan <- pan %>%
    tidyr::pivot_longer(cols = -c(site_name, farm, region, trap_type, habitat_type), 
               names_to = "invert_type", 
               values_to = "abundance")

## Add a columns for order, family, tribe, and genus. This code can be updated for any additional columns that need to be added without overwriting the values that have been moved ##

long_pan <- long_pan %>%
  mutate(
    phylum = if(! "phylum" %in% names(.)) NA else phylum,
    class = if (! "class" %in% names(.)) NA else class,
    superorder = if (! "superorder" %in% names(.)) NA else superorder,
    order = if (!"order" %in% names(.)) NA else order,
    family = if (!"family" %in% names(.)) NA else family,
    subfamily = if (! "subfamily" %in% names(.)) NA else subfamily,
    tribe = if (!"tribe" %in% names(.)) NA else tribe,
    genus = if (!"genus" %in% names(.)) NA else genus
  ) %>%
  relocate(phylum, .after = habitat_type) %>%
  relocate(class, .after = phylum) %>%
  relocate(superorder, .after = class) %>%
  relocate(order, .after = superorder) %>%
  relocate(family, .after = order) %>%
  relocate(subfamily, .after = family)%>%
  relocate(tribe, .after = subfamily) %>%
  relocate(genus, .after = tribe)

long_pan <- long_pan %>%                             # Renaming "invert type" to species
  rename(species = invert_type)

# Correcting invert name spelling


## Moving values from "species" into their correct taxonomic group columns ##

# Moving values to order based on their character strings #
long_pan <- long_pan %>%
  mutate(
    order = ifelse(species %in% c("coleoptera", "araneae", "orthoptera", "diptera", "hymenoptera", "lepidoptera", "opiliones", "lithobiomorpha", "stylommatophora", "neuroptera", "hemiptera", "amphipoda"), species, order),     # Move selected values
    species = ifelse(species %in% c("coleoptera", "araneae", "orthoptera", "diptera", "hymenoptera", "lepidoptera", "opiliones", "lithobiomorpha", "stylommatophora", "neuroptera", "hemiptera", "amphipoda"), "NA", species)     # Replace with "NA"
  )

# Moving values to family #
long_pan <- long_pan %>%
  mutate(
    family = ifelse(species %in% c("carabidae", "staphylinidae", "curculionidae", "latridiidae", "lynphiidae", "chrysopidae", "reduviidae"), species, family),  # Move selected values to family
    species = ifelse(species %in% c("carabidae", "staphylinidae", "curculionidae", "latridiidae", "lynphiidae", "chrysopidae", "reduviidae"), "NA", species)    # Replace with "NA"
  )

# Moving values to subfamily #
long_pan <- long_pan %>%
  mutate(
    subfamily = ifelse(species %in% c("harpalinae"), species, subfamily),
    species = ifelse(species %in% c("harpalinae"), "NA", species)
  )

# Moving values to tribe #
long_pan <- long_pan %>% 
  mutate(
    tribe = ifelse(species %in% c("stenolophini", "pentagonicini"), species, tribe),
    species = ifelse(species %in% c("stenolophini", "pentagonicini"), "NA", species)
  )

# Moving values to genus #
long_pan <- long_pan %>%
  mutate(
    genus = ifelse(species %in% c("sitona", "coccinella", "anoteropsis", "bobilla", "megaselia", "wiseana"), species, genus), 
    species = ifelse(species %in% c("sitona", "coccinella", "anoteropsis", "bobilla", "megaselia", "wiseana"), "NA", species)
  )

# Moving values to phylum and superorder #
long_pan <- long_pan %>%
  mutate(
    phylum = ifelse(species %in% c("annelid"), species, phylum),
    species = ifelse(species %in% c("annelid"), "NA", species)
  )

long_pan <- long_pan %>%
  mutate(
    superorder = ifelse(species %in% c("mite", "mites"), species, superorder),
    species = ifelse(species %in% c("mite", "mites"), "NA", species)
  )

# Filling the higher taxonomic cells #
long_pan <- long_pan %>%
  mutate(phylum = coalesce(phylum, "arthropoda"))  # filling all the NAs in with arthropoda


long_pan <- long_pan %>%
  mutate(phylum = recode(phylum, "annelid" = "annelida")) # editing annelid to be annelida


# Using case_when to make multiple condition based assignments, more flexible than #ifelse, if family is NA and [species] is a specific value, it fills the missing column with the corresponding value, if no conditions are met, it keeps the existing value

long_pan <- long_pan %>%
  mutate(
    genus = case_when(
      is.na(genus) & species == "coccinella_undecimpunctata" ~ "coccinella",
      is.na(genus) & species == "cartodere_bifasciata" ~ "cartodere",
      is.na(genus) & species == "peris_rapae" ~ "pieris",                          # peris_rapae is spelled wrong, it should be pieris_rapae
      is.na(genus) & species == "oponga_omoscopa" ~ "oponga",
      TRUE ~ genus
    ),
    
    tribe = case_when(
      is.na(tribe) & genus == "coccinella" ~ "coccinellini",
      is.na(tribe) & genus == "sitona" ~ "sitonini",
      is.na(tribe) & genus == "cartodere" ~ "cartoderini",
      is.na(tribe) & genus == "pieris" ~ "pierini",
      is.na(tribe) & genus == "bobilla" ~ "nemobiini",
      TRUE ~ tribe
    ), 
    
    subfamily = case_when(
      is.na(subfamily) & tribe == "coccinellini" ~ "coccinellinae",
      is.na(subfamily) & tribe == "sitonini" ~ "entiminae",
      is.na(subfamily) & tribe == "cartoderini" ~ "latridiinae",
      is.na(subfamily) & tribe == "pierini" ~"pierinae",
      is.na(subfamily) & genus == "oponga" ~ "hieroxestinae",
      is.na(subfamily) & tribe == "stenolophini" ~ "harpalinae",
      is.na(subfamily) & tribe == "pentagonicini" ~ "harpalinae",
      is.na(subfamily) & genus == "anoteropsis" ~ "artoriinae",
      is.na(subfamily) & tribe == "nemobiini" ~ "nemobiinae",
      is.na(subfamily) & genus == "megaselia" ~ "phorinae",
      TRUE ~ subfamily
      ),
      
    family = case_when(                                                           
      is.na(family) & subfamily == "coccinellinae" ~ "coccidellidae",                    
      is.na(family) & subfamily == "entiminae" ~ "curculionidae",
      is.na(family) & subfamily == "latridiinae"~ "latridiidae",
      is.na(family) & subfamily == "harpalinae" ~ "carabidae",
      is.na(family) & subfamily == "pierinae" ~ "pieridae",
      is.na(family) & subfamily == "hieroxestinae" ~ "tineidae",
      is.na(family) & subfamily == "artoriinae" ~ "lycosidae",
      is.na(family) & subfamily == "nemobiinae" ~ "trigonidiidae",
      is.na(family) & subfamily == "phorinae" ~ "phoridae",
      is.na(family) & genus == "wiseana" ~ "hepialidae",
      TRUE ~ family                                                               
    ), 
    
    order = case_when(
      family %in% c("coccidellidae", "curculionidae", "latridiidae", "carabidae", "staphylinidae") ~ "coleoptera",
      family %in% c("pieridae", "tineidae", "hepialidae") ~ "lepidoptera",
      family %in% c("lycosidae","lynphiidae") ~ "araneae",
      family %in% c("trigonidiidae") ~ "orthoptera",
      family %in% c("phoridae") ~ "diptera",
      family %in% c("chrysopidae") ~ "neuroptera",
      family %in% c("reduviidae") ~ "hemiptera",
      TRUE ~ order
    ),
    
    superorder = case_when(
      order %in% c("amphipoda") ~ "peracarida",
      order %in% c("coleoptera", "lepidoptera", "diptera", "hymenoptera") ~ "endopterygota",
      order %in% c("opiliones") ~ "parasitiformes",
      order %in% c("neuroptera") ~ "neuropterida",
      order %in% c("hemiptera") ~ "codylognatha",
      order %in% c("orthoptera") ~ "orthopterida",
      TRUE ~ superorder
    ),
    
    class = case_when(
      order %in% c("coleoptera", "lepidoptera", "orthoptera", "diptera", "neuroptera", "hemiptera", "hymenoptera") ~ "insecta",
      order %in% c("araneae", "opiliones") ~ "arachnida",
      superorder %in% c("mite", "mites") ~ "arachnida",
      order %in% c("amphipoda") ~ "malacostraca",
      order %in% c("lithobiomorpha") ~ "chilopoda",
      order %in% c("stylommatophora") ~ "gastropoda",
      TRUE ~ class
    ),
    
    phylum = case_when(
      order %in% c("stylommatophora") ~ "mollusca",
      TRUE ~ phylum
    )
  )

long_pan <- long_pan %>%
  mutate(phylum = ifelse(order == "stylommatophora", "mollusca", phylum))
###### Pitfall Trap Cleaning ######


## Transform pitfall to long format ##

pitfall <- pitfall %>%
  mutate(across(
    -c("Site name", "Farm", "Region", "Trap Type", "Habitat Type"),  # keep these as-is
    ~ suppressWarnings(as.numeric(.))
  ))

long_pitfall <- pitfall %>%
  tidyr::pivot_longer(
    cols = -c("Site name", "Farm", "Region", "Trap Type", "Habitat Type"),
    names_to = "invert_type",
    values_to = "abundance"
  )

long_pitfall <- long_pitfall %>%
  mutate(
    phylum = if(! "phylum" %in% names(.)) NA else phylum,
    class = if (! "class" %in% names(.)) NA else class,
    superorder = if (! "superorder" %in% names(.)) NA else superorder,
    order = if (!"order" %in% names(.)) NA else order,
    family = if (!"family" %in% names(.)) NA else family,
    subfamily = if (! "subfamily" %in% names(.)) NA else subfamily,
    tribe = if (!"tribe" %in% names(.)) NA else tribe,
    genus = if (!"genus" %in% names(.)) NA else genus
  ) %>%
  relocate(phylum, .after = "Habitat Type") %>%
  relocate(class, .after = phylum) %>%
  relocate(superorder, .after = class) %>%
  relocate(order, .after = superorder) %>%
  relocate(family, .after = order) %>%
  relocate(subfamily, .after = family)%>%
  relocate(tribe, .after = subfamily) %>%
  relocate(genus, .after = tribe)


long_pitfall <- long_pitfall %>%                             # Renaming "invert type" to species
  rename(species = invert_type)

## Moving values from "species" into their correct taxonomic group columns ##

# Moving values to order based on their character strings #
long_pitfall <- long_pitfall %>%
  mutate(
    order = ifelse(species %in% c("Coleoptera", "Araneae", "Orthoptera", "Diptera", "Hymenoptera", "Lepidoptera", "Opiliones", "Lithobiomorpha", "Stylommatophora", "Neuroptera", "Hemiptera", "Amphipoda"), species, order),     # Move selected values
    species = ifelse(species %in% c("Coleoptera", "Araneae", "Orthoptera", "Diptera", "Hymenoptera", "Lepidoptera", "Opiliones", "Lithobiomorpha", "Stylommatophora", "Neuroptera", "Hemiptera", "Amphipoda"), "NA", species)     # Replace with "NA"
  )

# Moving values to family #
long_pitfall <- long_pitfall %>%
  mutate(
    family = ifelse(species %in% c("Carabidae", "Staphylininae", "Curculionidae", "Latridiidae", "Lynphiidae", "Chrysopidae", "Reduviidae"), species, family),  # Move selected values to family
    species = ifelse(species %in% c("Carabidae", "Staphylininae", "Curculionidae", "Latridiidae", "Lynphiidae", "Chrysopidae", "Reduviidae"), "NA", species)    # Replace with "NA"
  )

# Correcting Staphylininae to Staphylinidae
long_pitfall <- long_pitfall %>%
  mutate(
    family = str_replace_all(family, c(
      "Staphylininae" = "Staphylinidae")))

# Moving values to subfamily #
long_pitfall <- long_pitfall %>%
  mutate(
    subfamily = ifelse(species %in% c("Harpalinae"), species, subfamily),
    species = ifelse(species %in% c("Harpalinae"), "NA", species)
  )

# Moving values to tribe #
long_pitfall <- long_pitfall %>% 
  mutate(
    tribe = ifelse(species %in% c("Stenolophini", "Pentagonicini", "Epuraeini"), species, tribe),
    species = ifelse(species %in% c("Stenolophini", "Pentagonicini", "Epuraeini"), "NA", species)
  )

# Moving values to genus #
long_pitfall <- long_pitfall %>%
  mutate(
    genus = ifelse(species %in% c("Sitona", "Mecodema", "Coccinella", "Anoteropsis", "Bobilla", "Megaselia", "Wiseana"), species, genus), 
    species = ifelse(species %in% c("Sitona", "Mecodema", "Coccinella", "Anoteropsis", "Bobilla", "Megaselia", "Wiseana"), "NA", species)
  )

# Moving values to phylum and superorder #
long_pitfall <- long_pitfall %>%
  mutate(
    phylum = ifelse(species %in% c("Annelid"), species, phylum),
    species = ifelse(species %in% c("Annelid"), "NA", species)
  )

long_pitfall <- long_pitfall %>%
  mutate(
    superorder = ifelse(species %in% c("Mite", "Mites"), species, superorder),
    species = ifelse(species %in% c("Mite", "Mites"), "NA", species)
  )

# Filling the higher taxonomic cells #
long_pitfall <- long_pitfall %>%
  mutate(phylum = coalesce(phylum, "Arthropoda"))  # filling all the NAs in with arthropoda


long_pitfall <- long_pitfall %>%
  mutate(phylum = recode(phylum, "Annelid" = "Annelida")) # editing annelid to be annelida

long_pitfall <- long_pitfall %>%
  mutate(
    genus = case_when(
      is.na(genus) & species == "Coccinella undecimpunctata" ~ "Coccinella",
      is.na(genus) & species == "Cartodere_bifasciata" ~ "Cartodere",
      is.na(genus) & species == "Peris_rapae" ~ "Pieris",                          # peris_rapae is spelled wrong, it should be pieris_rapae
      is.na(genus) & species == "Oponga_omoscopa" ~ "Oponga",
      is.na(genus) & species == "Notagonum submetallicum" ~ "Notagonum",
      is.na(genus) & species == "Anisodactylus biontatus" ~ "Anisodactylus",
      is.na(genus) & species == "Harpalus affinis" ~ "Harpalus",
      is.na(genus) & species == "Mecodema moniliferum" ~ "Mecodema",
      TRUE ~ genus
    ),
    
    tribe = case_when(
      is.na(tribe) & genus == "Coccinella" ~ "Coccinellini",
      is.na(tribe) & genus == "Sitona" ~ "Sitonini",
      is.na(tribe) & genus == "Cartodere" ~ "Cartoderini",
      is.na(tribe) & genus == "Pieris" ~ "Pierini",
      is.na(tribe) & genus == "Bobilla" ~ "Nemobiini",
      is.na(tribe) & genus == "Notagonum" ~ "Platynini",
      is.na(tribe) & genus == "Anisodactylus" ~ "Harpalini",
      is.na(tribe) & genus == "Harpalus" ~ "Harpalini",
      is.na(tribe) & genus == "Mecodema" ~ "Broscini",
      TRUE ~ tribe
    ), 
    
    subfamily = case_when(
      is.na(subfamily) & tribe == "Coccinellini" ~ "Coccinellinae",
      is.na(subfamily) & tribe == "Sitonini" ~ "Entiminae",
      is.na(subfamily) & tribe == "Cartoderini" ~ "Latridiinae",
      is.na(subfamily) & tribe == "Pierini" ~"Pierinae",
      is.na(subfamily) & genus == "Oponga" ~ "Hieroxestinae",
      is.na(subfamily) & tribe == "Stenolophini" ~ "Harpalinae",
      is.na(subfamily) & tribe == "Pentagonicini" ~ "Harpalinae",
      is.na(subfamily) & genus == "Anoteropsis" ~ "Artoriinae",
      is.na(subfamily) & tribe == "Nemobiini" ~ "Nemobiinae",
      is.na(subfamily) & genus == "Megaselia" ~ "Phorinae",
      is.na(subfamily) & tribe == "Platynini" ~ "Platyninae",
      is.na(subfamily) & tribe == "Harpalini" ~ "Harpalinae",
      is.na(subfamily) & tribe == "Broscini" ~ "Broscinae",
      TRUE ~ subfamily
    ),
    
    family = case_when(                                                           
      is.na(family) & subfamily == "Coccinellinae" ~ "Coccidellidae",                    
      is.na(family) & subfamily == "Entiminae" ~ "Curculionidae",
      is.na(family) & subfamily == "Latridiinae"~ "Latridiidae",
      is.na(family) & subfamily == "Harpalinae" ~ "Carabidae",
      is.na(family) & subfamily == "Pierinae" ~ "Pieridae",
      is.na(family) & subfamily == "Hieroxestinae" ~ "Tineidae",
      is.na(family) & subfamily == "Artoriinae" ~ "Lycosidae",
      is.na(family) & subfamily == "Nemobiinae" ~ "Trigonidiidae",
      is.na(family) & subfamily == "Phorinae" ~ "Phoridae",
      is.na(family) & genus == "Wiseana" ~ "Hepialidae",
      is.na(family) & subfamily == "Platyninae" ~ "Carabidae",
      is.na(family) & subfamily == "Broscinae" ~ "Carabidae",
      is.na(family) & tribe == "Epuraeini" ~ "Nitidulidae",
      TRUE ~ family                                                               
    ), 
    
    order = case_when(
      family %in% c("Coccidellidae", "Curculionidae", "Latridiidae", "Carabidae", "Staphylinidae", "Nitidulidae") ~ "Coleoptera",
      family %in% c("Pieridae", "Tineidae", "Hepialidae") ~ "Lepidoptera",
      family %in% c("Lycosidae","Lynphiidae") ~ "Araneae",
      family %in% c("Trigonidiidae") ~ "Orthoptera",
      family %in% c("Phoridae") ~ "Diptera",
      family %in% c("Chrysopidae") ~ "Neuroptera",
      family %in% c("Reduviidae") ~ "Hemiptera",
      TRUE ~ order
    ),
    
    superorder = case_when(
      order %in% c("Amphipoda") ~ "Peracarida",
      order %in% c("Coleoptera", "Lepidoptera", "Diptera", "Hymenoptera") ~ "Endopterygota",
      order %in% c("Opiliones") ~ "Parasitiformes",
      order %in% c("Neuroptera") ~ "Neuropterida",
      order %in% c("Hemiptera") ~ "Codylognatha",
      order %in% c("Orthoptera") ~ "Orthopterida",
      TRUE ~ superorder
    ),
    
    class = case_when(
      order %in% c("Coleoptera", "Lepidoptera", "Orthoptera", "Diptera", "Neuroptera", "Hemiptera", "Hymenoptera") ~ "Insecta",
      order %in% c("Araneae", "Opiliones") ~ "Arachnida",
      superorder %in% c("Mite", "Mites") ~ "Arachnida",
      order %in% c("Amphipoda") ~ "Malacostraca",
      order %in% c("Lithobiomorpha") ~ "Chilopoda",
      order %in% c("Stylommatophora") ~ "Gastropoda",
      TRUE ~ class
    ),
    
    phylum = case_when(
      order %in% c("Stylommatophora") ~ "Mollusca",
      TRUE ~ phylum
    )
  )




############### Code Pieces Repository/Workshop ######################


##### Separating columns by taxonomic group #####
#taxonomic_columns <- list(
 # Order = c("coleoptera", "araneae", "orthoptera", "diptera", "hymenoptera", "lepidoptera", "opiliones", "lithobiomorpha", "stylommatophora", "neuroptera", "hemiptera", "amphipoda")


##### Transform Pitfall and Pan to Long Format ##### Not ready for this as sheets are still mixed taxonomic group

### Pitfall ###
#long_pitfall <- pitfall%>%
#  tidyr::pivot_longer(cols = -c(site_name, farm, region, trap_type, habitat_type), 
#              names_to = "species", 
#               value_to = "count")

#print(long_pitfall)

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
