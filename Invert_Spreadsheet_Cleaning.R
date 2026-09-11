###############################################################################################

# Project: Invertebrate Biodiversity in Farmland Hedgerow Study
# Script Purpose: Initial Data Cleaning, produces "Invert_Data_R_Version"
# Author: Mary Buford Turnage
# Date Created: 2025-03-19
# Last Modified: 2026-08-27
# Version Control: GitHub Repo: https://github.com/MB-Turnage/Invertebrates2024.git

###############################################################################################

#### Packages ####
# install.packages("janitor") # If needed #
library(janitor)

# install.packages("openxlsx") # If needed #
library(openxlsx)

# install.packages("cli") #If needed #
library(cli)

# install.packages("writexl")
library(writexl)

## Load necessary packages ##
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)

##### Data Cleaning ######

## Read in the master "Inverts_2024" dataset ##
master <- readxl::read_excel("/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_2024.xlsx") #renamed Inverts_2024

## Check that all the sheet names are correct ##
sheet_names <- getSheetNames("/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_2024.xlsx")
print(sheet_names) # i.e, "Pitfall_Traps_Original", "Pan Traps", etc

## Read in the sheets for Pitfall Traps, Pan Traps, and Light Traps ##
pan <- read_excel("/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_2024.xlsx", sheet = "Pan Traps")
light <- read_excel("/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_2024.xlsx", sheet = "Light Traps")
pitfall <- read_excel("/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_2024.xlsx", sheet = "Pitfall_Traps_Original")

## Clean column names for easier reference - "Pitfall_Traps_Original " has a space in the Inverts_2024.xlxs ##
pitfall <- janitor::clean_names(pitfall)
pan <- clean_names(pan)
light <- clean_names(light)

## Replace the blanks with zeros ## 
pitfall[1:95, 6:50][is.na(pitfall[1:95, 6:50])] <- 0 
pan[1:33, 6:39][is.na(pan[1:33, 6:39])] <- 0 

## Light is in a different format, so checking if the count column is string ##
str(light)

## Converting abundance column to numeric ##
light$abundance <- as.numeric(light$abundance)

## Replacing blanks or NAs with zeros ##
light$abundance[is.na(light$abundance)] <- 0

###### Pan Trap Data Cleaning #######

## Transform pan to long format ##
long_pan <- pan %>%
    tidyr::pivot_longer(cols = -c(site_name, farm, region, trap_type, habitat_type), 
               names_to = "invert_type", 
               values_to = "abundance")

pan %>% filter(is.na(`Site name`) | is.na(Farm) | is.na(Habitat_Type))

## Add a columns for taxonomic levels. This code can be updated for any additional columns that need to be added without overwriting the values that have been moved ##

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

## Moving values from "species" into their correct taxonomic group columns ##

# Moving values to order based on their character strings #
long_pan <- long_pan %>%
  mutate(
    order = ifelse(species %in% c("coleoptera", "araneae", "orthoptera", "diptera", "hymenoptera", "lepidoptera", "opiliones", "lithobiomorpha", "stylommatophora", "neuroptera", "hemiptera", "amphipoda"), species, order),     # Move selected values
    species = ifelse(species %in% c("coleoptera", "araneae", "orthoptera", "diptera", "hymenoptera", "lepidoptera", "opiliones", "lithobiomorpha", "stylommatophora", "neuroptera", "hemiptera", "amphipoda"), NA_character_, species)     # Replace with "NA"
  )

# Moving values to family #
long_pan <- long_pan %>%
  mutate(
    family = ifelse(species %in% c("carabidae", "staphylinidae", "curculionidae", "latridiidae", "lynphiidae", "chrysopidae", "reduviidae"), species, family),  # Move selected values to family
    species = ifelse(species %in% c("carabidae", "staphylinidae", "curculionidae", "latridiidae", "lynphiidae", "chrysopidae", "reduviidae"), NA_character_, species)    # Replace with "NA"
  )

# Moving values to subfamily #
long_pan <- long_pan %>%
  mutate(
    subfamily = ifelse(species %in% c("harpalinae"), species, subfamily),
    species = ifelse(species %in% c("harpalinae"), NA_character_, species)
  )

# Moving values to tribe #
long_pan <- long_pan %>% 
  mutate(
    tribe = ifelse(species %in% c("stenolophini", "pentagonicini"), species, tribe),
    species = ifelse(species %in% c("stenolophini", "pentagonicini"), NA_character_, species)
  )

# Moving values to genus #
long_pan <- long_pan %>%
  mutate(
    genus = ifelse(species %in% c("sitona", "coccinella", "anoteropsis", "bobilla", "megaselia", "wiseana"), species, genus), 
    species = ifelse(species %in% c("sitona", "coccinella", "anoteropsis", "bobilla", "megaselia", "wiseana"), NA_character_, species)
  )

# Moving values to phylum and superorder #
long_pan <- long_pan %>%
  mutate(
    phylum = ifelse(species %in% c("annelid"), species, phylum),
    species = ifelse(species %in% c("annelid"), NA_character_, species)
  )

long_pan <- long_pan %>%
  mutate(
    superorder = ifelse(species %in% c("mite", "mites"), species, superorder),
    species = ifelse(species %in% c("mite", "mites"), NA_character_, species)
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

# add LTU column #
long_pan <- long_pan %>%
  mutate(across(c(phylum, class, superorder, order, family, subfamily, tribe, genus, species),
                ~ na_if(trimws(as.character(.x)), ""))) %>%
  mutate(
    LTU = coalesce(species, genus, tribe, subfamily, family, order, superorder, class, phylum),
    LTU_rank = case_when(
      !is.na(species)    ~ "species",
      !is.na(genus)      ~ "genus",
      !is.na(tribe)      ~ "tribe",
      !is.na(subfamily)  ~ "subfamily",
      !is.na(family)     ~ "family",
      !is.na(order)      ~ "order",
      !is.na(superorder) ~ "superorder",
      !is.na(class)      ~ "class",
      !is.na(phylum)     ~ "phylum",
      TRUE ~ "unidentified"
    )
  )

# Quick check: how many individuals landed at each rank
long_pan %>% count(LTU_rank, sort = TRUE)


###### Light Traps Data Cleaning #######

## Adding taxonomic columns to match long_pan ##

light <- light %>%
  mutate(
    phylum = if(! "phylum" %in% names(.)) NA_character_ else phylum,
    class = if (! "class" %in% names(.)) NA_character_ else class,
    superorder = if (! "superorder" %in% names(.)) NA_character_ else superorder,
    order = if (!"order" %in% names(.)) NA_character_ else order,
    family = if (!"family" %in% names(.)) NA_character_ else family,
    subfamily = if (! "subfamily" %in% names(.)) NA_character_ else subfamily,
    tribe = if (!"tribe" %in% names(.)) NA_character_ else tribe,
    genus = if (!"genus" %in% names(.)) NA_character_ else genus
  ) %>%
  relocate(phylum, .after = date) %>%
  relocate(class, .after = phylum) %>%
  relocate(superorder, .after = class) %>%
  relocate(order, .after = superorder) %>%
  relocate(family, .after = order) %>%
  relocate(subfamily, .after = family)%>%
  relocate(tribe, .after = subfamily) %>%
  relocate(genus, .after = tribe)

# fixing species names (or names of values in species column to start)
#light <- light %>%
 # mutate(
 #   species = str_replace_all(species, c(
 #     "Huhu" = "Prionoplus reticularis",
  #    "Flesh fly" = "Sarcophagidae"
      
 #   ))
#  )

# fixing issues in order spelling
light <- light %>%
  mutate(
    order = str_replace_all(order, c(
      "Coleopetera" = "Coleoptera",
      "Coleoptera" = "Coleoptera",
      "Coleotpera" = "Coleoptera",
      "coleoptera" = "Coleoptera"
    )))


light <- light %>%
  mutate(
    order = str_replace_all(order, c(
      "Hymenotpera" = "Hymenoptera"))
  )

light <- light %>%
  mutate(
    order = str_replace_all(order, c(
      "Lepidopera" = "Lepidoptera"))
  )

## Filling in taxonomic values for light trap datasheet ##

light <- light %>%
  mutate(phylum = coalesce(phylum, "arthropoda")) # filling all the NAs in with arthropoda

light <- light %>%
  mutate(class = coalesce(class, "insecta"))

## Changing "rove" to staphylinidae and other small corrections ##
light <- light %>%
  mutate(species = ifelse(species == "Rove", "staphylinidae", species),         # Replace "Rove" with staphylinidae in species column
         family = ifelse(species == "staphylinidae", "staphylinidae", family),  # Move staphylinidae to family column
         species = ifelse(species == "staphylinidae", NA_character_, species))             # Replace staphylinidae with NA

light <- light %>%
  mutate(species =ifelse(species == "	Diving Beelte", "dytiscidae", species),
         family = ifelse(species == "dytiscidae", "dytiscidae", family),
         species = ifelse(species == "dytiscidae", NA_character_, species))

# Moving values to genus #
light <- light %>%
  mutate(
    genus = ifelse(species %in% c("Pseudocoremia", "Odontria", "Amblyptilia", "Capua", "Eudonia", "Coleophora", "Orocrambus", "Staphylinidae", "Wisiena"), species, genus), 
    species = ifelse(species %in% c("Pseudocoremia", "Odontria", "Amblyptilia", "Capua", "Eudonia", "Coleophora", "Orocrambus", "Staphylinidae", "Wisiena"), NA_character_, species),
    genus = ifelse(genus %in% c("Pseudocoremia", "Odontria", "Amblyptilia", "Capua", "Eudonia", "Coleophora", "Orocrambus", "Staphylinidae", "Wisiena"), tolower(genus), genus))

# Moving values to tribe #
light <- light %>%
  mutate(
    tribe = ifelse(species %in% c("Lebiini"), species, tribe), 
    species = ifelse(species %in% c("Lebiini"), NA_character_, species),
    tribe = ifelse(tribe %in% c("Lebiini"), tolower(tribe), tribe))

# Moving values to subfamily #
light <- light %>%
  mutate(
    subfamily = ifelse(species %in% c("Licininae", "Pselaphinae"), species, subfamily), 
    species = ifelse(species %in% c("Licininae", "Pselaphinae"), NA_character_, species),
    subfamily = ifelse(subfamily %in% c("Licininae", "Pselaphinae"), tolower(subfamily), subfamily))

# Moving values to family #
light <- light %>%
  mutate(
    family = ifelse(species %in% c("Geometridae", "Crambidae", "Carabidae", "Hydrophilidae", "Sarcophagidae", "Chironimidae", "Lygaeidae", "Syrphidae", "Muscidae", "Scirtidae", "Tortricidae"), species, family),
    species = ifelse(species %in% c("Geometridae", "Crambidae", "Carabidae", "Hydrophilidae", "Sarcophagidae", "Chironimidae", "Lygaeidae", "Syrphidae", "Muscidae", "Scirtidae", "Tortricidae"), NA_character_, species),
    family = ifelse(family %in% c("Geometridae", "Crambidae", "Carabidae", "Hydrophilidae", "Sarcophagidae", "Chironimidae", "Lygaeidae", "Syrphidae", "Muscidae", "Scirtidae", "Tortricidae"), tolower(family), family))
  

light <- light %>%
  mutate(
    genus = case_when(
      is.na(genus) & species == "Epyaxa rosearia" ~ "epyaxa",
      is.na(genus) & species == "Orocrambus flexuosellus" ~ "orocrambus",
      is.na(genus) & species == "Eudonia submarginalis" ~ "eudonia",
      is.na(genus) & species == "Forficula auricularia" ~ "forficula",
      is.na(genus) & species == "Acrossidius tasmaniae" ~ "acrossidius",
      is.na(genus) & species == "Hygraula nitens" ~ "hygraula",
      is.na(genus) & species == "Chloroclystis inductata" ~ "chloroclystis",
      is.na(genus) & species == "Netelia ephippiata" ~ "netelia",
      is.na(genus) & species == "Hydriomena deltoidata" ~ "hydriomena",
      is.na(genus) & species == "Xanthorhoe semifissata" ~ "xanthorhoe",
      is.na(genus) & species == "Agonochelia antipodium" ~ "agonochelia",
      is.na(genus) & species == "Barea exarcha" ~ "barea",
      is.na(genus) & species == "Cydia succedana" ~ "cydia",
      is.na(genus) & species == "Epiphyas postivittana" ~ "epiphyas",
      is.na(genus) & species == "Eudonia leptalaea" ~ "eudonia",
      is.na(genus) & species == "Helastia corcularia" ~ "helastia",
      is.na(genus) & species == "Homodotis megapilata" ~ "homodotis",
      is.na(genus) & species == "Cartodere bifasciata" ~ "cartodere",
      is.na(genus) & species == "Prionoplus reticularis" ~ "prionoplus",
      is.na(genus) & species == "Ichneutica atristriga" ~ "ichneutica",
      is.na(genus) & species == "Ichneutica mutans" ~ "ichneutica",
      is.na(genus) & species == "Ichneutica plena" ~ "ichneutica",
      is.na(genus) & species == "Monocrepidius exsul" ~ "monocrepidius",
      is.na(genus) & species == "Notagonum submetallicum" ~ "notagonum",
      is.na(genus) & species == "Orocrambus ramosellus" ~ "orocrambus",
      is.na(genus) & species == "Tebenna micalis" ~ "tebenna",
      is.na(genus) & species == "Uresiphita maorialis" ~ "uresiphita",

      TRUE ~ genus
    ),
    
    tribe = case_when(
      is.na(tribe) & genus == "Pseudocoremia" ~ "boarmiini",
      is.na(tribe) & genus == "epyaxa" ~ "xanthorhoini",
      is.na(tribe) & genus == "orocrambus" ~ "crambini",
      is.na(tribe) & genus == "Odontria" ~ "liparetrini",
      is.na(tribe) & genus == "forficula" ~ "forficulini",
      is.na(tribe) & genus == "Amblyptilia" ~ "platyptiliini",
      is.na(tribe) & genus == "acrossidius" ~ "aphodiini",
      is.na(tribe) & genus == "Capua" ~ "archipini",
      is.na(tribe) & genus == "prionoplus" ~ "rhipidocerini",
      is.na(tribe) & genus == "notagonum" ~ "platynini",
      is.na(tribe) & genus == "orocrambus" ~ "crambini",
      TRUE ~ tribe
    ), 
    
    subfamily = case_when(
      is.na(subfamily) & tribe == "boarmiini" ~ "ennominae",
      is.na(subfamily) & tribe == "xanthorhoini" ~ "larentiinae",
      is.na(subfamily) & tribe == "crambini" ~ "crambinae",
      is.na(subfamily) & tribe == "liparetrini" ~ "melolonthinae",
      is.na(subfamily) & genus == "scopariinae" ~ "crambinae",
      is.na(subfamily) & tribe == "forficulini" ~ "forficulinae",
      is.na(subfamily) & tribe == "aphodiini" ~ "aphodiinae",
      is.na(subfamily) & tribe == "archipini" ~ "tortricinae",
      is.na(subfamily) & tribe == "rhipidocerini" ~ "prioninae",
      is.na(subfamily) & tribe == "platynini" ~ "platyninae",
      is.na(subfamily) & tribe == "crambini" ~ "crambinae",
      
      TRUE ~ subfamily
    ),
    
    family = case_when(                                                           
      is.na(family) & subfamily == "ennominae" ~ "geometridae",                    
      is.na(family) & subfamily == "larentiinae" ~ "geometridae",
      is.na(family) & subfamily == "crambinae"~ "crambidae",
      is.na(family) & subfamily == "melolonthinae"~ "scarabaeidae",
      is.na(family) & subfamily == "forficulinae" ~"forficulidae",
      is.na(family) & tribe == "platyptiliini" ~ "pterophoridae",
      is.na(family) & genus == "eudonia" ~ "crambidae",
      is.na(family) & subfamily == "aphodiinae" ~ "scarabaeidae",
      is.na(family) & genus == "hygraula" ~ "crambidae",
      is.na(family) & subfamily == "tortricinae" ~ "tortricidae",
      is.na(family) & genus == "chloroclystis" ~ "geometridae",
      is.na(family) & genus == "Wisiena" ~ "hepialidae",   #Wisiena spelled differently in pan trap sheet
      is.na(family) & genus == "netelia" ~ "ichneumonidae",
      is.na(family) & genus == "hydriomena" ~ "geometridae",
      is.na(family) & genus == "xanthorhoe" ~ "geometridae",
      is.na(family) & genus == "agonochelia" ~ "carabidae",
      is.na(family) & genus ==  "barea" ~ "oecophoridae",
      is.na(family) & genus == "helastia" ~ "geometridae",
      is.na(family) & genus == "prionoplus" ~ "cerambycidae",
      is.na(family) & genus == "cartodere" ~ "latridiidae",
      is.na(family) & genus == "cydia" ~ "tortricidae",
      is.na(family) & genus == "epiphyas" ~ "tortricidae",
      is.na(family) & genus == "homodotis" ~"geometridae",
      is.na(family) & genus == "amblyptilia" ~ "pterophoridae",
      is.na(family) & genus == "ichneutica" ~ "noctuidae",
      is.na(family) & tribe == "lebiini" ~ "carabidae",
      is.na(family) & genus == "monocrepidius" ~ "elateridae",
      is.na(family) & subfamily == "platyninae" ~ "carabidae",
      is.na(family) & genus == "tebenna" ~ "choreutidae",
      is.na(family) & genus == "uresiphita" ~ "crambidae",
      is.na(family) & genus == "capua" ~ "tortricidae",
    
      
      TRUE ~ family                                                               
    ), 
    
        superorder = case_when(
      order %in% c("coleoptera", "lepidoptera", "diptera", "hymenoptera") ~ "endopterygota",
      order %in% c("opiliones") ~ "parasitiformes",
      order %in% c("neuroptera") ~ "neuropterida",
      order %in% c("hemiptera") ~ "codylognatha",
      order %in% c("orthoptera") ~ "orthopterida",
      TRUE ~ superorder
    ),
  )



# standardise lower cases
light <- light %>%
  mutate(
    order = ifelse(order %in% c("Coleoptera", "Hymenoptera", "Lepidoptera", "Dermaptera", "Diptera", "Trichoptera"), tolower(order), order))

light <- light %>%
  mutate(
    species = ifelse(species %in% c("Epyaxa rosearia", "Orocrambus flexuosellus", "Eudonia submarginalis", "Acrossidius tasmaniae", "Agonochelia antipodium", 
                                    "Chloroclystis inductata", "Cydia succedana", "Epiphyas postivittana", "Barea exarcha", "Eudonia leptalaea", "Forficula auricularia",
                                    "Helastia corcularia", "Homodotis megapilata", "Cartodere bifasciata", "Hydriomena deltoidata", "Prionoplus reticularis",
                                    "Hygraula nitens", "Ichneutica atristriga", "Ichneutica mutans", "Ichneutica plena", "Monocrepidius exsul", "Notagonum submetallicum",
                                    "Netelia ephippiata", "Orocrambus ramosellus", "Tebenna micalis", "Uresiphita maorialis", "Xanthorhoe semifissata"), tolower(species), species))



# add LTU column #
light <- light %>%
  mutate(across(c(phylum, class, superorder, order, family, subfamily, tribe, genus, species),
                ~ na_if(trimws(as.character(.x)), ""))) %>%
  mutate(
    LTU = coalesce(species, genus, tribe, subfamily, family, order, superorder, class, phylum),
    LTU_rank = case_when(
      !is.na(species)    ~ "species",
      !is.na(genus)      ~ "genus",
      !is.na(tribe)      ~ "tribe",
      !is.na(subfamily)  ~ "subfamily",
      !is.na(family)     ~ "family",
      !is.na(order)      ~ "order",
      !is.na(superorder) ~ "superorder",
      !is.na(class)      ~ "class",
      !is.na(phylum)     ~ "phylum",
      TRUE ~ "unidentified"
    )
  )

# Quick check: how many individuals landed at each rank
light %>% count(LTU_rank, sort = TRUE)

###### Pitfall Trap Cleaning ######

## Transform pitfall to long format ##

pitfall <- pitfall %>%
  mutate(across(
    -c("site_name", "farm", "region", "trap_type", "habitat_type"),  # keep these as-is
    ~ suppressWarnings(as.numeric(.))
  ))

long_pitfall <- pitfall %>%
  tidyr::pivot_longer(
    cols = -c("site_name", "farm", "region", "trap_type", "habitat_type"),
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
  relocate(phylum, .after = "habitat_type") %>%
  relocate(class, .after = phylum) %>%
  relocate(superorder, .after = class) %>%
  relocate(order, .after = superorder) %>%
  relocate(family, .after = order) %>%
  relocate(subfamily, .after = family)%>%
  relocate(tribe, .after = subfamily) %>%
  relocate(genus, .after = tribe)


long_pitfall <- long_pitfall %>%                             # Renaming "invert type" to species
  rename(species = invert_type)

### Moving values from "species" into their correct taxonomic group columns ###

## Correcting Spelling Errors ##

# Correcting Staphylininae to Staphylinidae #
long_pitfall <- long_pitfall %>%
  mutate(
    family = str_replace_all(family, c(
      "staphylininae" = "staphylinidae")))

# Correcting Curculionoidea to Curculionidae"
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "curculionoidea" = "curculionidae")))

# Correcting Listronotus.boneriensis to Listronotus.bonariensis #
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "listronotus.boneriensis" = "listronotus.bonariensis")))

# Correcting Lycosideae to Lycosidae #
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "lycosideae" = "lycosidae")))

# Correcting Amphipod to Amphipoda #
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "amphipod" = "amphipoda")))

# Correcting Assian_bug -> Stenolemus fraterculus (Assassin Bug) #
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "assian_bug" = "stenolemus_fraterculus")))

# Correcting milipede to diplopoda #
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "millipede" = "diplopoda")))

# Correcting "Sciarodia" to "Sciaridae" #
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "sciarodia" = "sciaridae")))

# Correcting 	Pleioplectron_simplex...61 and Pleioplectron_simplex...108 to Pleioplectron simplex #
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "pleioplectron_simplex...61" = "pleioplectron simplex")))

long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "pleioplectron_simplex...108" = "pleioplectron simplex")))

# Correcting slater to Porcellio scaber
long_pitfall <- long_pitfall %>%
  mutate(
    species = str_replace_all(species, c(
      "slater" = "porcellio_scaber")))




## Moving values to order based on their character strings ##
long_pitfall <- long_pitfall %>%
  mutate(
    order = ifelse(species %in% c("coleoptera", "julida", "araneae", "orthoptera", "diptera", "hymenoptera", "lepidoptera", "opiliones", "lithobiomorpha", "stylommatophora", "neuroptera", "hemiptera", "amphipoda", "dermaptera", "isopoda", "thysanoptera"), species, order),     # Move selected values
    species = ifelse(species %in% c("coleoptera", "julida", "araneae", "orthoptera", "diptera", "hymenoptera", "lepidoptera", "opiliones", "lithobiomorpha", "stylommatophora", "neuroptera", "hemiptera", "amphipoda", "dermaptera", "isopoda", "thysanoptera"), NA_character_, species)     # Replace with "NA"
  )

# Moving values to family #
long_pitfall <- long_pitfall %>%
  mutate(
    family = ifelse(species %in% c("carabidae", "talitridae", "sciaridae", "miridae", "cicadellidae", "aphidae", "tetragnathidae", "gnphosidae", "theridiidae", "salticidae", "lycosidae",  "anthicidae", "cerambycidae", "curculionidae", "latridiidae", "lynphiidae", "chrysopidae", "reduviidae", "henicopidae", "ichneumonioidea", "tineidae", "muscidae", "proctotrupidae"), species, family),  # Move selected values to family
    species = ifelse(species %in% c("carabidae", "talitridae", "sciaridae", "miridae", "cicadellidae", "aphidae", "tetragnathidae", "gnphosidae", "theridiidae", "salticidae", "lycosidae",  "anthicidae", "cerambycidae", "curculionidae", "latridiidae", "lynphiidae", "chrysopidae", "reduviidae", "henicopidae", "ichneumonioidea", "tineidae", "muscidae", "proctotrupidae"), NA_character_, species))    # Replace with "NA" 


# Moving values to subfamily #
long_pitfall <- long_pitfall %>%
  mutate(
    subfamily = ifelse(species %in% c("harpalinae", "deltocephalinae", "orsillinae", "staphylininae"), species, subfamily),
    species = ifelse(species %in% c("harpalinae", "deltocephalinae", "orsillinae", "staphylininae"), NA_character_, species)
  )

# Moving values to tribe #
long_pitfall <- long_pitfall %>% 
  mutate(
    tribe = ifelse(species %in% c("stenolophini", "pentagonicini", "epuraeini", "alysiini"), species, tribe),
    species = ifelse(species %in% c("stenolophini", "pentagonicini", "epuraeini", "alysiini"), NA_character_, species)
  )

# Moving values to genus #
long_pitfall <- long_pitfall %>%
  mutate(
    genus = ifelse(species %in% c("somatidia", "nuncia", "deroceras", "cotes", "algidia", "allotrochosina", "cambridgea","steatoda", "sidymella", "odontria", "sitona", "mecodema", "coccinella", "anoteropsis", "listronotus", "bobilla", "megaselia", "wiseana", "labarrus", "xylotoles", "chalepistes", "acrossidius", "lithobius", "orocrambus"), species, genus), 
    species = ifelse(species %in% c("somatidia", "nuncia", "deroceras", "cotes", "algidia", "allotrochosina", "cambridgea","steatoda", "sidymella", "odontria", "sitona", "mecodema", "coccinella", "anoteropsis", "listronotus", "bobilla", "megaselia", "wiseana", "labarrus", "xylotoles", "chalepistes", "acrossidius", "lithobius", "orocrambus"), NA_character_, species)
  )

# Moving values to phylum, class, and superorder #
long_pitfall <- long_pitfall %>%
  mutate(
    phylum = ifelse(species %in% c("annelid"), species, phylum),
    species = ifelse(species %in% c("annelid"), NA_character_, species)
  )

long_pitfall <- long_pitfall %>%
  mutate(
    class = ifelse(species %in% c("collembola", "diplopoda"), species, class),
    species = ifelse(species %in% c("collembola", "diplopoda"), NA_character_, species)
  )

long_pitfall <- long_pitfall %>%
  mutate(
    superorder = ifelse(species %in% c("Mite", "Mites"), species, superorder),
    species = ifelse(species %in% c("Mite", "Mites"), NA_character_, species)
  )

# Filling the higher taxonomic cells #
long_pitfall <- long_pitfall %>%
  mutate(phylum = coalesce(phylum, "arthropoda"))  # filling all the NAs in with arthropoda


long_pitfall <- long_pitfall %>%
  mutate(phylum = recode(phylum, "annelid" = "annelida")) # editing annelid to be annelida

long_pitfall <- long_pitfall %>%
  mutate(
    genus = case_when(
      is.na(genus) & species == "coccinella_undecimpunctata" ~ "coccinella",
      is.na(genus) & species == "cartodere_bifasciata" ~ "cartodere",
      is.na(genus) & species == "peris_rapae" ~ "pieris",                          # peris_rapae is spelled wrong, it should be pieris_rapae
      is.na(genus) & species == "oponga_omoscopa" ~ "oponga",
      is.na(genus) & species == "notagonum_submetallicum" ~ "notagonum",
      is.na(genus) & species == "anisodactylus_biontatus" ~ "anisodactylus",
      is.na(genus) & species == "harpalus_affinis" ~ "harpalus",
      is.na(genus) & species == "mecodema_moniliferum" ~ "mecodema",
      is.na(genus) & species == "sitona_lepidus" ~ "sitona",
      is.na(genus) & species == "labarrus_lividus" ~ "labarrus",
      is.na(genus) & species == "monocrepidius_exsul" ~ "monocrepidius",
      is.na(genus) & species == "epitimetes_lutosus" ~"epitimetes",
      is.na(genus) & species == "hylastes_ater" ~ "hylastes",
      is.na(genus) & species == "otiorhynchus_rugosostriatus" ~ "otiorhynchus",
      is.na(genus) & species == "otiorhynchus_ovatus" ~ "otiorhynchus",
      is.na(genus) & species == "pittosporobius_crassus" ~ "pittosporobius",
      is.na(genus) & species == "listronotus.bonariensis" ~ "listronotus",
      is.na(genus) & species == "acrossidius_tasmaniae" ~ "acrossidius",
      is.na(genus) & species == "naupactus_leucoloma" ~ "naupactus",
      is.na(genus) & species == "allotrochosina_schauinsladi" ~ "allotrochosina",
      is.na(genus) & species == "bobilla_nigrova" ~ "bobilla",
      is.na(genus) & species == "forficula_dentata" ~ "forficula",
      is.na(genus) & species == "stenolemus_fraterculus" ~ "stenolemus",
      is.na(genus) & species == "lithobius_forticatus" ~ "lithobius",
      is.na(genus) & species == "micromus_tasmaniae" ~ "micromus",
      is.na(genus) & species == "pleioplectron_simplex" ~ "pleioplectron",
      is.na(genus) & species == "odiaglyptus biformis" ~ "odiaglyptus",
      is.na(genus) & species == "nyctemera_annulata" ~ "nyctemera",
      is.na(genus) & species == "phalangium_opilio" ~ "phalangium",
      is.na(genus) & species == "nelima_doriae" ~ "nelima",
      is.na(genus) & species == "ophyiulus_pilosus" ~ "ophyiulus",
      is.na(genus) & species == "porcellio scaber" ~ "porcellio",
      TRUE ~ genus
    ),
    
    tribe = case_when(
      is.na(tribe) & genus == "coccinella" ~ "coccinellini",
      is.na(tribe) & genus == "sitona" ~ "sitonini",
      is.na(tribe) & genus == "cartodere" ~ "cartoderini",
      is.na(tribe) & genus == "pieris" ~ "pierini",
      is.na(tribe) & genus == "bobilla" ~ "nemobiini",
      is.na(tribe) & genus == "notagonum" ~ "platynini",
      is.na(tribe) & genus == "anisodactylus" ~ "harpalini",
      is.na(tribe) & genus == "harpalus" ~ "harpalini",
      is.na(tribe) & genus == "mecodema" ~ "broscini",
      is.na(tribe) & genus == "somatidia" ~ "parmenini",
      is.na(tribe) & genus == "labarrus" ~ "aphodiini",
      is.na(tribe) & genus == "xylotoles" ~ "dorcadiini",
      is.na(tribe) & genus == "chalepistes" ~ "tropiphorini",
      is.na(tribe) & genus == "epitimetes" ~ "otiorhynchini",
      is.na(tribe) & genus == "hylastes" ~ "hylastini",
      is.na(tribe) & genus == "otiorhynchus" ~ "otiorhynchini",
      is.na(tribe) & genus == "pittosporobius" ~ "eugnomini",
      is.na(tribe) & genus == "listronotus" ~ "listroderini",
      is.na(tribe) & genus == "acrossidius" ~ "aphodiini",
      is.na(tribe) & genus == "naupactus" ~ "naupactini",
      is.na(tribe) & genus == "odontria" ~ "liparetrini",
      is.na(tribe) & genus == "stenolemus" ~ "emesini",
      is.na(tribe) & genus == "pleioplectron" ~ "macropathini",
      is.na(tribe) & genus == "orocrambus" ~ "crambini",
      is.na(tribe) & genus == "ophyiulus" ~ "leptoiulini",
      is.na(tribe) & genus == "monocrepidius" ~ "oophorini",
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
      is.na(subfamily) & tribe == "platynini" ~ "platyninae",
      is.na(subfamily) & tribe == "harpalini" ~ "harpalinae",
      is.na(subfamily) & tribe == "broscini" ~ "broscinae",
      is.na(subfamily) & tribe == "aphodiini" ~ "aphodiinae",
      is.na(subfamily) & genus == "oophorini" ~ "agrypninae",
      is.na(subfamily) & tribe == "dorcadiini" ~ "lamiinae",
      is.na(subfamily) & tribe == "tropiphorini" ~ "entiminae",
      is.na(subfamily) & tribe == "otiorhynchini" ~ "entiminae", 
      is.na(subfamily) & tribe == "hylastini" ~ "scolytinae",
      is.na(subfamily) & tribe == "eugnomini" ~ "curculioninae",
      is.na(subfamily) & tribe == "listroderini" ~ "cyclominae",
      is.na(subfamily) & tribe == "naupactini" ~ "entiminae",
      is.na(subfamily) & tribe == "liparetrini" ~ "melolonthinae",
      is.na(subfamily) & genus == "steatoda" ~ "latrodectinae",
      is.na(subfamily) & genus == "sidymella" ~ "stephanopinae",
      is.na(subfamily) & genus == "cambridgea" ~ "porteriinae",
      is.na(subfamily) & genus == "allotrochosina" ~ "venoniinae",
      is.na(subfamily) & genus == "algidia" ~ "triaenonychinae",
      is.na(subfamily) & tribe == "alysiini" ~ "alysiinae",
      is.na(subfamily) & genus == "cotes" ~ "lemodinae",
      is.na(subfamily) & genus == "deroceras" ~ "agriolimacinae",
      is.na(subfamily) & genus == "forficula" ~ "forficulinae",
      is.na(subfamily) & tribe == "emesini" ~ "emesinae",
      is.na(subfamily) & tribe == "macropathini" ~ "macropathinae",
      is.na(subfamily) & genus == "odiaglyptus" ~ "tetracneminae",
      is.na(subfamily) & genus == "nyctemera" ~ "arctiinae",
      is.na(subfamily) & tribe == "crambini" ~ "crambinae",
      is.na(subfamily) & genus == "phalangium" ~ "phalangiinae",
      is.na(subfamily) & genus == "nuncia" ~ "triaenonychinae",
      is.na(subfamily) & genus == "nelima" ~ "leiobuninae", 
      is.na(subfamily) & tribe == "leptoiulini" ~ "julinae",
      is.na(subfamily) & tribe == "epuraeini" ~ "epuraeinae",
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
      is.na(family) & subfamily == "platyninae" ~ "carabidae",
      is.na(family) & subfamily == "broscinae" ~ "carabidae",
      is.na(family) & subfamily == "epuraeinae" ~ "nitidulidae",
      is.na(family) & tribe == "parmenini" ~ "cerambycidae",
      is.na(family) & subfamily == "aphodiinae" ~ "scarabaeidae",
      is.na(family) & subfamily == "agrypninae" ~ "elateridae",
      is.na(family) & subfamily == "lamiinae" ~ "cerambycidae",
      is.na(family) & subfamily == "scolytinae" ~ "curculionidae",
      is.na(family) & subfamily == "curculioninae" ~ "curculionidae",
      is.na(family) & subfamily == "cyclominae" ~ "curculioninae",
      is.na(family) & subfamily == "melolonthinae" ~ "scarabaeidae",
      is.na(family) & subfamily == "latrodectinae" ~ "theridiidae",
      is.na(family) & subfamily == "stephanopinae" ~ "thomisidae",
      is.na(family) & subfamily == "porteriinae" ~ "desidae",
      is.na(family) & subfamily == "venoniinae" ~ "lycosidae",
      is.na(family) & subfamily == "triaenonychinae" ~ "triaenonychidae",
      is.na(family) & subfamily == "alysiinae" ~ "braconidae",
      is.na(family) & subfamily == "lemodinae" ~ "anthicidae",
      is.na(family) & subfamily == "deltocephalinae" ~ "cicadellidae",
      is.na(family) & subfamily == "agriolimacinae" ~ "agriolimacidae",
      is.na(family) & subfamily == "forficulinae" ~ "forficulidae",
      is.na(family) & subfamily == "emesinae" ~ "reduviidae",
      is.na(family) & genus == "lithobius" ~ "lithobiidae",
      is.na(family) & genus == "micromus" ~ "hemerobiidae",
      is.na(family) & subfamily == "macropathinae" ~ "rhaphidophoridae",
      is.na(family) & subfamily == "tetracneminae" ~ "encyrtidae",
      is.na(family) & subfamily == "arctiinae" ~ "erebidae",
      is.na(family) & subfamily == "crambinae" ~ "crambidae",
      is.na(family) & subfamily == "orsillinae" ~ "lygaeidae",
      is.na(family) & subfamily == "phalangiinae" ~ "phalangiidae",
      is.na(family) & subfamily == "leiobuninae" ~ "sclerosomatidae",
      is.na(family) & subfamily == "julinae" ~ "julidae",
      is.na(family) & genus == "porcellio" ~ "porcellionidae",
      is.na(family) & subfamily == "staphylininae" ~ "staphylinidae",
      TRUE ~ family                                                               
    ), 
    
    order = case_when(
      family %in% c("cerambycidae", "coccidellidae", "anthicidae", "curculionidae", "latridiidae", "carabidae", "staphylinidae", "nitidulidae", "scarabaeidae", "elateridae") ~ "coleoptera",
      family %in% c("pieridae", "tineidae", "hepialidae", "erebidae", "crambidae") ~ "lepidoptera",
      family %in% c("lycosidae","lynphiidae", "theridiidae", "thomisidae", "tetragnathidae", "gnaphosidae", "salticidae", "desidae") ~ "araneae",
      family %in% c("trigonidiidae", "rhaphidophoridae") ~ "orthoptera",
      family %in% c("phoridae", "muscidae", "sciaridae") ~ "diptera",
      family %in% c("chrysopidae", "hemerobiidae") ~ "neuroptera",
      family %in% c("reduviidae", "aphidae", "cicadellidae", "miridae", "lygaeidae") ~ "hemiptera",
      family %in% c("triaenonychidae", "phalangiidae", "sclerosomatidae") ~ "opiliones",
      family %in% c("braconidae", "ichneumonioidea", "proctotrupidae", "encyrtidae") ~ "hymenoptera",
      family %in% c("agriolimacidae") ~ "stylommatophora",
      family %in% c("forficulidae") ~ "dermaptera",
      family %in% c("henicopidae", "lithobiidae") ~ "lithobiomorpha",
      family %in% c("talitridae") ~ "amphipoda",
      species %in% c("wingless_diptera", "unknown_diptera") ~ "diptera",
      family %in% c("julidae") ~ "julida",
      family %in% c("porcellionidae") ~ "isopoda",
      TRUE ~ order
    ),
    
    superorder = case_when(
      order %in% c("amphipoda") ~ "peracarida",
      order %in% c("coleoptera", "lepidoptera", "diptera", "hymenoptera") ~ "endopterygota",
      order %in% c("opiliones") ~ "parasitiformes",
      order %in% c("heuroptera") ~ "neuropterida",
      order %in% c("hemiptera") ~ "codylognatha",
      order %in% c("orthoptera") ~ "orthopterida",
      order %in% c("julida") ~ "juliformia",
      TRUE ~ superorder
    ),
    
    class = case_when(
      order %in% c("coleoptera", "lepidoptera", "orthoptera", "diptera", "neuroptera", "hemiptera", "hymenoptera", "dermaptera", "thysanoptera") ~ "insecta",
      order %in% c("araneae", "opiliones") ~ "arachnida",
      superorder %in% c("mite", "mites") ~ "arachnida",
      order %in% c("amphipoda", "isopoda") ~ "malacostraca",
      order %in% c("lithobiomorpha") ~ "chilopoda",
      order %in% c("stylommatophora") ~ "gastropoda",
      order %in% c("julida") ~ "diplopoda",
      TRUE ~ class
    ),
    
    phylum = case_when(
      order %in% c("stylommatophora") ~ "mollusca",
      TRUE ~ phylum
    )
  )

# add LTU column #
long_pitfall <- long_pitfall %>%
  mutate(across(c(phylum, class, superorder, order, family, subfamily, tribe, genus, species),
                ~ na_if(trimws(as.character(.x)), ""))) %>%
  mutate(
    LTU = coalesce(species, genus, tribe, subfamily, family, order, superorder, class, phylum),
    LTU_rank = case_when(
      !is.na(species)    ~ "species",
      !is.na(genus)      ~ "genus",
      !is.na(tribe)      ~ "tribe",
      !is.na(subfamily)  ~ "subfamily",
      !is.na(family)     ~ "family",
      !is.na(order)      ~ "order",
      !is.na(superorder) ~ "superorder",
      !is.na(class)      ~ "class",
      !is.na(phylum)     ~ "phylum",
      TRUE ~ "unidentified"
    )
  )

# Quick check: how many individuals landed at each rank
long_pitfall %>% count(LTU_rank, sort = TRUE)


#### Save cleaned data ####
## Save light, long_pan, and long_pitfall into .xlsx files ####
my_list <- list(
  long_pitfall = long_pitfall,
  long_pan = long_pan,
  light = light
)

write_xlsx(my_list, "/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_Data_R_Version.xlsx")


