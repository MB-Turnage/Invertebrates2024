###############################################################################################

# Project: Invertebrate Biodiversity in Farmland Hedgerow Study
# Author: Mary Buford Turnage
# Date Created: 2025-03-19
# Last Modified: 2025-03-19
# Version Control: GitHub Repo: https://github.com/MB-Turnage/Invertebrates2024.git

###############################################################################################

##########################
##### Data Cleaning ######
##########################

# install.packages("janitor") # If needed #
library(janitor)

# install.packages("openxlsx") # If needed #
library(openxlsx)

# install.packages("cli") #If needed #
library(cli)

## Load necessary packages ##
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(stringr)


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
pan[1:33, 6:41][is.na(pan[1:33, 6:41])] <- 0 

## Light is in a different format, so checking if the count column is string ##
str(light)

## Converting abundance column to numeric ##
light$abundance <- as.numeric(light$abundance)

## Replacing blanks or NAs with zeros ##
light$abundance[is.na(light$abundance)] <- 0


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

## Adding taxonomic columns to match long_pan ##

light <- light %>%
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
  relocate(phylum, .after = date) %>%
  relocate(class, .after = phylum) %>%
  relocate(superorder, .after = class) %>%
  relocate(order, .after = superorder) %>%
  relocate(family, .after = order) %>%
  relocate(subfamily, .after = family)%>%
  relocate(tribe, .after = subfamily) %>%
  relocate(genus, .after = tribe)

## Filling in taxonomic values for light trap datasheet ##

light <- light %>%
  mutate(phylum = coalesce(phylum, "arthropoda")) # filling all the NAs in with arthropoda

light <- light %>%
  mutate(class = coalesce(class, "insecta"))

## Changing "rove" to staphylinidae and other small corrections ##
light <- light %>%
  mutate(species = ifelse(species == "Rove", "staphylinidae", species),         # Replace "Rove" with staphylinidae in species column
         family = ifelse(species == "staphylinidae", "staphylinidae", family),  # Move staphylinidae to family column
         species = ifelse(species == "staphylinidae", NA, species))             # Replace staphylinidae with NA

light <- light %>%
  mutate(species =ifelse(species == "	Diving Beelte", "dytiscidae", species),
         family = ifelse(species == "dytiscidae", "dytiscidae", family),
         species = ifelse(species == "dytiscidae", NA, species))

# Moving values to genus #
light <- light %>%
  mutate(
    genus = ifelse(species %in% c("Pseudocoremia", "Odontria", "Amblyptilia", "Capua"), species, genus), 
    species = ifelse(species %in% c("Pseudocoremia", "Odontria", "Amblyptilia", "Capua"), "NA", species))

# Moving values to family #
light <- light %>%
  mutate(
    family = ifelse(species %in% c("Geometridae", "Crambidae"), species, family),
    species = ifelse(species %in% c("Geometridae", "Crambidae"), "NA", species)
  )

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


light <- light %>%
  mutate(
    order = str_replace_all(order, c(
      "Coleopetera" = "Coleoptera",
      "Coleoptera" = "Coleoptera",
      "Coleotpera" = "Coleoptera",
      "coleoptera" = "Coleoptera"
    ))
  )

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

## Save light and long_pan into new excel copies ##


######################################################
##### Initial Pan and Light Trap Data Exploring ######
#####################################################

## Summarize by order and habitat type ##
light_summary <- light %>%
  group_by(location, order) %>%    # Group by habitat type and order
  summarize(TotalAbundance =sum(abundance))  # Sum abundance within each group

ggplot(light_summary, aes(x = location, y = TotalAbundance, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") + # Use position = "dodge" for grouped bars
  labs(
    title = "Light Trap Insect Abundance by Order and Habitat Type",
    x = "Habitat Type",
    y = "Total Abundance"
  ) +
  theme_minimal()

pan_summary <- long_pan %>%
  mutate(abundance = replace_na(abundance, 0))

pan_summary <- pan_summary %>%
  group_by(habitat_type, order) %>%    # Group by habitat type and order
  summarize(TotalAbundance =sum(abundance))  # Sum abundance within each group

ggplot(pan_summary, aes(x = habitat_type, y = TotalAbundance, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") + # Use position = "dodge" for grouped bars
  labs(
    title = "Pan Trap Insect Abundance by Order and Habitat Type",
    x = "Habitat Type",
    y = "Total Abundance"
  ) +
  theme_minimal()

### Plot diversity ###

## Calculate order diversity ##
pan_richness <- long_pan %>%
  group_by(habitat_type) %>%
  summarize(OrderRichness = n_distinct(order))  # Count unique orders

light_richness <- light %>%
  group_by(location) %>%
  summarize(OrderRichness = n_distinct(order))





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
