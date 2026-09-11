#### Invert Analysis ###


#### Load packages ####
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

#### Load data ####
master <- readxl::read_excel('/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_Data_R_Version.xlsx')

## Check that all the sheet names are correct ##
sheet_names <- getSheetNames('/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_Data_R_Version.xlsx')
print(sheet_names) # i.e, "long_pitfall", "long_pan", "light"

## Read in each sheet seperately - not sure if I actually need ot have the master in here ##
long_pitfall <- read_excel('/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_Data_R_Version.xlsx', sheet = "long_pitfall")
light <- read_excel('/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_Data_R_Version.xlsx', sheet = "light")
long_pan <- read_excel('/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Inverts_Data_R_Version.xlsx', sheet = "long_pan")

# Convert to tibbles #
long_pitfall<-as_tibble(long_pitfall)
long_pitfall

light<-as_tibble(light)
light

long_pan<-as_tibble(long_pan)
long_pan

hedge_data<-read.csv('/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Data/Data Spreadsheets/Chapter 2/Invert_Spreadsheets/Hedgerows_Inverts.csv')
head(hedge_data)

hedge_data <- as_tibble(hedge_data)


long_pan %>%
  select(farm, habitat_type) %>%
  distinct() %>%
  arrange(farm, habitat_type) %>%
  print(n = 50)



#### Creating columns and calculating mean hedgerow heights ####
hedge_data <- hedge_data %>%
  mutate(                                                               # mutate() is used to add new columns or modify existing ones
    mean_height = rowMeans(                                             # creates new column called mean_height, rowMeans calculates the mean across columns for each row 
      select(., height_at_0m, height_at_25m, height_at_50m),            # select() picks specific columns from the tibble, "." refers to the data coming from hedge_tibble (the data frame for the call to focus on?)
      na.rm = TRUE))                                                    # na.rm = TRUE ignores any missing or NA values when calculating the mean, if "na.rm = FALSE" and a value was "NA", the resulting mean would be "NA"

# Creating columns and calculating mean canopy layers
hedge_data <- hedge_data %>%
  mutate(
    mean_canopy_layers = rowMeans(
      select(., vert_layers_0, vert_layers_25, vert_layers_50),
      na.rm = TRUE
    )
  )

# Prepping data sets for join - changing the names in 
hedge_data <- hedge_data%>% 
  dplyr::rename(
    habitat_type = Type_of_Hedgerow,
    farm = Farm                                                      # new_name = old_name pattern
  ) %>%
  dplyr::mutate(
    habitat_type = case_when(                                         # changes values conditionally
      habitat_type == "native" ~ "Native",
      habitat_type == "nonnative" ~ "NonNative",
      habitat_type == 'mix' ~ "Mix",
      TRUE ~ habitat_type                                             # keep all other values unchanged
    )
  )




# Join mean and mean canopy layers to each invert tibble

# pitfall
pitfall_hedge_data <- long_pitfall %>%
  left_join(
    hedge_data %>%
      select(farm, habitat_type, mean_height, mean_canopy_layers),    # selects four columns from hedge_tibble, creates a smaller version of hedge tibble containing only those variables
    by = c("farm", "habitat_type"),                                                                          # then joins long_pitfall to the smaller version of hedge_tibble, matches rows where Farm and Habitat_Type are the same in both tibbles
    relationship = "many-to-many"                                                                            # tells R that its fine that there are multiple rows in long_pitfall that match each row in hedge_tibble
  )

# Convert bird_hedge_data back to a tibble
pitfall_hedge_data <- as_tibble(pitfall_hedge_data)

# Add values to fill NAs in the fence and field habitat types in mean_height and mean_canopy layers

# Filling in values for fence and field heights #
pitfall_hedge_data <- pitfall_hedge_data %>%
  mutate(
    mean_height = case_when(
      habitat_type == "Fence" & is.na(mean_height) ~ 1,
      habitat_type == "Field" & is.na(mean_height) ~ 0.72, 
      TRUE ~ mean_height
    )
  )

# Filling in values for fence and field canopy layers
pitfall_hedge_data <- pitfall_hedge_data %>%
  mutate(
    mean_canopy_layers = case_when(
      habitat_type == "Fence" & is.na(mean_canopy_layers) ~ 0.5,
      habitat_type == "Field" & is.na(mean_canopy_layers) ~ 0.1,
      TRUE ~ mean_canopy_layers
    )
  )


# Export the dataset as a csv for backup
#write.csv(pitfall_hedge_data, "pitfall_hedge_data")



# pan
pan_hedge_data <- long_pan %>%
  left_join(
    hedge_data %>%
      select(farm, habitat_type, mean_height, mean_canopy_layers),    
    by = c("farm", "habitat_type"),                                                                          
    relationship = "many-to-many"                                                                            
  )

# Convert bird_hedge_data back to a tibble
pan_hedge_data <- as_tibble(pan_hedge_data)

# Add values to fill NAs in the fence and field habitat types in mean_height and mean_canopy layers

# Filling in values for fence and field heights #
pan_hedge_data <- pan_hedge_data %>%
  mutate(
    mean_height = case_when(
      habitat_type == "Fence" & is.na(mean_height) ~ 1,
      habitat_type == "Field" & is.na(mean_height) ~ 0.72, 
      TRUE ~ mean_height
    )
  )

# Filling in values for fence and field canopy layers
pan_hedge_data <- pan_hedge_data %>%
  mutate(
    mean_canopy_layers = case_when(
      habitat_type == "Fence" & is.na(mean_canopy_layers) ~ 0.5,
      habitat_type == "Field" & is.na(mean_canopy_layers) ~ 0.1,
      TRUE ~ mean_canopy_layers
    )
  )


# Export the dataset as a csv for backup
#write.csv(pan_hedge_data, "pan_hedge_data")

# light
light_hedge_data <- light %>%
  left_join(
    hedge_data %>%
      select(farm, habitat_type, mean_height, mean_canopy_layers),    
    by = c("farm", "habitat_type"),                                                                          
    relationship = "many-to-many"                                                                            
  )

# Convert bird_hedge_data back to a tibble
light_hedge_data <- as_tibble(light_hedge_data)

# Add values to fill NAs in the fence and field habitat types in mean_height and mean_canopy layers

# Filling in values for fence and field heights #
light_hedge_data <- light_hedge_data %>%
  mutate(
    mean_height = case_when(
      habitat_type == "Fence" & is.na(mean_height) ~ 1,
      habitat_type == "Field" & is.na(mean_height) ~ 0.72, 
      TRUE ~ mean_height
    )
  )

# Filling in values for fence and field canopy layers
light_hedge_data <- light_hedge_data %>%
  mutate(
    mean_canopy_layers = case_when(
      habitat_type == "Fence" & is.na(mean_canopy_layers) ~ 0.5,
      habitat_type == "Field" & is.na(mean_canopy_layers) ~ 0.1,
      TRUE ~ mean_canopy_layers
    )
  )


# Export the dataset as a csv for backup
write.csv(light_hedge_data, "light_hedge_data")

#### building community matrices to account for zeros ####

# Expanding grid to account for zeros
habitat_types <- c("Native", "NonNative", "Field", "Fence")

summarise_with_zeros <- function(long_data, farm_list) {
  scaffold <- expand_grid(farm = farm_list, habitat_type = habitat_types)
  
  scaffold %>%
    left_join(
      long_data %>%
        filter(abundance > 0) %>%
        group_by(farm, habitat_type, mean_height, mean_canopy_layers) %>%
        summarise(
          total_abundance = sum(abundance, na.rm = TRUE),
          richness = n_distinct(LTU),
          .groups = "drop"
        ),
      by = c("farm", "habitat_type")
    ) %>%
    mutate(
      total_abundance = replace_na(total_abundance, 0),
      richness = replace_na(richness, 0)
    )
}

pitfall_summary <- summarise_with_zeros(pitfall_hedge_data, unique(pitfall_hedge_data$farm))
pan_summary <- summarise_with_zeros(pan_hedge_data, unique(pan_hedge_data$farm))
light_summary <- summarise_with_zeros(light_hedge_data, unique(light_hedge_data$farm))


# troubleshooting species richness errors with pitfall and pan summaries
# It looks like the problem is from them both starting as wider - the sprich finds a column for every taxa present in the dataset even if 
# the taxa wasn't present in a trap. Planning to re-try filtering out abundances that are zero.
pan_hedge_data <- pan_hedge_data %>% filter(!is.na(farm)) # to remove the 

long_pan %>%
  select(farm, habitat_type) %>%
  distinct() %>%
  arrange(farm, habitat_type) %>%
  print(n = 50)

n_distinct(long_pan$LTU)

nrow(pan_hedge_data)
pan_hedge_data %>% filter(is.na(habitat_type)) %>% nrow()
n_distinct(pan_hedge_data$LTU)

nrow(long_pan)

long_pan %>%
  group_by(farm, habitat_type) %>%
  summarise(richness = n_distinct(LTU), total_abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  print(n = 40)

long_pan %>% filter(abundance == 0) %>% nrow()
#
##### Initial summaries#####
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

###


pan_summary <- long_pan %>%
  group_by(habitat_type, order) %>%    # Group by habitat type and order
  summarize(abundance =sum(abundance))  # Sum abundance within each group

ggplot(pan_summary, aes(x = habitat_type, y = abundance, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") + # Use position = "dodge" for grouped bars
  labs(
    title = "Pan Trap Insect Abundance by Order and Habitat Type",
    x = "Habitat Type",
    y = "Total Abundance"
  ) +
  theme_minimal()


###

pitfall_summary <- long_pitfall %>%
  group_by(location, order) %>%    # Group by habitat type and order
  summarize(abundance =sum(abundance))  # Sum abundance within each group

ggplot(pitfall_summary, aes(x = location, y = abundance, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") + # Use position = "dodge" for grouped bars
  labs(
    title = "Pitfall Trap Insect Abundance by Order and Habitat Type",
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

## Summarize by order and habitat type ##
light_orderXhab <- light %>%
  group_by(location, order) %>%    # Group by habitat type and order
  summarize(TotalAbundance =sum(abundance))  # Sum abundance within each group

ggplot(light_orderXhab, aes(x = location, y = TotalAbundance, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") + # Use position = "dodge" for grouped bars
  labs(
    title = "Light Trap Insect Abundance by Order and Habitat Type",
    x = "Habitat Type",
    y = "Total Abundance"
  ) +
  theme_minimal()

pan_orderXhab <- long_pan %>%
  mutate(abundance = replace_na(abundance, 0))

pan_orderXhab <- pan_orderXhab %>%
  group_by(habitat_type, order) %>%    # Group by habitat type and order
  summarize(TotalAbundance =sum(abundance))  # Sum abundance within each group

ggplot(pan_orderXhab, aes(x = habitat_type, y = TotalAbundance, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") + # Use position = "dodge" for grouped bars
  labs(
    title = "Pan Trap Insect Abundance by Order and Habitat Type",
    x = "Habitat Type",
    y = "Total Abundance"
  ) +
  theme_minimal()

pitfall_orderXhab <- long_pitfall %>%
  group_by(`Habitat Type`, order) %>%    # Group by habitat type and order
  summarize(TotalAbundance =sum(abundance, na.rm = TRUE))  # Sum abundance within each group

ggplot(pitfall_orderXhab, aes(x = `Habitat Type`, y = TotalAbundance, fill = order)) +
  geom_bar(stat = "identity", position = "dodge") + # Use position = "dodge" for grouped bars
  labs(
    title = "Pitfall Trap Insect Abundance by Order and Habitat Type",
    x = "Habitat Type",
    y = "Total Abundance"
  ) +
  theme_minimal()

