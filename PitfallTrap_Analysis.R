#### Pitfall Trap Analysis ####

#### Load packages & data ####
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

# long dataset with hedgerow metrics
long_pitfall <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Code/Chapter 2/Invertebrates_2024/Invertebrates_2024/pitfall_hedge_data.csv")

#### Build farm x location x LTU matrix ####
# Asks which taxa are where, as opposed to how many
# One row per sample, one column per LTU, counts in cells

habitat_types <- c("Native", "NonNative", "Field", "Fence")

build_matrix <- function(long_pitfall) {
  farm_list <- unique(na.omit(long_pitfall$farm))
  scaffold <- expand_grid(farm = farm_list, habitat_type = habitat_types)
  
  counts <- long_pitfall %>%
    filter(!is.na(LTU)) %>%
    group_by(farm, habitat_type, LTU) %>%
    summarise(abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = LTU, values_from = abundance, values_fill = 0)
  
  scaffold %>%
    left_join(counts, by = c("farm", "habitat_type")) %>%
    mutate(across(-c(farm, habitat_type), ~ replace_na(.x, 0)))
}

pitfall_matrix <- build_matrix(pitfall_hedge_data)

#
# checks before using
dim(pitfall_matrix)                                  # 32 rows; columns = 2 + number of LTUs
rowSums(pitfall_matrix[, -c(1, 2)])                  # total abundance
rowSums(pitfall_matrix[, -c(1, 2)] > 0)              # per-row richness
sum(colSums(pitfall_matrix[, -c(1, 2)]) == 0)        # LTUs with zero total, should be 0 or explainable

# for each farm x habitat type in long_pitfall, adds up insects and counts LTUs 
check <- pitfall_hedge_data %>%
  filter(abundance > 0) %>%
  group_by(farm, habitat_type) %>%
  summarise(total_abundance = sum(abundance), richness = n_distinct(LTU), .groups = "drop")

taxa_cols <- setdiff(names(pitfall_matrix), c("farm", "habitat_type"))

# looks at finished matrix and adds the numbers across columns and counts how many columns are above zero
matrix_check <- pitfall_matrix %>%
  mutate(total_abundance = rowSums(pick(all_of(taxa_cols))),
         richness        = rowSums(pick(all_of(taxa_cols)) > 0)) %>%
  select(farm, habitat_type, total_abundance, richness)

full_join(check, matrix_check, by = c("farm", "habitat_type"),
          suffix = c("_long", "_matrix")) %>%
  filter(total_abundance_long != total_abundance_matrix |
           richness_long != richness_matrix)

# check and matrix_check should give the same answer - checking that total abundance and richness both match #

#### Prepare matrix for vegan ####
# Split matrix into metadata columns (farm, habitat) and numeric community

pitfall_comm <- pitfall_matrix %>% select(-farm, -habitat_type)
pitfall_meta <- pitfall_matrix %>% select(farm, habitat_type)


