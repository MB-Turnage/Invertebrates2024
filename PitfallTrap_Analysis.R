#### Pitfall Trap Analysis ####

#### Load packages & data ####
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(vegan)

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

# save as csv
# Export the dataset as a csv for backup
#cd "/Users/marybufordturnage/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Code/Chapter 2/Invertebrates_2024/Invertebrates_2024"write.csv(pitfall_matrix, "~/Library/CloudStorage/OneDrive-UniversityofCanterbury 2/PhD_General/Code/Chapter 2/Invertebrates_2024/Invertebrates_2024/pitfall_matrix.csv")



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

set.seed(42)   # permutations and NMDS starts are random, so this makes results reproducible

# Split matrix into metadata columns (farm, habitat) and numeric community, row order still connects labels to value columns

pitfall_comm <- pitfall_matrix %>%
  select(-farm, -habitat_type) %>%       
  as.data.frame()

rownames(pitfall_comm) <- paste(pitfall_matrix$farm, pitfall_matrix$habitat_type, sep = "_") # adds farm and habitat type back as a labels to keep it easy to read

pitfall_meta <- pitfall_matrix %>%
  select(farm, habitat_type) %>%                                                             # sets habitat_type and farm as factors/categories
  mutate(farm = factor(farm),
         habitat_type = factor(habitat_type,
                               levels = c("Native", "NonNative", "Fence", "Field")))         # setting levels controls plotting and reference order


# Bray-Curtis is undefined for an all-zero row (you checked this: none are empty) # dont understand this
stopifnot(all(rowSums(pitfall_comm) > 0))


#### Transform and build dissimilarity matrix ####

pitfall_t    <- sqrt(pitfall_comm)                     # sqrt transforms counts
pitfall_bray <- vegdist(pitfall_t, method = "bray")    # calculates Bray-Curtis dissimilarity between every pair of samples (0 = identical composition, 1 = no shared taxa)

#### NMDS ####
# to check/visualize dispersion and location effects

# NMDS
nmds_pitfall <- metaMDS(pitfall_t, distance = "bray", k = 2,
                trymax = 100, autotransform = FALSE)

# extract NMDS scores
nmds_pitfall_scores <- as.data.frame(scores(nmds_pitfall, display = "sites")) %>%
  bind_cols(pitfall_meta)

head(nmds_pitfall_scores)

# find the centroids
group_centroids <- nmds_pitfall_scores %>%
  group_by(habitat_type) %>%
  summarise(Centroid_X = mean(NMDS1),
            Centroid_Y = mean(NMDS2),
            .groups = "drop")

# Attach each sample's centroid so we can draw a line from point to centroid
spider_data <- nmds_pitfall_scores %>%
  left_join(group_centroids, by = "habitat_type")

habitat_cols <- c("Native" = "#1b7837", "NonNative" = "#d95f02",
                  "Fence" = "#7570b3", "Field" = "#8c510a")

ggplot() +
  # ellipses
  stat_ellipse(data = nmds_pitfall_scores,
               aes(NMDS1, NMDS2, group = habitat_type, fill = habitat_type),
               geom = "polygon", type = "norm", level = 0.95,
               alpha = 0.12, colour = "black", linewidth = 0.4) +
  # lines from each sample to its habitat centroid
  geom_segment(data = spider_data,
               aes(x = Centroid_X, y = Centroid_Y, xend = NMDS1, yend = NMDS2,
                   colour = habitat_type),
               linewidth = 0.3, alpha = 0.7) +
  # samples
  geom_point(data = nmds_pitfall_scores,
             aes(NMDS1, NMDS2, colour = habitat_type), size = 3) +
  # centroids
  geom_point(data = group_centroids,
             aes(Centroid_X, Centroid_Y, fill = habitat_type),
             shape = 22, size = 4, colour = "black") +
  scale_colour_manual(values = habitat_cols) +
  scale_fill_manual(values = habitat_cols) +
  coord_equal() +
  labs(colour = "Habitat", fill = "Habitat",
       title = "Pitfall trap community composition",
       subtitle = paste("NMDS, Bray-Curtis, stress =", round(nmds_pitfall$stress, 3))) +
  theme_bw()

# Notes: no clear separation between locations
# Fence is tightest though, NonNative has most spread

nmds3 <- metaMDS(pitfall_t, distance = "bray", k = 3,
                 trymax = 100, autotransform = FALSE)

nmds3$stress # value: 0.125 which indicates that a 3D model fits better

# I can report axes 1 vs 2 and 1 vs 3, report whichever I use and its stress

# check the sheppard plot
stressplot(nmds_pitfall)      
# supports using the ordination, non-metric R2 is 0.968 which is above the 0.95 threshold
# the 3D dhepard plot should be tighter

stressplot(nmds3)

# check the outlier
nmds_pitfall_scores %>% mutate(sample = rownames(pitfall_comm)) %>% arrange(NMDS1) %>% head(3)
rowSums(pitfall_comm)["LI1_NonNative"]
rowSums(pitfall_comm > 0)["LI1_NonNative"]   # its richness
summary(rowSums(pitfall_comm))               # to compare with the other samples

# Note: looks like it has 7 rows with 6 LTUs - little to compare
# Keep outlier in dataset for now, later run PERMDISP and PERMANOVA without it as a sensitivity check, 
# if the conclusions are the same, I can say the result doesn't depend on that one

# checking if farm structures the ordination better
ggplot(nmds_pitfall_scores, aes(NMDS1, NMDS2, colour = farm, shape = habitat_type)) +
  geom_point(size = 3) + theme_bw() # it does not look like it, the farm-blocked PERMANOVA design is correct


#### Making a 3D NMDS plot ####
library(plotly)

# rotatable version

# 3D scores + metadata (nmds3 is the k = 3 solution)
scores3 <- as.data.frame(scores(nmds3, display = "sites")) %>%
  bind_cols(pitfall_meta) %>%
  mutate(sample = paste(farm, habitat_type, sep = "_"))

cent3 <- scores3 %>%
  group_by(habitat_type) %>%
  summarise(across(c(NMDS1, NMDS2, NMDS3), mean), .groups = "drop")

plot_ly() %>%
  add_markers(data = scores3,
              x = ~NMDS1, y = ~NMDS2, z = ~NMDS3,
              color = ~habitat_type, colors = habitat_cols,
              text = ~sample, hoverinfo = "text",
              marker = list(size = 5)) %>%
  add_markers(data = cent3,
              x = ~NMDS1, y = ~NMDS2, z = ~NMDS3,
              color = ~habitat_type, colors = habitat_cols,
              marker = list(size = 10, symbol = "diamond",
                            line = list(color = "black", width = 2)),
              showlegend = FALSE, hoverinfo = "none") %>%
  layout(title = paste("NMDS k = 3, stress =", round(nmds3$stress, 3)),
         scene = list(xaxis = list(title = "NMDS1"),
                      yaxis = list(title = "NMDS2"),
                      zaxis = list(title = "NMDS3")))


#### PERMADISP ####
# tests whether the spread of samples around their habitat’s centroid differs among habitats using Bray-Curtis
library(permute)


### run betadisper

## pitfall_bray = vegdist(sqrt(pitfall_comm), method = "bray") from transform step
bd_centroid <- betadisper(pitfall_bray, pitfall_meta$habitat_type, type = "centroid")            # type = centroid measures each sample's distance to its habitat's centroid, 
bd_centroid

## permutation test
# "if habitat labels mean nothing, how often would I see this result by chance?
set.seed(42)
perm_ctrl <- how(nperm = 999, blocks = pitfall_meta$farm)

permutest(bd_centroid, pairwise = TRUE, permutations = perm_ctrl)

## trying with median
bd_median <- betadisper(pitfall_bray, pitfall_meta$habitat_type, type = "median")            # type =type = median is more sensitive to outliers 
bd_median

perm_ctrl <- how(nperm = 999, blocks = pitfall_meta$farm)

permutest(bd_median, pairwise = TRUE, permutations = perm_ctrl)

# Notes: Dispersion differs among habitats
# centroid gives F = 3.6505, median gives F = 2.6819 (p < 0.05 for both) - both "agree" and give similar average distances to centroid indicating result doesn't hinge on LI1 NonNative outlier

# The pattern is two groups - Native/Non Native significantly more variable than Fence, Field not significantly different than either


## apply Holm correction to pairwise p-values (claude says the values above are unadjusted and idk what that means - find out)
# unadjusted means the p-value taken from a single test at face value (for p = 0.005 if there were truly no difference between habitats, you'd see this result 0.5% of the time)
# adjusted corrects for the fact that I ran several tests (6 pairwise comparisons, each with a = 0.05 (5%) false-positive chance, so across 6 tests that becomes a 26% false postive chance)

pw <- permutest(bd, pairwise = TRUE, permutations = perm_ctrl)$pairwise$permuted
p.adjust(pw, method = "holm")
# only the Native-Fence comparison is still significant

# unrestricted permutations within farms
# CHECK if it is centroid or median, maybe set different names and change code accordingly
set.seed(42)
permutest(bd, pairwise = TRUE, permutations = 999)

# Note: Restricted test matches my design better because it removes farm-farm variation
# It asks: “within a farm, does it matter which habitat a sample came from?”
# and, it looks like I have significant differences between fence and native and non-native yay!

#### PERMANOVA ####
set.seed(42)
perm_ctrl <- how(nperm = 999, blocks = pitfall_meta$farm)

pitfall_perm <- adonis2(pitfall_bray ~ farm + habitat_type,
                        data = pitfall_meta,
                        permutations = perm_ctrl,
                        by = "terms")
pitfall_perm

# pairwise comparison since habitat_type is significant
bray_mat <- as.matrix(pitfall_bray)
pairs <- combn(levels(pitfall_meta$habitat_type), 2, simplify = FALSE)

set.seed(42)
pw <- lapply(pairs, function(p) {
  keep <- pitfall_meta$habitat_type %in% p
  m <- droplevels(pitfall_meta[keep, ])
  d <- as.dist(bray_mat[keep, keep])
  a <- adonis2(d ~ farm + habitat_type, data = m,
               permutations = how(nperm = 999, blocks = m$farm),
               by = "terms")
  data.frame(comparison = paste(p, collapse = " vs "),
             F = a["habitat_type", "F"],
             R2 = a["habitat_type", "R2"],
             p = a["habitat_type", "Pr(>F)"])
}) %>% bind_rows()

pw$p_holm <- p.adjust(pw$p, method = "holm")
pw

## Sensitivity checks - "Does my conclusion depend on a choice I made along the way?

# Presence/absence - converts every count to 1 (caught) or 0 (not caught) before Bray-Curtis, a taxon caught 80 times counts as much as one caught 1
bray_pa <- vegdist(pitfall_comm, method = "bray", binary = TRUE)
set.seed(42)
adonis2(bray_pa ~ farm + habitat_type, data = pitfall_meta,
        permutations = perm_ctrl, by = "terms")
# interpretation: since this test also has habitat type as significant, habitat differ in which taxa occur, not just the number of occurences for each taxa

# Without rare taxa (in fewer than 2 samples)
comm_common <- pitfall_comm[, colSums(pitfall_comm > 0) >= 2]
bray_common <- vegdist(sqrt(comm_common), method = "bray")
set.seed(42)
adonis2(bray_common ~ farm + habitat_type, data = pitfall_meta,
        permutations = perm_ctrl, by = "terms")
# interpretation: result comes from taxa that occur repeatedly

# Without the LI1_NonNative outlier
keep <- rownames(pitfall_comm) != "LI1_NonNative"
bray_no_out <- vegdist(sqrt(pitfall_comm[keep, ]), method = "bray")
meta_no_out <- droplevels(pitfall_meta[keep, ])
set.seed(42)
adonis2(bray_no_out ~ farm + habitat_type, data = meta_no_out,
        permutations = how(nperm = 999, blocks = meta_no_out$farm),
        by = "terms")
# interpretation: the unusual sample did not change the results
