library(ggplot2)
library(dplyr)
library(tibble)
library(ggseg) 
library(ggsegHO) 

# 1. Define region data and corresponding values (e.g., p-values or statistics)
someData <- tibble(
  region = c(
    "Frontal Pole", "Insular Cortex", "Sup. Frontal Gyrus", "Mid. Frontal Gyrus", 
    "Inf. Frontal Gyrus pars triangularis", "Inf. Frontal Gyrus pars opercularis",
    "Precentral Gyrus", "Temporal Pole", "Sup. Temporal Gyrus ant.",
    "Sup. Temporal Gyrus post.", "Mid. Temporal Gyrus ant.",
    "Mid. Temporal Gyrus post.", "Mid. Temporal Gyrus temporooccipital",
    "Inf. Temporal Gyrus ant.", "Inf. Temporal Gyrus post.",
    "Inf. Temporal Gyrus temporooccipital", "Postcentral Gyrus", "Sup. Parietal Lobule",
    "Supramarginal Gyrus ant.", "Supramarginal Gyrus post.", "Angular Gyrus",
    "Lat. Occipital Cortex superior", "Lat. Occipital Cortex inf.",
    "Intracalcarine Cortex", "Frontal Medial Cortex", "Juxtapositional Lobule Cortex",
    "Subcallosal Cortex", "Paracingulate Gyrus", "Cingulate Gyrus ant.",
    "Cingulate Gyrus post.", "Precuneous Cortex", "Cuneal Cortex", "Frontal Orbital Cortex",
    "Parahippocampal Gyrus ant.", "Parahippocampal Gyrus post.", "Lingual Gyrus",
    "Temporal Fusiform Cortex ant.", "Temporal Fusiform Cortex post.",
    "Temporal Occipital Fusiform Cortex", "Occipital Fusiform Gyrus", "Frontal Operculum Cortex",
    "Central Opercular Cortex", "Parietal Operculum Cortex", "Planum Polare", 
    "Heschl s Gyrus includes H1 and H2 ", "Planum Temporale", "Supracalcarine Cortex", 
    "Occipital Pole"
  ),
  p = mean_cluster_1  # Replace with your vector of values (length = 48)
)

# 2. Merge region values with HO atlas
HO <- hoCort
newAtlas <- HO %>% 
  as_tibble() %>% 
  left_join(someData, by = "region") %>% 
  as_brain_atlas()


# 3. Generate ROI heatmap
p <- ggplot() +
  geom_brain(
    atlas = newAtlas,
    mapping = aes(fill = p),
    position = position_brain(hemi ~ side)
  ) +
  scale_fill_gradientn(
    colors = c("#0000FF", "#00FFFF", "#00FF00", "#FFFF00", "#FF0000"),
    limits = c(xxx, xxx),
    name = "Deviation"
  ) +
  theme_void() +
  theme(
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "right"
  )

