library(NbClust)
library(tidyr)
library(dplyr)
library(ggplot2)
combined_data <- data.frame(BaseName = character(), stringsAsFactors = FALSE)

for (i in 1:65) {
  pat_data <- readRDS(file = paste0("xxx"))
  

  ad_ds <- pat_data %>% 
    filter(group == "MCI") %>%
    dplyr::select(BaseName, z1) %>%
    distinct(BaseName, .keep_all = TRUE)  # Keep only unique BaseName

  ad_ds <- ad_ds %>% rename(!!paste0("z1_", i) := z1)
  
  combined_data <- combined_data %>%
    full_join(ad_ds, by = "BaseName")
}

combined_data <- combined_data[, c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10)]
kmeans_result <- kmeans(combined_data_no_na, centers = 2, nstart = 25)
combined_data_no_na$Cluster <- kmeans_result$cluster


nb <- NbClust(data = combined_data_no_na, diss = NULL, distance = "euclidean",
              min.nc = 2, max.nc = 8, method = "kmeans", index = "all")

optimal_clusters <- nb$Best.nc[1, "Number_clusters"]
print(paste("Optimal number of clusters:", optimal_clusters))



