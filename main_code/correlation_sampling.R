results <- readRDS('xxx.rds')

female_mat <- as.matrix(results$Centiles_female_boot[,2,])
male_mat <- as.matrix(results$Centiles_male_boot[,2,])
X_sampled <- seq(0, 85, length.out = xxx) #dependens on your need
index_sampled <- sapply(X_sampled, function(x) which.min(abs(X - x)))

main_female <- Centiles_female1[2, index_sampled]
main_male <- Centiles_male1[2, index_sampled]

boot_female <- apply(female_mat, 2, median)[index_sampled]
boot_male <- apply(male_mat, 2, median)[index_sampled]

cor_female <- cor(main_female, boot_female, method = "pearson")
cor_male <- cor(main_male, boot_male, method = "pearson")

cat("Female trajectory correlation:", round(cor_female, 4), "\n")
cat("Male trajectory correlation:", round(cor_male, 4), "\n")




growth_female_main <- diff(Centiles_female1[2, ]) / diff(X)
growth_male_main   <- diff(Centiles_male1[2, ]) / diff(X)
X_mid <- (X[-length(X)] + X[-1]) / 2  # Midpoints for derivative values

female_growth <- t(apply(female_mat, 1, function(y) diff(y) / diff(X)))
male_growth   <- t(apply(male_mat, 1, function(y) diff(y) / diff(X)))

boot_rate_female_median <- apply(female_growth, 2, median)
boot_rate_male_median   <- apply(male_growth, 2, median)

X_sampled_rate <- seq(0, 85, length.out = xxx)
index_sampled_rate <- sapply(X_sampled_rate, function(x) which.min(abs(X_mid - x)))

main_rate_female <- growth_female_main[index_sampled_rate]
main_rate_male   <- growth_male_main[index_sampled_rate]

boot_rate_female_sampled <- boot_rate_female_median[index_sampled_rate]
boot_rate_male_sampled   <- boot_rate_male_median[index_sampled_rate]

cor_rate_female <- cor(main_rate_female, boot_rate_female_sampled, method = "pearson")
cor_rate_male   <- cor(main_rate_male, boot_rate_male_sampled, method = "pearson")

cat("✅ Female growth rate correlation (80 points):", round(cor_rate_female, 4), "\n")
cat("✅ Male growth rate correlation (80 points):", round(cor_rate_male, 4), "\n")

