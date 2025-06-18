# Load required libraries
library(gamlss)
library(dplyr)

M_HC2$age_group <- cut(
  M_HC2$Age, 
  breaks = seq(0, 85, by = 5), 
  include.lowest = TRUE, 
  right = TRUE
)
age_group_summary <- M_HC2 %>%
  group_by(age_group) %>%
  summarise(
    participant_count = n(),                    
    site_count = n_distinct(site)                
  ) %>%
  arrange(age_group)

n_iterations <- 1000          
n_points <- 4000             
X <- seq(0, 85, length.out = n_points)  

female_curves <- array(NA, dim = c(n_iterations, n_points)) 
male_curves <- array(NA, dim = c(n_iterations, n_points))
peak_ages_female <- numeric(n_iterations)
peak_ages_male <- numeric(n_iterations)
set.seed(123)  # For reproducibility
target_sample_size <- xxx  # Adjust based on your needs

age_groups <- unique(M_HC2$age_group)

sample_age_group <- function(one_group, data, target_size = 108) {
  grp_data <- subset(data, age_group == one_group)
  

  if (nrow(grp_data) > target_size) {
    grp_data[sample(nrow(grp_data), target_size), ]
  } else {
    grp_data
  }
}

X <- seq(0, 85, length.out = n_points)

for (i in 1:n_iterations) {
  cat("Iteration:", i, "\n")
  
  balanced_sample <- do.call(
    rbind,
    lapply(age_groups, sample_age_group, data = M_HC2, target_size = target_sample_size)
  )  
  mod_gamlss <- gamlss(
    cortex ~ bs(Age, df = 8) + Gender + sequence + random(as.factor(site)),
    sigma.fo = ~bs(Age) + Gender,
    nu.fo = ~1,
    tau.fo = ~1,
    family = JSU,
    data = balanced_sample, 
    control = con
  )
  pred_curve_female <- getQuantile(mod_gamlss, quantile = 0.5, term = 'Age', 
                                   fixed.at = list(Gender = 'F'), n.points = 1000)
  female_curves[i, ] <- pred_curve_female(X)
  
  pred_curve_male <- getQuantile(mod_gamlss, quantile = 0.5, term = 'Age', 
                                 fixed.at = list(Gender = 'M'), n.points = n_points)
  male_curves[i, ] <- pred_curve_male(X)
  peak_ages_female[i] <- X[which.max(female_curves[i, ])]
  peak_ages_male[i] <- X[which.max(male_curves[i, ])]
  
}

female_median <- apply(female_curves, 2, median)
female_lower <- apply(female_curves, 2, function(x) quantile(x, 0.025))
female_upper <- apply(female_curves, 2, function(x) quantile(x, 0.975))

male_median <- apply(male_curves, 2, median)
male_lower <- apply(male_curves, 2, function(x) quantile(x, 0.025))
male_upper <- apply(male_curves, 2, function(x) quantile(x, 0.975))

peak_age_female_median <- median(peak_ages_female)
peak_age_female_ci <- quantile(peak_ages_female, c(0.025, 0.975))

peak_age_male_median <- median(peak_ages_male)
peak_age_male_ci <- quantile(peak_ages_male, c(0.025, 0.975))
results <- data.frame(
  female = female_curves,
  male = male_curves
)
saveRDS(results, "xxx.rds")

