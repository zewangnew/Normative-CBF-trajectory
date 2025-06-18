library(gamlss)
library(splines)

n_bootstrap <- 1000  # Number of bootstrap repetitions
age_intervals <- 10  # Age intervals for stratified sampling
quantiles <- c(0.05, 0.5, 0.95)
n_points <- 4000     # Number of points for curve prediction

Centiles_female_boot <- array(NA, dim = c(n_bootstrap, length(quantiles), n_points))
Centiles_male_boot <- array(NA, dim = c(n_bootstrap, length(quantiles), n_points))

x_age <- seq(0, 85, length.out = n_points)

M_HC2$age_group <- cut(M_HC2$Age, breaks = age_intervals, include.lowest = TRUE)
stratified_groups <- split(M_HC2, list(M_HC2$age_group, M_HC2$Gender))

set.seed(123)  # For reproducibility

for (b in 1:n_bootstrap) {
  print(paste("Bootstrap iteration:", b))
  
  boot_data <- do.call(rbind, lapply(stratified_groups, function(df) {
    df[sample(nrow(df), replace = TRUE), ]
  }))

  mod_boot <- gamlss(
    cortex ~ bs(Age, df = 8) + Gender + random(as.factor(site)), 
    sigma.fo = ~bs(Age) + Gender, 
    nu.fo = ~1, 
    tau.fo = ~1, 
    family = JSU, 
    data = boot_data, 
    control = con
  )
  
  for (i in 1:length(quantiles)) {
    # Female
    Qua_female <- getQuantile(mod_boot, quantile = quantiles[i], term = 'Age', fixed.at = list(Gender = 'F'), n.points = n_points)
    Centiles_female_boot[b, i, ] <- Qua_female(x_age)
    
    # Male
    Qua_male <- getQuantile(mod_boot, quantile = quantiles[i], term = 'Age', fixed.at = list(Gender = 'M'), n.points = n_points)
    Centiles_male_boot[b, i, ] <- Qua_male(x_age)
  }
}

Centiles_female_CI <- apply(Centiles_female_boot, c(2, 3), function(x) c(mean = mean(x), CI_low = quantile(x, 0.025), CI_high = quantile(x, 0.975)))
Centiles_male_CI <- apply(Centiles_male_boot, c(2, 3), function(x) c(mean = mean(x), CI_low = quantile(x, 0.025), CI_high = quantile(x, 0.975)))

Centiles_female_mean <- apply(Centiles_female_boot, c(2, 3), mean)
Centiles_male_mean <- apply(Centiles_male_boot, c(2, 3), mean)
Centiles_mean <- (Centiles_female_mean + Centiles_male_mean) / 2

saveRDS(combined_boot, file = "xxx.rds")

