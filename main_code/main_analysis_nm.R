
# ------------------------
# Load required libraries
# ------------------------
library(gamlss)
library(ggplot2)

# ------------------------
# Load and preprocess data
# ------------------------

# Load dataset
M_HC <- read.csv("xxx/all.csv")

# Filter: quality & age range
M_HC <- subset(M_HC, M > 0.1538 & Age < 85)

# Keep relevant columns and rename
M_HC2 <- M_HC[, c(1, 2, 3, 4, 20, 21, 22, 23, 24)]
colnames(M_HC2)[4] <- "cortex"

# Filter: only cognitively normal (CN) participants
M_HC2 <- subset(M_HC2, group == "CN")

# Convert data types
M_HC2$cortex <- as.numeric(M_HC2$cortex)
M_HC2$Age <- as.numeric(M_HC2$Age)
M_HC2$Gender <- as.factor(trimws(M_HC2$Gender))
M_HC2$site <- as.factor(M_HC2$study)
M_HC2$sequence <- as.factor(M_HC2$sequence)

# Remove NA and extremely low CBF
M_HC2 <- na.omit(M_HC2)
M_HC2 <- subset(M_HC2, cortex > 15)

# ------------------------
# Step 1: Select best distribution family using BIC
# ------------------------

all_families <- c("NO", "GA", "IG", "BCCG", "BCPE", "BCT", "GB2", "GG", 
                  "GIG", "JSU", "TF", "LOGNO", "EXP", "exGAUS", "EGB2", 
                  "WEI", "IGAMMA", "PE", "PE2", "SHASH", "SEP1", "SEP2", 
                  "SEP3", "SEP4", "ST1", "ST2", "ST3", "ST4", "ST5", 
                  "PARETO2", "GU", "RG", "SIMPLEX")

bic_values <- numeric(length(all_families))
con <- gamlss.control(n.cyc = 200)

for (i in seq_along(all_families)) {
  cat("Testing family:", all_families[i], "\n")
  try({
    mod_tmp <- gamlss(
      cortex ~ bs(Age) + Gender + sequence + random(as.factor(site)),
      sigma.fo = ~ bs(Age) + Gender,
      nu.fo = ~ 1, tau.fo = ~ 1,
      family = all_families[i],
      data = M_HC2,
      control = con
    )
    bic_values[i] <- BIC(mod_tmp)
  }, silent = TRUE)
}

best_family <- all_families[which.min(bic_values)]
cat("Selected best family:", best_family, "\n")

# ------------------------
# Step 2: Tune smoothing (df) using the selected family
# ------------------------

df_range <- 4:9
model_list <- list()
bic_df <- numeric(length(df_range))

for (i in seq_along(df_range)) {
  df <- df_range[i]
  cat("Fitting df =", df, "\n")
  model_list[[i]] <- gamlss(
    cortex ~ bs(Age, df = df) + Gender + sequence + random(as.factor(site)),
    sigma.fo = ~ bs(Age) + Gender,
    nu.fo = ~ 1, tau.fo = ~ 1,
    family = best_family,
    data = M_HC2,
    control = con
  )
  bic_df[i] <- BIC(model_list[[i]])
}

best_df_index <- which.min(bic_df)
best_model <- model_list[[best_df_index]]
cat("Selected best df:", df_range[best_df_index], "\n")

# ------------------------
# Step 3: Compute site-specific centile curves
# ------------------------

quantiles <- c(0.05, 0.5, 0.95)
sites <- levels(M_HC2$site)
n_sites <- length(sites)
n_q <- length(quantiles)
n_points <- 4000
age_grid <- seq(0, 85, length.out = n_points)

Centiles_female <- array(NA, dim = c(n_sites, n_q, n_points))
Centiles_male <- array(NA, dim = c(n_sites, n_q, n_points))

for (s in seq_along(sites)) {
  for (q in seq_along(quantiles)) {
    # Female
    Centiles_female[s, q, ] <- getQuantile(
      best_model, quantile = quantiles[q], term = "Age",
      fixed.at = list(Gender = "F", site = sites[s]),
      n.points = n_points
    )(age_grid)
    
    # Male
    Centiles_male[s, q, ] <- getQuantile(
      best_model, quantile = quantiles[q], term = "Age",
      fixed.at = list(Gender = "M", site = sites[s]),
      n.points = n_points
    )(age_grid)
  }
}

# ------------------------
# Step 4: Compute average centile trajectories
# ------------------------

Centiles_female_mean <- apply(Centiles_female, c(2, 3), mean, na.rm = TRUE)
Centiles_male_mean <- apply(Centiles_male, c(2, 3), mean, na.rm = TRUE)
Centiles_avg <- (Centiles_female_mean + Centiles_male_mean) / 2  # Overall average

# ------------------------
# Step 5: Extract age contribution
# ------------------------

term_contrib <- predict(best_model, what = c("mu", "sigma", "nu", "tau"),
                        newdata = M_HC2, type = "term")
Age_Term <- M_HC2$cortex - rowSums(term_contrib)
M_HC2$Age_term <- Age_Term
M_HC2$Normalized_Cortex <- (M_HC2$Age_term / max(Age_Term)) * 100

