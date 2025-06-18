# Required libraries
library(gamlss)
library(tidyr)


source("ZZZ_function.R", encoding = "UTF-8")
source("301.functions.r", encoding = "UTF-8")
source("300.variables.r", encoding = "UTF-8")
source("100.common-variables.r", encoding = "UTF-8")
source("101.common-functions.r", encoding = "UTF-8")

M_HC <- read.csv("xxx/all.csv")
M_HC <- subset(M_HC, M > 0.1538 & Age < 85)

M_HC_num <- c(4, 5, 6, 8:13, 25:80)
n_repeats <- 20

# Loop over each region of interest (ROI)
for (i in seq_along(M_HC_num)) {
  message("Processing ROI: ", i)
  
  # Prepare dataset for current ROI
  M_HC2 <- M_HC[, c(1:3, M_HC_num[i], 14, 20:24)]
  names(M_HC2)[4] <- "cortex"
  M_HC2$cortex <- as.numeric(as.character(M_HC2$cortex))
  
  # Identify healthy controls (excluding selected studies)
  M_HC3 <- subset(M_HC2, !study %in% c('adni', 'UMB', 'depconn', 'embarc', 'allftd') & cortex > 15)
  
  # Prepare disease datasets by study
  umb_dataset <- subset(M_HC2, site == 'UMB')
  
  ad_dataset <- subset(M_HC2, study == 'adni')
  ad_dataset$cortex <- as.numeric(as.character(ad_dataset$cortex))
  ad_dataset$Age <- as.numeric(ad_dataset$Age)
  ad_dataset$Gender <- as.factor(trimws(ad_dataset$Gender))
  ad_dataset$site <- as.factor(ad_dataset$study)
  ad_dataset$sequence <- as.factor(ad_dataset$sequence)
  ad_dataset$PLD <- as.factor(ad_dataset$PLD)
  ad_dataset <- na.omit(ad_dataset)
  
  # Extract unique subjects with highest quality (M)
  ad_dataset$Participant <- sub("(_\\d{4}-\\d{2}-\\d{2}_.*)$", "", ad_dataset$BaseName)
  ad_dataset_filtered <- ad_dataset[ave(ad_dataset$M, ad_dataset$Participant, FUN = function(x) x == max(x, na.rm = TRUE)) == 1, ]
  ad_dataset_filtered$Participant <- NULL
  
  dep_dataset <- subset(M_HC2, study %in% c('embarc', 'depconn'))
  ftd_dataset <- subset(M_HC2, site == 'allftd')
  
  dis_dataset <- rbind(ad_dataset_filtered, dep_dataset, ftd_dataset, umb_dataset)
  AD_controls <- subset(dis_dataset, group == 'CN')
  
  # Clean and format datasets
  for (df in list(M_HC3, dis_dataset)) {
    df$cortex <- as.numeric(as.character(df$cortex))
    df$Age <- as.numeric(df$Age)
    df$Gender <- as.factor(trimws(df$Gender))
    df$site <- as.factor(df$site)
    df$sequence <- as.factor(df$sequence)
    df <- na.omit(df)
  }
  
  # Initialize matrices to store z-scores across repetitions
  all_z1 <- matrix(NA, nrow = nrow(dis_dataset), ncol = n_repeats)
  all_z2 <- matrix(NA, nrow = nrow(dis_dataset), ncol = n_repeats)
  
  # Loop for repeated train/test evaluation
  for (repeat_idx in seq_len(n_repeats)) {
    set.seed(123 + repeat_idx)
    
    AD_idx <- sample(seq_len(nrow(AD_controls)), floor(0.5 * nrow(AD_controls)))
    AD_train <- AD_controls[AD_idx, ]
    AD_test  <- AD_controls[-AD_idx, ]
    
    train_dataset <- na.omit(rbind(M_HC3, AD_train))
    test_dataset <- na.omit(rbind(subset(dis_dataset, group != 'CN'), AD_test))
    
    # Fit GAMLSS model
    mod <- gamlss(cortex ~ bs(Age, df = 8) + Gender + sequence + random(as.factor(site)),
                  sigma.fo = ~bs(Age) + Gender,
                  nu.fo = ~1, tau.fo = ~1, family = JSU,
                  data = train_dataset, control = gamlss.control(n.cyc = 200))
    
    # Predict additive terms
    term_effects <- predict(mod, what = c("mu", "sigma", "nu", "tau"), newdata = test_dataset, type = "term")
    Age_Term <- test_dataset$cortex - rowSums(term_effects)
    
    # Extract GAMLSS distribution parameters
    mu <- predict(mod, newdata = test_dataset, what = "mu", type = "response")
    sigma <- predict(mod, newdata = test_dataset, what = "sigma", type = "response")
    nu <- predict(mod, newdata = test_dataset, what = "nu", type = "response")
    tau <- predict(mod, newdata = test_dataset, what = "tau", type = "response")
    
    # Calculate Z and centile scores
    z1 <- zzz_cent(mod, "z-scores", mu, sigma, nu, 'Age', test_dataset$Age, test_dataset$cortex, FALSE, 3)

    matched <- match(test_dataset$BaseName, dis_dataset$BaseName)
    all_z1[matched, repeat_idx] <- z1
  }
  
  # Aggregate across repetitions
  dis_dataset$z1 <- rowMeans(all_z1, na.rm = TRUE)
  dis_dataset$z1_sd <- apply(all_z1, 1, sd, na.rm = TRUE)
  
  # Save results
  saveRDS(all_z1, paste0("xxx/all_z_", i, ".rds"))
  saveRDS(dis_dataset, paste0("xxx/R_JSU_score100_", i, ".rds"))
}