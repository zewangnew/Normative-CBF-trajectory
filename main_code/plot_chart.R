library(ggplot2)

# 1. SCATTER PLOT OF NORMALIZED SUBCORTICAL CBF
scatter_plot <- ggplot(M_HC2, aes(x = Age, y = Normalized_Cortex, color = Gender)) +
  geom_point(size = 3, shape = 16, alpha = 0.8) +
  geom_vline(xintercept = c(0, 7, 18, 50, 80), linetype = "dashed", color = "grey80", size = 1) +
  scale_color_manual(values = c("#E3170A", "#247BA0")) +
  labs(x = "Age (years)", y = "Normalized SUB CBF (max = 100)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25)) +
  scale_x_continuous(breaks = c(0, 7, 18, 50, 80),
                     labels = c("Birth", "7 yrs", "18 yrs", "50 yrs", "80 yrs")) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = 1, color = "black"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )



# 2. NORMATIVE TRAJECTORY PLOT FOR CBF
X <- seq(0, 85, length.out = 4000)


normative_plot <- ggplot(normative_data, aes(x = X)) +
  geom_line(aes(y = Female_Median), color = "#E3170A", size = 1.2) +
  geom_line(aes(y = Female_Lower), linetype = "dashed", color = "#E3170A", size = 1) +
  geom_line(aes(y = Female_Upper), linetype = "dashed", color = "#E3170A", size = 1) +
  geom_line(aes(y = Male_Median), color = "#247BA0", size = 1.2) +
  geom_line(aes(y = Male_Lower), linetype = "dashed", color = "#247BA0", size = 1) +
  geom_line(aes(y = Male_Upper), linetype = "dashed", color = "#247BA0", size = 1) +
  geom_vline(xintercept = c(0, 7, 18, 50, 80), linetype = "dashed", color = "grey80", size = 1) +
  labs(x = "Age (years)", y = "Normalized WM CBF (max = 100)") +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, 25)) +
  scale_x_continuous(breaks = c(0, 7, 18, 50, 80),
                     labels = c("Birth", "7 yrs", "18 yrs", "50 yrs", "80 yrs")) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = 1, color = "black"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


# 3. GROWTH RATE DERIVATIVE PLOT
growth_rate_female <- diff(Centiles_female1[2, ]) / diff(X)
growth_rate_male   <- diff(Centiles_male1[2, ]) / diff(X)
X_midpoints        <- (X[-length(X)] + X[-1]) / 2

growth_rate_data <- data.frame(
  X = X_midpoints,
  Growth_rate_female = growth_rate_female,
  Growth_rate_male = growth_rate_male
)

growth_rate_plot <- ggplot(growth_rate_data, aes(x = X)) +
  geom_line(aes(y = Growth_rate_female), color = "#E3170A", size = 1.2) +
  geom_line(aes(y = Growth_rate_male), color = "#247BA0", linetype = "dashed", size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 1) +
  geom_vline(xintercept = c(0, 7, 18, 50, 80), linetype = "dashed", color = "grey80", size = 1) +
  labs(x = "Age (years)", y = "Growth rate (unit/day)") +
  scale_y_continuous(limits = c(-4, 12), breaks = seq(-4, 12, 4)) +
  scale_x_continuous(breaks = c(0, 7, 18, 50, 80),
                     labels = c("Birth", "7 yrs", "18 yrs", "50 yrs", "80 yrs")) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = 1, color = "black"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# 4. BOOTSTRAP TRAJECTORY PLOT
bootstrap_data <- data.frame(
  X = X,
  Female_Lower = apply(female_mat, 2, function(x) quantile(x, 0.025)) / divisor,
  Female_Upper = apply(female_mat, 2, function(x) quantile(x, 0.975)) / divisor,
  Male_Lower   = apply(male_mat, 2, function(x) quantile(x, 0.025)) / divisor,
  Male_Upper   = apply(male_mat, 2, function(x) quantile(x, 0.975)) / divisor
)

normative_data <- data.frame(
  X = X,
  Female_Median = apply(female_mat, 2, median) / divisor,
  Male_Median = apply(male_mat, 2, median) / divisor
)

bootstrap_plot <- ggplot() +
  geom_ribbon(data = bootstrap_data, aes(x = X, ymin = Female_Lower, ymax = Female_Upper),
              fill = "#E3170A", alpha = 0.2) +
  geom_ribbon(data = bootstrap_data, aes(x = X, ymin = Male_Lower, ymax = Male_Upper),
              fill = "#247BA0", alpha = 0.2) +
  geom_line(data = normative_data, aes(x = X, y = Female_Median), color = "#E3170A", size = 1.2) +
  geom_line(data = normative_data, aes(x = X, y = Male_Median), color = "#247BA0", size = 1.2) +
  labs(x = "Age (years)", y = "Normalized Cortical CBF (max = 100)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
  scale_x_continuous(breaks = c(0, 7, 18, 50, 80),
                     labels = c("Birth", "7 yrs", "18 yrs", "50 yrs", "80 yrs")) +
  geom_vline(xintercept = c(0, 7, 18, 50, 80), linetype = "dashed", color = "grey80", size = 1) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = 1, color = "black"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(bootstrap_plot)
