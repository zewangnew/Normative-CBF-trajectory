#plot progression of subtype
cdrsb_summary <- combined_cdr_data %>%
  group_by(Cluster, TimePoint) %>%
  summarize(
    Mean_CDRSB = mean(MMSCORE, na.rm = TRUE),
    SE_CDRSB = sd(MMSCORE, na.rm = TRUE) / sqrt(n())
  )

cdrsb_summary$Cluster <- as.factor(cdrsb_summary$Cluster)
p <- ggplot(cdrsb_summary, aes(x = TimePoint, y = Mean_CDRSB, color = Cluster, group = Cluster)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = Mean_CDRSB - SE_CDRSB, ymax = Mean_CDRSB + SE_CDRSB), width = 0.2) +
  scale_x_continuous(breaks = 0:5) +
  labs(
    title = "CDR-SB Progression Over Time",
    x = "Time from Baseline (Years)",
    y = "CDR-SB Score",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = 0.8, color = "black"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

conversion_summary <- combined_cdr_data1 %>%
  group_by(Cluster, TimePoint) %>%
  summarize(
    Total_MCI = sum(DIAGNOSIS == 2),
    Converted_AD = sum(DIAGNOSIS == 3),
    Conversion_Rate = Converted_AD / (Total_MCI +Converted_AD)
  ) %>%
  na.omit()
conversion_summary <- conversion_summary %>%
  filter(TimePoint != 4)
p <- ggplot(conversion_summary, aes(x = TimePoint, y = Conversion_Rate, color = Cluster, group = Cluster)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Conversion Rate of MCI to AD Over Time",
    x = "Time from Baseline (Years)",
    y = "Conversion Rate",
    color = "Cluster"
  ) +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    axis.line = element_line(size = 0.8, color = "black"),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#violin plot
library(gghalves)


ad_data <- filtered_data %>%
  filter(group1 == "MCIP") %>%
  dplyr::select(BaseName, TimePoint, z1_1, z1_2, z1_3, z1_4, z1_5, z1_6, z1_7, z1_8, z1_9, z1_10) %>%
  filter(TimePoint %in% c(0, 2))  # Only include TimePoint 0 and 2


# Pivot to long format
ad_data_long <- ad_data %>%
  pivot_longer(
    cols = c(z1_1, z1_2, z1_3, z1_4, z1_5, z1_6, z1_7, z1_8, z1_9, z1_10),
    names_to = "Region",
    values_to = "Value"
  ) %>%
  mutate(
    Region = factor(
      Region,
      levels = c("z1_1", "z1_2", "z1_3", "z1_4", "z1_5", "z1_6", "z1_7", "z1_8", "z1_9", "z1_10"),
      labels = c("Cortex", "WM", "SUB", "", "SSN", "DAN", "VAN", "AN", "FPN", "DMN")  
    )
  )

# Create a half-violin and box plot
p <- ggplot(ad_data_long, aes(x = Region, y = Value, fill = factor(TimePoint))) +
  geom_half_violin(trim = TRUE,
                   side = "r",  # Right-side violin
                   alpha = 0.6,
                   position = position_dodge(width = 0.5) 
  ) +
  geom_boxplot(
    width = 0.2,
    position = position_dodge(width = 0.5),  
    outlier.shape = NA,
    alpha = 0.8
  )+ylim(-3,3)+
  theme_minimal() +
  theme(
    axis.line = element_line(size = 0.8, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )