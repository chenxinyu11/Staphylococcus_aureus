library(vegan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(tibble)
library(RColorBrewer)
data_raw <- read.table("C:/shannon1.TXT", 
                       header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data_raw <- data_raw %>%
  mutate(SampleID = paste0(continent, "_", year))
st_data <- data_raw %>% select(-continent, -year, -SampleID)
rownames(st_data) <- data_raw$SampleID
dist_mat <- vegdist(st_data, method = "bray")
years <- sort(unique(data_raw$year))
dist_long_yearly <- data.frame()
for (yr in years) {
  samples_in_year <- data_raw$SampleID[data_raw$year == yr]
  if (length(samples_in_year) > 1) {
    dist_sub <- as.matrix(dist_mat)[samples_in_year, samples_in_year]
    dist_tri <- dist_sub[lower.tri(dist_sub)]
    year_df <- data.frame(
      Year = yr,
      BC = dist_tri
    )
    
    dist_long_yearly <- rbind(dist_long_yearly, year_df)
  }
}

yearly_bc_summary <- dist_long_yearly %>%
  group_by(Year) %>%
  summarise(
    MeanBC = mean(BC, na.rm = TRUE),
    SD = sd(BC, na.rm = TRUE),
    MedianBC = median(BC, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(Year)

yearly_violin_plot <- ggplot(dist_long_yearly, aes(x = factor(Year), y = BC)) +
  geom_violin(aes(fill = factor(Year)), alpha = 0.7, width = 0.8, trim = FALSE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 0.3, alpha = 0.1, color = "gray40") +
  geom_point(data = yearly_bc_summary, 
             aes(y = MeanBC), 
             shape = 18, size = 3, color = "red") +
  labs(x = "Year", 
       y = "Bray-Curtis Distance",
       title = "Yearly Variation in ST Type Diversity",
       subtitle = "Bray-Curtis distances within each year") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Set1"))(length(years))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )
yearly_violin_plot <- yearly_violin_plot +
  geom_text(data = yearly_bc_summary,
            aes(x = factor(Year), y = MeanBC, 
                label = paste0("μ=", round(MeanBC, 3))),
            vjust = -1, color = "red", size = 3, fontface = "bold")
yearly_trend_plot <- ggplot(yearly_bc_summary, aes(x = Year, y = MeanBC)) +
  geom_line(color = "#377EB8", size = 1.2) +
  geom_point(aes(size = Count), color = "#E41A1C", alpha = 0.8) +
  geom_errorbar(aes(ymin = MeanBC - SD, ymax = MeanBC + SD), 
                width = 0.3, color = "#377EB8", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "#4DAF4A", linetype = "dashed") +
  labs(x = "Year", 
       y = "Mean Bray-Curtis Distance",
       title = "Temporal Trend of ST Type Diversity",
       subtitle = "Mean BC distance with standard deviation error bars",
       size = "Sample Pairs") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = years) +
  scale_size_continuous(range = c(2, 6))
if (nrow(yearly_bc_summary) > 2) {
  trend_model <- lm(MeanBC ~ Year, data = yearly_bc_summary)
  trend_summary <- summary(trend_model)
  trend_p <- trend_summary$coefficients[2, 4]
  trend_r2 <- trend_summary$r.squared
  
  trend_text <- paste0("Linear trend: R² = ", round(trend_r2, 3), 
                       ", p = ", ifelse(trend_p < 0.001, "< 0.001", round(trend_p, 3)))
  
  yearly_trend_plot <- yearly_trend_plot +
    annotate("text", x = min(yearly_bc_summary$Year), 
             y = max(yearly_bc_summary$MeanBC) * 0.95,
             label = trend_text, hjust = 0, vjust = 1, 
             color = "#984EA3", size = 4, fontface = "bold")
}

combined_plot <- yearly_violin_plot + yearly_trend_plot + 
  plot_layout(ncol = 1, heights = c(2, 1)) +
  plot_annotation(
    title = "Annual Analysis of ST Type Diversity Patterns",
    subtitle = "Top: Distribution of Bray-Curtis distances within each year\nBottom: Temporal trend of mean diversity",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
                  plot.subtitle = element_text(hjust = 0.5, size = 14))
  )

print(combined_plot)