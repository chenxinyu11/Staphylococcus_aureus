library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(forcats)
library(stringr)
library(scales)
library(ggnewscale)

assign_period <- function(year) {
  case_when(
    year >= 2001 & year <= 2020 ~ "2001-2020"
    TRUE ~ NA_character_
  )
}
df <- read_tsv("C:/arg_results.txt")
colnames(df) <- tolower(gsub(" ", "_", colnames(df)))

st_filter <- c("ST1"))

global_prev <- df %>%
  mutate(
    year = as.numeric(year),
    period = assign_period(year),
    drug_class = ifelse(str_detect(drug_class, ";"), "Multi-drug", drug_class)
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  mutate(total_global = n_distinct(gca_id)) %>%
  group_by(period, best_hit_aro) %>%
  summarise(positive_global = n_distinct(gca_id),
            total_global = first(total_global),
            .groups = "drop") %>%
  mutate(global_prevalence = (positive_global / total_global) * 100)

top30_args <- global_prev %>%
  group_by(best_hit_aro) %>%
  summarise(mean_prev = mean(global_prevalence, na.rm = TRUE)) %>%
  arrange(desc(mean_prev)) %>%
  slice_head(n = 30) %>%
  pull(best_hit_aro)

df_st <- df %>%
  mutate(
    year = as.numeric(year),
    period = assign_period(year),
    drug_class = ifelse(str_detect(drug_class, ";"), "Multi-drug", drug_class)
  ) %>%
  filter(!is.na(period), st != "ST-", st != "", !is.na(st)) %>%
  filter(st %in% st_filter)

arg_prev_st <- df_st %>%
  group_by(period, st) %>%
  mutate(total_st = n_distinct(gca_id)) %>%
  group_by(period, st, best_hit_aro, drug_class) %>%
  summarise(positive = n_distinct(gca_id),
            total_st = first(total_st),
            .groups = "drop") %>%
  mutate(prevalence = (positive / total_st) * 100) %>%
  filter(best_hit_aro %in% top30_args)  # 使用全局top30 ARG

df_continent <- df %>%
  mutate(
    year = as.numeric(year),
    period = assign_period(year),
    drug_class = ifelse(str_detect(drug_class, ";"), "Multi-drug", drug_class)
  ) %>%
  filter(!is.na(period))

total_samples_continent <- df_continent %>%
  group_by(period, continent) %>%
  summarise(total = n_distinct(gca_id), .groups = "drop")

global_total_continent <- df_continent %>%
  group_by(period) %>%
  summarise(total = n_distinct(gca_id), .groups = "drop") %>%
  mutate(continent = "Global")

total_samples_continent <- bind_rows(total_samples_continent, global_total_continent)

arg_counts_continent <- df_continent %>%
  group_by(period, continent, best_hit_aro, drug_class) %>%
  summarise(positive = n_distinct(gca_id), .groups = "drop")

global_counts_continent <- df_continent %>%
  group_by(period, best_hit_aro, drug_class) %>%
  summarise(positive = n_distinct(gca_id), .groups = "drop") %>%
  mutate(continent = "Global")

arg_counts_continent <- bind_rows(arg_counts_continent, global_counts_continent)

arg_prev_continent <- arg_counts_continent %>%
  left_join(total_samples_continent, by = c("period", "continent")) %>%
  mutate(prevalence = (positive / total) * 100) %>%
  filter(best_hit_aro %in% top30_args)  # 使用全局top30 ARG

drug_classes <- unique(c(arg_prev_st$drug_class, arg_prev_continent$drug_class))

# 莫兰迪色系
muted_colors <- c(
  "#8FA6CB", "#A5C1A5", "#E0C1B3", "#C4AEAD", 
  "#E5D4B3", "#D6A2AD", "#9DBEB9", "#B8A9C9",
  "#C7D3A5", "#D3C0CD", "#A7D1D1", "#D1C7B7"
)

set.seed(123)
color_palettes <- list()
for (i in seq_along(drug_classes)) {
  dc <- drug_classes[i]
  color_idx <- (i - 1) %% length(muted_colors) + 1
  color_palettes[[dc]] <- colorRampPalette(c("#FFFFFF", muted_colors[color_idx]))(100)
}

create_heatmap <- function(data, title, subtitle, x_var) {
  base_plot <- ggplot() +
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = "ARG (Global Top 30)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "lightgray"),
      strip.text = element_text(face = "bold")
    )
  
  current_plot <- base_plot
  
  for (i in seq_along(drug_classes)) {
    dc <- drug_classes[i]
    dc_data <- data %>% filter(drug_class == dc)
    
    if (nrow(dc_data) > 0) {
      current_plot <- current_plot +
        new_scale_fill() +
        geom_tile(
          data = dc_data,
          aes(x = {{x_var}}, y = best_hit_aro, fill = prevalence),
          color = "white", linewidth = 0.5
        ) +
        geom_text(
          data = dc_data,
          aes(x = {{x_var}}, y = best_hit_aro, 
              label = ifelse(prevalence > 0, sprintf("%.1f", prevalence), "")),
          size = 3, color = "black"
        ) +
        scale_fill_gradientn(
          name = dc,
          colors = color_palettes[[dc]],
          limits = c(0, 100),
          na.value = "grey90"
        )
    }
  }
  
  final_plot <- current_plot +
    facet_grid(drug_class ~ period, scales = "free", space = "free")
  
  return(final_plot)
}

arg_prev_st <- arg_prev_st %>%
  mutate(st_period = paste(st, period, sep = "_"),
         st_factor = fct_reorder(factor(st), as.numeric(gsub("ST", "", st))),
         period = factor(period, levels = c("2000-2009", "2010-2019"))) %>%
  arrange(period, st_factor) %>%
  mutate(st_period = factor(st_period, levels = unique(st_period)))

st_plot <- create_heatmap(
  data = arg_prev_st,
  title = "ARG Prevalence in Key ST Types",
  subtitle = "Prevalence calculated within each ST type",
  x_var = st_period
)
