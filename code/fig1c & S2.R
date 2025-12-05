
library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)
library(gridExtra)
library(RColorBrewer)

st_data <- read.csv("C:/STmeta.csv", stringsAsFactors = FALSE)

prepare_data <- function(df, group_var) {
  processed_df <- df %>%
    rename(
      year = Year,
      ST_type = ST
    ) %>%
    mutate(year = as.numeric(year)) %>%
    filter(!is.na(year), !is.na(ST_type), ST_type != "ST-") %>%   # ✅ 忽略 ST-
    group_by(year, !!sym(group_var), ST_type) %>%
    summarise(abundance = n(), .groups = "drop")
  total_abundance <- processed_df %>%
    group_by(!!sym(group_var), year) %>%
    summarise(total_abundance = sum(abundance), .groups = "drop")
  combined_df <- processed_df %>%
    left_join(total_abundance, by = c(group_var, "year")) %>%
    mutate(proportion = abundance / total_abundance)
  return(combined_df)
}

get_top_st_types <- function(df, group_var) {
  avg_df <- df %>%
    group_by(!!sym(group_var), ST_type) %>%
    summarise(mean_prop = mean(proportion, na.rm = TRUE), .groups = "drop")
  
  top_st <- avg_df %>%
    group_by(!!sym(group_var)) %>%
    slice_max(mean_prop, n = 10, with_ties = FALSE) %>%
    ungroup()
  
  df <- df %>%
    left_join(top_st %>% select(!!sym(group_var), ST_type) %>% mutate(is_top = TRUE),
              by = c(group_var, "ST_type")) %>%
    mutate(ST_group = if_else(is_top, ST_type, "Others"))
  
  final_df <- df %>%
    group_by(!!sym(group_var), year, ST_group) %>%
    summarise(
      abundance = sum(abundance),
      total_abundance = first(total_abundance),
      proportion = sum(proportion),
      .groups = "drop"
    )
  
  return(final_df)
}

generate_global_palette <- function(df) {
  all_st <- sort(unique(df$ST_group))
  if ("Others" %in% all_st) {
    all_st <- c(setdiff(all_st, "Others"), "Others")
  }
  
  n_colors <- length(all_st)
  base_colors <- if (n_colors <= 12) {
    RColorBrewer::brewer.pal(min(n_colors, 12), "Set3")
  } else {
    colorRampPalette(brewer.pal(12, "Set3"))(n_colors)
  }
  names(base_colors) <- all_st
  if ("Others" %in% names(base_colors)) base_colors["Others"] <- "grey70"
  
  return(base_colors)
}


create_stacked_plot <- function(df, group_var, group_name) {
  groups <- unique(df[[group_var]])
  
  global_palette <- generate_global_palette(df)
  
  plot_list <- list()
  
  for (group in groups) {
    group_df <- df %>%
      filter(!!sym(group_var) == group)
  
    local_palette <- global_palette[names(global_palette) %in% unique(group_df$ST_group)]
    
    p <- ggplot(group_df, aes(x = factor(year), y = proportion, fill = ST_group)) +
      geom_bar(stat = "identity", width = 0.8, color = "black", size = 0.2) +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      scale_fill_manual(values = local_palette) +  # ✅ 局部安全映射
      labs(
        title = paste("Top 10 ST Types in", group),
        x = "Year", y = "Proportion (100%)", fill = "ST Type"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    plot_list[[group]] <- p
  }
  
  if (length(plot_list) > 1) {
    grid.arrange(grobs = plot_list, ncol = 2,
                 top = paste("Dominant ST Types Over Time by", group_name))
  } else {
    print(plot_list[[1]])
  }
  
  return(plot_list)
}

analyze_trends <- function(df, group_var) {
  all_trends <- list()
  groups <- unique(df[[group_var]])
  for (group in groups) {
    group_df <- df %>% filter(!!sym(group_var) == group)
    group_trends <- list()
    for (st_type in unique(group_df$ST_group)) {
      st_data <- group_df %>% filter(ST_group == st_type)
      if (nrow(st_data) < 2) {
        group_trends[[st_type]] <- data.frame(
          Group = group,
          ST_type = st_type,
          Slope = NA,
          P_value = NA,
          R_squared = NA,
          Trend = "NA"
        )
        next
      }
      model <- lm(proportion ~ year, data = st_data)
      model_summary <- summary(model)
      coefficients <- coef(model_summary)
      if ("year" %in% rownames(coefficients)) {
        slope <- coefficients["year", "Estimate"]
        p_value <- coefficients["year", "Pr(>|t|)"]
        r_squared <- model_summary$r.squared
        trend_direction <- ifelse(p_value < 0.05, 
                                  ifelse(slope > 0, "up", "down"),
                                  "ns")
      } else {
        slope <- NA
        p_value <- NA
        r_squared <- NA
        trend_direction <- "na"
      }
      
      group_trends[[st_type]] <- data.frame(
        Group = group,
        ST_type = st_type,
        Slope = slope,
        P_value = p_value,
        R_squared = r_squared,
        Trend = trend_direction
      )
    }

    group_trend_df <- bind_rows(group_trends)
    all_trends[[group]] <- group_trend_df
  }
  
  trend_df <- bind_rows(all_trends)
  
  return(trend_df)
}

analyze_by_continent <- function() {
  prepared_df <- prepare_data(st_data, "Continent")
  top_st_df <- get_top_st_types(prepared_df, "Continent")
  plots <- create_stacked_plot(top_st_df, "Continent", "Continent")

  continent_trends <- analyze_trends(top_st_df, "Continent")
  write.csv(continent_trends, "continent_st_trends.csv", row.names = FALSE)
  write.csv(top_st_df, "top10_st_types_by_continent.csv", row.names = FALSE)
  return(list(data = top_st_df, plots = plots, trends = continent_trends))
}

continent_results <- analyze_by_continent()
