library(ggplot2)
library(dplyr)
library(readr)
library(cowplot)
library(RColorBrewer)
library(scales)
library(ggforce)
df <- read_csv("C:/STmeta.csv")
df <- df %>%
  mutate(
    Year = as.integer(Year),
    Period = case_when(
      Year <= 2000 ~ "≤2000",
      Year <= 2010 ~ "2001-2010",
      Year <= 2020 ~ "2011-2020",
      Year <= 2024 ~ "2021-2024",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Period != "Other") %>%
  mutate(
    Period = factor(Period, levels = c("≤2000", "2001-2010", "2011-2020", "2021-2024")),
    ST = ifelse(ST == "ST-", "Others", ST)
  ) %>%
  group_by(Period, region) %>%
  mutate(
    topST = ST != "Others" & 
      ST %in% names(sort(table(ST), decreasing = TRUE))[1:min(5, length(unique(ST[ST != "Others"])))],
    ST = ifelse(topST, ST, "Others")
  ) %>%
  ungroup() %>%
  select(-topST)

df_line <- df %>%
  group_by(Period, region) %>%
  summarise(n = n(), .groups = "drop")

period_levels <- levels(df$Period)
region_levels <- unique(df$region)

df_line <- df_line %>%
  mutate(
    x = as.numeric(factor(Period, levels = period_levels)),
    y = as.numeric(factor(region, levels = rev(region_levels)))
  )

df_pie <- df %>%
  group_by(region, Period, ST) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(region, Period) %>%
  mutate(
    prop = n / sum(n),
    cumulative = cumsum(prop),
    start = c(0, cumulative[-n()]),
    end = cumulative
  ) %>%
  ungroup() %>%
  left_join(df_line, by = c("region", "Period"))

size_levels <- c(10, 100, 1000, 10000)

df_line <- df_line %>%
  mutate(
    size_factor = log10(n) / log10(max(size_levels)),
    ring_outer_size = 0.4 + 0.6 * size_factor,
    ring_inner_size = ring_outer_size * 0.6
  )

all_st <- sort(unique(df_pie$ST))
n_colors <- length(all_st)
palette <- brewer.pal(min(n_colors, 9), "Pastel1")
if (n_colors > 9) palette <- colorRampPalette(palette)(n_colors)
st_colors <- setNames(palette, all_st)
st_colors["Others"] <- "#F0F0F0" 

p_main <- ggplot(df_line, aes(x = x, y = y)) +
  geom_line(aes(group = region), color = "#6A7B8D", linewidth = 1, alpha = 0.7) +
  scale_x_continuous(
    breaks = 1:length(period_levels),
    labels = period_levels,
    expand = expansion(mult = 0.1)
  ) +
  scale_y_continuous(
    breaks = 1:length(region_levels),
    labels = rev(region_levels),
    expand = expansion(mult = 0.1)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = "#7F8C8D"),
    axis.title = element_text(size = 14, face = "bold", color = "#34495E"),
    axis.text = element_text(size = 12, color = "#34495E"),
    axis.text.y = element_text(face = "bold"),
    panel.grid.major = element_line(color = "#ECF0F1", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin = margin(30, 120, 30, 30),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  coord_cartesian(clip = "off")
final_plot <- p_main
for (i in 1:nrow(df_line)) {
  this_period <- df_line$Period[i]
  this_region <- df_line$region[i]
  x_coord <- df_line$x[i]
  y_coord <- df_line$y[i]
  ring_outer_size <- df_line$ring_outer_size[i]
  ring_inner_size <- df_line$ring_inner_size[i]
  sample_size <- df_line$n[i]
  ring_data <- df_pie %>%
    filter(Period == this_period, region == this_region)
  if (nrow(ring_data) > 0) {
    ring_plot <- ggplot() +
      geom_arc_bar(
        data = ring_data,
        aes(
          x0 = 0, y0 = 0,
          r0 = ring_inner_size * 0.6,
          r = ring_outer_size,
          start = start * 2 * pi,
          end = end * 2 * pi,
          fill = ST
        ),
        color = "white",
        size = 0.5, 
        alpha = 0.9   
      ) +
      geom_text(
        data = data.frame(label = sample_size),
        aes(x = 0, y = 0, label = label),
        size = 4.5, 
        fontface = "bold",
        color = "#2C3E50"
      ) +
      scale_fill_manual(values = st_colors) +
      coord_equal() +
      theme_void() +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "transparent", color = NA))
    
    final_plot <- final_plot + 
      annotation_custom(
        ggplotGrob(ring_plot),
        xmin = x_coord - ring_outer_size * 0.5,
        xmax = x_coord + ring_outer_size * 0.5,
        ymin = y_coord - ring_outer_size * 0.5,
        ymax = y_coord + ring_outer_size * 0.5
      )
  }
}

legend_plot_st <- ggplot(df_pie %>% distinct(ST), aes(x = "", y = 1, fill = ST)) +
  geom_col(color = "white", size = 0.5) +
  scale_fill_manual(values = st_colors) +
  guides(fill = guide_legend(
    title = "ST",
    title.position = "top",
    title.theme = element_text(size = 12, face = "bold", color = "#2C3E50"),
    ncol = 2,
    keywidth = 1.0,
    keyheight = 1.0
  )) +
  theme_void() +
  theme(
    legend.title = element_text(size = 12, face = "bold", color = "#2C3E50"),
    legend.text = element_text(size = 10, color = "#34495E"),
    legend.key = element_rect(color = "#BDC3C7", size = 0.5)
  )

size_legend_data <- data.frame(
  n = size_levels,
  stringsAsFactors = FALSE
) %>%
  mutate(
    size_factor = log10(n) / log10(max(size_levels)),
    ring_size = 0.4 + 0.6 * size_factor,
    label = format(n, big.mark = ",")
  )

size_legend <- ggplot(size_legend_data) +
  geom_point(
    aes(x = 1, y = ring_size, size = ring_size),
    shape = 21, fill = "#3498DB", color = "white", stroke = 1
  ) +
  geom_text(
    aes(x = 1.15, y = ring_size, label = label),
    size = 4, hjust = 0, vjust = 0.5, color = "#2C3E50"
  ) +
  scale_size_continuous(range = range(size_legend_data$ring_size) * 20) +
  scale_y_continuous(limits = c(min(size_legend_data$ring_size) - 0.05, 
                                max(size_legend_data$ring_size) + 0.05)) +
  coord_cartesian(xlim = c(0.8, 1.5)) +
  labs(title = "sample number") +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, color = "#2C3E50"),
    legend.position = "none"
  )

legends_combined <- plot_grid(
  ggdraw() + 
    draw_plot(get_legend(legend_plot_st), x = 0, y = 0.1, width = 1, height = 0.9),
  ggdraw() + 
    draw_plot(size_legend, x = 0.1, y = 0.1, width = 0.8, height = 0.8),
  ncol = 1,
  rel_heights = c(0.7, 0.3),
  labels = NULL
)

legends_combined <- ggdraw(legends_combined) +
  theme(
    plot.background = element_rect(fill = "#F8F9FA", color = "#BDC3C7", size = 1),
    plot.margin = margin(15, 15, 15, 15)
  )

final_composite <- plot_grid(
  final_plot,
  legends_combined,
  ncol = 2,
  rel_widths = c(0.75, 0.25)
) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(30, 30, 30, 30)
  )

final_composite <- ggdraw(final_composite)
final_composite
