library(sf)
library(ggplot2)
library(dplyr)
custom_palette <- c("#67798b", "#b5c1c8", "#cd8b82", "#bc696c")
map <- st_read("E:/ne_10m_admin_0_countries.shp") %>%
  st_set_crs(4326) %>%
  filter(NAME != "Antarctica")
data <- read.csv("E:/mapdata.csv")
map_data <- left_join(map, data, by = c("NAME" = "location_name")) %>%
  mutate(log_val = log1p(val))
ggplot(map_data) +
  geom_sf(
    aes(fill = log_val),
    color = "gray30",
    size = 0.1,
    alpha = 0.9
  ) +
  scale_fill_gradientn(
    name = "log(1 + Value)",
    colours = custom_palette,
    na.value = "lightgray"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.background = element_rect(fill = "white")
  ) +
  guides(fill = guide_colorbar(title.position = "top"))