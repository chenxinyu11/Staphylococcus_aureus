library(vegan)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
data <- read.table("C:/shannon2.TXT", 
                   header = TRUE, sep = "\t", stringsAsFactors = FALSE)
data$year <- as.numeric(data$year)
st_cols <- setdiff(colnames(data), c("continent", "year"))
data$shannon <- diversity(data[, st_cols], index = "shannon")
data$richness <- apply(data[, st_cols], 1, function(x) sum(x > 0))
data$simpson <- diversity(data[, st_cols], index = "simpson")
data$evenness <- ifelse(data$richness > 1,
                        data$shannon / log(data$richness),
                        NA)
diversity_results <- data.frame()
group_col <- "continent"
for (grp in unique(data[[group_col]])) {
  grp_data <- data[data[[group_col]] == grp, ]
  if (nrow(grp_data) < 3) next
  for (idx in c("shannon","richness","simpson","evenness")) {
    model <- lm(as.formula(paste(idx,"~ year")), data = grp_data)
    summary_model <- summary(model)
    diversity_results <- rbind(diversity_results,
                               data.frame(
                                 Group = grp,
                                 Index = idx,
                                 Slope = coef(model)[2],
                                 R_squared = summary_model$r.squared,
                                 P_value = summary_model$coefficients[2,4],
                                 N_samples = nrow(grp_data)
                               ))
  }
}

global_models <- list()
global_summaries <- list()
for (idx in c("shannon","richness","simpson","evenness")) {
  model <- lm(as.formula(paste(idx,"~ year")), data = data)
  summary_model <- summary(model)
  global_models[[idx]] <- model
  global_summaries[[idx]] <- summary_model
}

trend_results <- list()

for (idx in c("shannon","richness","simpson","evenness")) {
  global_model <- global_models[[idx]]
  global_summary <- global_summaries[[idx]]
  
  trend_results[[idx]] <- data.frame(
    Group = "Global",
    Index = idx,
    Slope = round(coef(global_model)[2], 4),
    R_squared = round(global_summary$r.squared, 3),
    P_value = ifelse(global_summary$coefficients[2,4] < 0.001, "<0.001",
                     round(global_summary$coefficients[2,4], 3)),
    stringsAsFactors = FALSE
  )
  
  group_trend <- diversity_results %>%
    filter(Index == idx) %>%
    mutate(P_value = ifelse(P_value < 0.001, "<0.001", round(P_value,3))) %>%
    select(Group, Index, Slope, R_squared, P_value)
  
  trend_results[[idx]] <- rbind(trend_results[[idx]], group_trend)
}

trend_df <- do.call(rbind, trend_results)
print(trend_df)

groups <- unique(data[[group_col]])
group_colors <- setNames(brewer.pal(min(8,length(groups)), "Set2"), groups)

plot_diversity <- function(df, index_name, global_model, global_summary, trend_df, group_col, ylab) {
  index_trend <- trend_df %>% filter(Index == index_name)
  index_trend_text <- paste(index_trend$Group, 
                            ": Slope=", index_trend$Slope,
                            ", R²=", index_trend$R_squared,
                            ", p=", index_trend$P_value, sep="")
  
  p <- ggplot(df, aes_string(x="year", y=index_name, color=group_col)) +
    geom_point(alpha=0.6, size=2) +
    geom_smooth(aes_string(group=group_col), method="lm", se=FALSE, alpha=0.5, size=0.8) +
    geom_smooth(method="lm", se=FALSE, color="black", linetype="solid", size=1.2) +
    scale_color_manual(values=group_colors) +
    labs(title=paste(index_name,"Trends"),
         x="Year", y=ylab, color=group_col) +
    theme_minimal() +
    theme(plot.title=element_text(hjust=0.5, face="bold", size=14),
          legend.position="bottom") +
    annotate("text", x=min(df$year), y=max(df[[index_name]], na.rm=TRUE),
             label=paste(index_trend_text, collapse="\n"),
             hjust=0, vjust=1, size=3.5, fontface="bold")
  return(p)
}

p_shannon  <- plot_diversity(data, "shannon",  global_models$shannon,  global_summaries$shannon,  trend_df, group_col, "Shannon Diversity Index")
p_richness <- plot_diversity(data, "richness", global_models$richness, global_summaries$richness, trend_df, group_col, "Number of ST Types")
p_simpson  <- plot_diversity(data, "simpson",  global_models$simpson,  global_summaries$simpson,  trend_df, group_col, "Simpson Diversity Index")
p_evenness <- plot_diversity(data, "evenness", global_models$evenness, global_summaries$evenness, trend_df, group_col, "Evenness (Pielou's Index)")

print(p_shannon)
print(p_richness)
print(p_simpson)
print(p_evenness)
