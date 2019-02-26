# Load relevant libraries
library("dplyr")
library("ggplot2")

plot_3 <- function(file) {
  # Filter through world data frame to get relevant data
  world_data <- read.csv(file = file, stringsAsFactors = FALSE) %>%
    select(indicator, human.development.index, world.happiness.report.score)
  
  colnames(world_data) <- c("country", "development", "happiness")
  
  world_data <- tail(world_data, -4)
  
  world_data <- world_data %>%
    arrange(development) %>%
    filter(
      development != "-",
      happiness != "-") %>%
    mutate(
      development = as.numeric(development),
      happiness = as.numeric(happiness)
    )

  # Summarize top 5 highest and lowest development and compare values
  top_5 <- world_data %>%
    top_n(5, development) %>%
    summarise(
      mean_dev = mean(development),
      mean_happy = mean(happiness)
    ) %>%
    mutate(type = "Most Developed")

  low_5 <- world_data %>%
    top_n(-5, development) %>%
    summarise(
      mean_dev = mean(development),
      mean_happy = mean(happiness)
    ) %>%
    mutate(type = "Least Developed")

  combined <- full_join(top_5, low_5)

  # Plot bar graph comparine dev levels to happiness scores
  colors <- c("#0072B2", "#FF9999")

  dev_happy_plot <- ggplot(data = combined, aes(x = mean_dev, y = mean_happy)) +
    geom_bar(stat = "identity", fill = colors) +
    geom_text(aes(label = combined$type, vjust = -2)) +
    labs(
      x = "Mean Human Development Index",
      y = "Mean Happiness Score",
      title = "Happiness Scores of the Top 5 and Lowest 5 Developed Nations"
    ) +
    scale_y_continuous(limits = c(0, 8))

  dev_happy_plot
}
