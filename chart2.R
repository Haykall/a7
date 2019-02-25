# Plot Comparing GDP and World Happiness Index
library(dplyr)
library(plotly)

data_set <- read.csv("WDVP Datasets - small countries are beautiful 6.51.37 PM.csv")

# Function that makes the scatter plot comparing Happiness and GDP of developing countries
scatter_plot <- function(csv_file) {
  data_set <- read.csv(csv_file, stringsAsFactors = F)
  gdp_happiness <- data_set[-c(1:4), ] %>%
    # Removed the countries that didn't have the data available, only 38 countries left
    filter(world.happiness.report.score != "-") %>%
    select(indicator, GDP, world.happiness.report.score)
  happiness_plot <- plot_ly(gdp_happiness, y = ~GDP, x = ~world.happiness.report.score) %>%
    add_markers(
      y = ~GDP,
      x = ~world.happiness.report.score,
      text = ~ paste(indicator, "GDP(billions, PPP):", GDP, "World Happiness:",
                     world.happiness.report.score, sep = "<br />"),
      hovertext = text,
      alpha = .7
      ) %>%
    layout(title = "Comparing developing countries GDP(Billions, PPP) and World Happiness Score",
           xaxis = list(title = "World Happiness Score"),
           yaxis = list(title = "GDP (Billions, PPP)"))
  happiness_plot
}
scatter_plot("WDVP Datasets - small countries are beautiful 6.51.37 PM.csv")
