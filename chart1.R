#(Jessica chart)
library("dplyr")
library("ggplot2")
world_data_viz <- read.csv("WDVP Datasets - small countries are beautiful 6.51.37 PM.csv",
                         stringsAsFactors = FALSE)

my_plot <- function(dataset) {
  dataset %>% select(population, health.expenditure, education.expenditure) %>% arrange(desc(population))
}

vary <- my_plot(world_data_viz)