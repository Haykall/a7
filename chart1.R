
library("dplyr")
library("ggplot2")
library("tidyr")
library("styler")
library("lintr")


my_plot <- function(file_csv) {
  # convert characters in dataset to numerics
  dataset <- read.csv(file_csv, stringsAsFactors = F)
  dataset$health.expenditure <- 
    as.numeric(as.character(dataset$health.expenditure))
  dataset$education.expenditure <- 
    as.numeric(as.character(dataset$education.expenditure))
  
  #filter dataset to show the countries with the highest human development index
  #along with their respective health expenditure and education expenditure
  highest_hdi <- dataset %>%
    select(indicator, human.development.index, education.expenditure, 
           health.expenditure) %>%
    arrange(human.development.index) %>%
    filter(human.development.index < 1 & human.development.index > 0.3) %>%
    head(5)

  #determine what information is disploayed on the plot, what fills the bar
  # chart
  high_plot <- highest_hdi %>% gather(
    key = expenditure, value = health.expenditure,
    -indicator, -human.development.index
  )

  #plot the highest hdi countries, comparing health and education expenditure
  high_final_plot <- ggplot(high_plot) + geom_col(
    mapping = aes(x = indicator, y = health.expenditure, fill = expenditure),
    position = "dodge"
  ) + scale_color_brewer(palette = "Set3") + labs(
    title = "Comparing health  and education expenditure on the countries 
    with the top 5 lowest HDI ",
    x = "Indicators", y = "Expenditure Scale"
  )
  
  high_final_plot
}


