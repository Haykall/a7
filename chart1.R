# (Jessica chart)
library("dplyr")
library("ggplot2")
library("tidyr")
library("styler")
library("lintr")

world_data_viz <- read.csv("WDVP Datasets - small countries are 
                           beautiful 6.51.37 PM.csv",
  stringsAsFactors = FALSE
)

my_plot <- function(dataset) {
  dataset$health.expenditure <- 
    as.numeric(as.character(world_data_viz$health.expenditure))
  dataset$education.expenditure <- 
    as.numeric(as.character(world_data_viz$education.expenditure))
  
  lowest_hdi <- dataset %>%
    select(indicator, human.development.index, education.expenditure, 
           health.expenditure) %>%
    arrange(desc(human.development.index)) %>%
    filter(human.development.index < 1 & human.development.index > 0.3) %>%
    head(5)
  highest_hdi <- dataset %>%
    select(indicator, human.development.index, education.expenditure, 
           health.expenditure) %>%
    arrange(human.development.index) %>%
    filter(human.development.index < 1 & human.development.index > 0.3) %>%
    head(5)

  low_plot <- lowest_hdi %>% gather(
    key = expenditure, value = health.expenditure,
    -indicator, -human.development.index
  )
  high_plot <- highest_hdi %>% gather(
    key = expenditure, value = health.expenditure,
    -indicator, -human.development.index
  )


  low_final_plot <- ggplot(low_plot) + geom_col(
    mapping = aes(x = indicator, y = health.expenditure, fill = expenditure),
    position = "dodge"
  ) + scale_color_brewer(palette = "Set3") + labs(
    title = "Comparing health  and education expenditure on the 
    countries with the top 5 highest HDI ",
    x = "Indicators", y = "Expenditure Scale"
  )

  high_final_plot <- ggplot(high_plot) + geom_col(
    mapping = aes(x = indicator, y = health.expenditure, fill = expenditure),
    position = "dodge"
  ) + scale_color_brewer(palette = "Set3") + labs(
    title = "Comparing health  and education expenditure on the countries 
    with the top 5 lowest HDI ",
    x = "Indicators", y = "Expenditure Scale"
  )
}

vary <- my_plot(world_data_viz)
