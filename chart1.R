#(Jessica chart)
library("dplyr")
library("ggplot2")


world_data_viz <- read.csv("WDVP Datasets - small countries are beautiful 6.51.37 PM.csv",
                         stringsAsFactors = FALSE)

#my_plot <- function(dataset) {
 aplot <- world_data_viz %>% select(indicator, human.development.index, health.expenditure, education.expenditure) %>% 
   arrange(desc(human.development.index)) %>% filter(human.development.index < 1 & human.development.index > 0.3)
 ayy <- aplot %>% group_by(human.development.index) %>% head(5)

 this_plot <- ggplot(ayy) + geom_col(mapping = aes(x = indicator,  y = health.expenditure))
#}

vary <- my_plot(world_data_viz)