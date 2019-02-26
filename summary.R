library(dplyr)
  data <- read.csv("WDVP Datasets - small countries are beautiful 6.51.37 PM.csv", stringsAsFactors = FALSE)
  View(data)
  data <- data[-c(1:4),]
  data[data == "-"] <- NA
  summary <- function(dataset) {
  dataset <- dataset[-c(1:4),]
  dataset[dataset == "-"] <- NA
  dataset <- type.convert(dataset, na.strings = "NA", as.is = TRUE, dec = ".",
                       numerals = c("allow.loss", "warn.loss", "no.loss"))  
  ret <- list()
  ret$no_countries <- nrow(dataset)
  max_pop <- dataset %>% filter(population == "5,000,000")
  View(max_pop)
  ret$max_pop <- max_pop %>% pull(indicator)
  ret$max_pop_amount <- max_pop %>% pull(population)
  min_pop <- dataset %>% filter(population == "100,000")
  ret$min_pop <- min_pop %>% pull(indicator)
  ret$min_pop_amount <- min_pop %>% pull(population)
  ret$max_GDP <- max(dataset$GDP, na.rm = TRUE)
  ret$min_GDP <- min(dataset$GDP, na.rm = TRUE)
  ret$mean_GDP <- mean(dataset$GDP, na.rm = TRUE)
  max_GDP <- dataset %>% select(
    indicator, GDP) %>% arrange(
      desc(GDP)) %>% head(1)
  ret$max_GDP_country <- max_GDP %>% pull(indicator)
  happy <- dataset %>% select(
    indicator, world.happiness.report.score) %>% arrange(
      desc(world.happiness.report.score))
  happy <- head(happy, 5)
  ret$happy_top5 <- happy %>% pull(indicator)
  ret
}
test1 <- summary(data)
str(data)
min <- data %>% select(indicator, population) %>% arrange(desc(population))
View(min)
data <- data %>% mutate(pop = as.numeric(population))
View(data)
max(data$population)