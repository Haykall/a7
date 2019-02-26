library(dplyr)
#creates the function to extract data
summary <- function(file) {
  dataset <- read.csv(file, stringsAsFactors = FALSE)
  #removes the description rows with no numbers
  dataset <- dataset[-c(1:4), ]
  #converts blank cells with "-" with an NA instead
  dataset[dataset == "-"] <- NA
  #converts the columns into numerics
  dataset <- type.convert(dataset, na.strings = "NA", as.is = TRUE, dec = ".",
                       numerals = c("allow.loss", "warn.loss", "no.loss"))
  #creates a list  to return
  ret <- list()
  # Finds the number of countries based on how many rows there are
  ret$no_countries <- nrow(dataset)
  #Finds the countrues with the max population
  max_pop <- dataset %>% filter(population == "5,000,000")
  ret$max_pop <- max_pop %>% pull(indicator)
  ret$max_pop_amount <- max_pop %>% pull(population)
  #finds the countries with the min population
  min_pop <- dataset %>% filter(population == "100,000")
  ret$min_pop <- min_pop %>% pull(indicator)
  ret$min_pop_amount <- min_pop %>% pull(population)
  #Finds max, min, and min GDP from the data along with the country with the
  #highest GDP
  ret$max_GDP <- max(dataset$GDP, na.rm = TRUE)
  ret$min_GDP <- min(dataset$GDP, na.rm = TRUE)
  ret$mean_GDP <- mean(dataset$GDP, na.rm = TRUE)
  max_GDP <- dataset %>% select(
    indicator, GDP) %>% arrange(
      desc(GDP)) %>% head(1)
  ret$max_GDP_country <- max_GDP %>% pull(indicator)
  #Finds the 5 happiest countries
  happy <- dataset %>% select(
    indicator, world.happiness.report.score) %>% arrange(
      desc(world.happiness.report.score))
  happy <- head(happy, 5)
  ret$happy_top5 <- happy %>% pull(indicator)
  ret
  }