library(dplyr)
  data <- read.csv("WDVP Datasets - small countries are beautiful 6.51.37 PM.csv", stringsAsFactors = FALSE)
summary <- function(dataset) {
  dataset <- dataset[-c(1:4),]
  dataset[dataset == "-"] <- NA
  dataset <- type.convert(dataset, na.strings = "NA", as.is = TRUE, dec = ".",
                       numerals = c("allow.loss", "warn.loss", "no.loss"))  
  ret <- list()
  ret$no_countries <- nrow(dataset)
  max_pop <- dataset %>% filter(population == max(population))
  ret$max_pop <- max_pop %>% pull(indicator)
  min_pop <- dataset %>% filter(population == min(population))
  ret$min_pop <- min_pop %>% pull(indicator)
  ret$max_GDP <- max(dataset$GDP, na.rm = TRUE)
  ret$min_GDP <- min(dataset$GDP, na.rm = TRUE)
  ret$mean_GDP <- mean(dataset$GDP, na.rm = TRUE)
  GDP <- dataset %>% select(
    indicator, GDP) %>% arrange(
      desc(GDP)) %>% head(1)
  ret$max_GDP_country <- GDP %>% pull(indicator)
  happy <- dataset %>% select(
    indicator, world.happiness.report.score) %>% arrange(
      desc(world.happiness.report.score))
  happy <- head(happy, 5)
  ret$happy_top5 <- happy %>% pull(indicator)
  ret
}
test1 <- summary(data)

data <- transform(data, indicator = as.numeric(indicator), population = as.numeric(population))
View(data)
y <- sum(data$GDP, na.rm = TRUE)
z <- mean(data$population, na.rm = TRUE)
is.na(data$GPD, na.rm=TRUE)
sapply(data, mode)
str(data)
data <- type.convert(data, na.strings = "NA", as.is = TRUE, dec = ".",
             numerals = c("allow.loss", "warn.loss", "no.loss"))
data <- data[-c(1:4),]
data[data == "-"] <- NA
min(data$GDP, na.rm = TRUE)
max
z <- as.numeric(data$population)
yeet <- data %>% select(indicator, world.happiness.report.score)
yeet <- yeet %>% arrange(desc(world.happiness.report.score))


