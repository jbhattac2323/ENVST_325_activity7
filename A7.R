#Tutorial 6
# read in greenhouse gas data from reservoirs
ghg <- read.csv("/cloud/project/activity07/Deemer_GHG_Data.csv")
library(dplyr)
library(ggplot2)
library(olsrr)
library(PerformanceAnalytics)

#Get intuitive spread of ch4 levels as shown in tutorial
ggplot(ghg, aes(x = ch4)) +
  geom_histogram(color = "black") +
  labs(
    title = "Distribution of CH4",
    x = "CH4",
    y = "Count"
  ) +
  theme_minimal()

#Transform to linear
ghg$log.ch4 <- log(ghg$ch4+1)
ghg$log.age <- log(ghg$age)
ghg$log.DIP <- log(ghg$DIP+1)
ghg$log.precip <- log(ghg$precipitation)