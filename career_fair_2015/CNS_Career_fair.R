fair <- read.csv("~/Google Drive/100 - Publicly hosted/F15_CNS-career_fair.csv", na.strings="")
summary(fair)
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggthemes)

gather(select(fair, Organization.Name, Special, CompSci:Math), key = major, value = value, CompSci:Math) %>%
  filter(value) %>%
  ggplot(aes(x=factor(major))) + geom_histogram(aes(fill=major)) + 
  scale_fill_discrete(guide=F) + ggtitle("Career Fair - College of Natural Science,\nProspective Jobs by Major (2015)") + 
  theme_fivethirtyeight() + scale_color_fivethirtyeight() + labs(x="Majors", y="Number of Organizations") 
