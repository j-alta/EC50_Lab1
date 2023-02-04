#EC 50 Lab 1

library(tidyverse)
library(haven)
library(ggthemes)
library(statar) 

#set working directory 
setwd("~/Documents/EC 50/")
horn_panel_adjusted <- read_csv("lab1_df_sample.csv")
view(horn_panel_adjusted)

nlsy97 <- read_dta("nlsy97.dta")
view(nlsy97)


#plot a histogram based on fares
horn_panel_adjusted$lst_celsius
ggplot(horn_panel_adjusted) +
  geom_histogram(aes(lst_celsius))

#how to createa new variable
mean_temp = mean(horn_panel_adjusted$lst_celsius, na.rm = TRUE)
sd_temp = sd(horn_panel_adjusted$lst_celsius, na.rm = TRUE)
max_temp = max(horn_panel_adjusted$lst_celsius, na.rm = TRUE)

horn_panel_adjusted$above_1sd <- ifelse(horn_panel_adjusted$lst_celsius > mean_temp + sd_temp, 1, 0)

#record percentage
mean(horn_panel_adjusted$above_1sd, na.rm = TRUE)
horn_panel_adjusted$ranked_temp = rank(horn_panel_adjusted$lst_celsius)

#generate percentile rank
max_rank = max(horn_panel_adjusted$ranked_temp)
horn_panel_adjusted$percentile_temp = (horn_panel_adjusted$ranked_temp/max_rank)*100

#sort data from lowest to hihgest
df <- df[order(df$percentile_temp)]


#binned scatter plot 
ggplot(df, aes(x = mean_ndvi, y = lst_celsius)) + 
  stat_binmean(n = 20, geom = "point")) 

#step 1: create a vector
#step 2: create a new column
#step 3: set the function and set length and set replace to true 
c = c(1, 0)

#how to set the sample size 
set.seed(12345)
df$treat = sample(c, nrow(df), replace = TRUE)
mean(df$treat) 

#how to save
ggsave("lab1_histogram.png")




