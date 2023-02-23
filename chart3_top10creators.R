#Load Libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# Filtering for the top 10 Creators in 2020
creator_checkouts <- spl_df2 %>% group_by(Creator) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit
top10creators <- creator_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

# Plotting the top 10 Authors in 2020
ggplot(top10creators) + 
  geom_col(aes(x = total_checkouts, y = reorder(Creator, +total_checkouts), fill = Creator))

####