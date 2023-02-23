#Load Libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# data frame 2
df2 <- read.csv("~/Downloads/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df2 <- df2 %>% filter(CheckoutYear < 2023)

# Filtering for the top 10 Creators in 2022
creator_checkouts <- spl_df2 %>% group_by(Creator) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit
top10creators <- creator_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

# Plotting the top 10 Authors in 2022
ggplot(top10creators) + 
  geom_col(aes(x = total_checkouts, y = reorder(Creator, +total_checkouts), fill = Creator))

####