#Load Libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")
library("tidyverse")

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# filtering for top 3 creators each month of 2020
top3creators_2020 <- spl_df2 %>% group_by(CheckoutMonth, Creator) %>% summarize(total_checkouts = sum(Checkouts)) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:3)

# graphing the top 3 creators for each month in 2020
ggplot(top3creators_2020) + 
  geom_point(aes(x = CheckoutMonth, y = total_checkouts, col = Creator, shape = 20), size = 4) +
  labs(title = "Top Creators of 2020", x = "Months", y = "Total # of Checkouts") +
  scale_shape_identity() +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(labels = label_number_si())

####