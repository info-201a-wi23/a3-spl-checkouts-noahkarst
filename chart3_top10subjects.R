#Load Libraries
library("dplyr")
library("treemapify")
library("stringr")
library("ggplot2")
library("scales")

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# Filtering for the 10 most popular book subjects in 2022
top10subjects <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>%
  summarize(total_checkouts = sum((Checkouts))) %>%
  arrange(desc(total_checkouts)) %>% slice(1:10)

# Plotting the top 10 subjects in 2020

ggplot(top10subjects) +
  geom_treemap(aes(fill = subjects, area = total_checkouts)) +
  geom_treemap_text(aes(area = total_checkouts, label = paste0(subjects, "\n", total_checkouts)), colour = "white", place = "centre") +
  theme(legend.position = "none")

####