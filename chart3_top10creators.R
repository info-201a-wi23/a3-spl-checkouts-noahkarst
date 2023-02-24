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

# Filtering for the top 10 Creators in 2020
creator_checkouts <- spl_df2 %>% group_by(Creator) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit
top10creators <- creator_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

# Plotting the top 10 Authors in 2020
ggplot(top10creators) + 
  geom_col(aes(x = total_checkouts, y = reorder(Creator, +total_checkouts), fill = Creator))

####

# DO A LINE AND POINT PLOT

topcreators_jan <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 1) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Jan")
topcreators_feb <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 2) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Feb")
topcreators_mar <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 3) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Mar")
topcreators_apr <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 4) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Apr")
topcreators_may <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 5) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "May")
topcreators_jun <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 6) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Jun")
topcreators_jul <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 7) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Jul")
topcreators_aug <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 8) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Aug")
topcreators_sep <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 9) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Sep")
topcreators_oct <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 10) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Oct")
topcreators_nov <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 11) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Nov")
topcreators_dec <- spl_df2 %>% group_by(Creator) %>% filter(CheckoutMonth == 12) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:5) %>% add_column(month = "Dec")

ggplot() + 
  geom_point(data = topcreators_jan, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_feb, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_mar, aes(x = month, y = total_checkouts, group = 1)) +
  geom_point(data = topcreators_apr, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_may, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_jun, aes(x = month, y = total_checkouts, group = 1)) +
  geom_point(data = topcreators_jul, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_aug, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_sep, aes(x = month, y = total_checkouts, group = 1)) +
  geom_point(data = topcreators_oct, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_nov, aes(x = month, y = total_checkouts, group = 1)) + 
  geom_point(data = topcreators_dec, aes(x = month, y = total_checkouts, group = 1)) +
  labs(title = "Top Creators of 2020", x = "Months", y = "Total # of Checkouts") +
  scale_x_discrete(limits = month.abb) +
  scale_y_continuous(labels = label_number_si())

