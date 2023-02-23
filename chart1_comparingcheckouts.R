#Load Libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# data frame
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df1 <- df %>% filter(CheckoutYear < 2023)

####

# BOOK checkouts per month
book_checkouts_yr <- spl_df1 %>% filter(MaterialType == "BOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# EBOOK checkouts per month
ebook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "EBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# AUDIOBOOK checkouts per month
audiobook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "AUDIOBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# Plotting the comparison
ggplot() + 
  geom_line(data = book_checkouts_yr, aes(x = CheckoutYear, y = total_checkouts, group = 1), colour = "blue") + 
  geom_line(data = ebook_checkouts_yr, aes(x = CheckoutYear, y = total_checkouts, group = 1), colour = "red") + 
  geom_line(data = audiobook_checkouts_yr, aes(x = CheckoutYear, y = total_checkouts, group = 1), colour = "green") +
  labs(title = "Comparing Book, eBook, and Audiobook checkouts from 2013 - 2022", x = "Checkout Year", y = "Total # of Checkouts") +
  scale_x_continuous(breaks = seq(2013, 2022, 1)) + 
  scale_y_continuous(labels = label_number_si())

####