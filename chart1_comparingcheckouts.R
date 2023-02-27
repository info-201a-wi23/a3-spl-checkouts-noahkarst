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

# Filtering for BOOK, EBOOK, and AUDIOBOOK checkouts per year
materialtype_trend <- spl_df1 %>% filter(MaterialType %in% c("BOOK", "EBOOK", "AUDIOBOOK")) %>% group_by(CheckoutYear, MaterialType) %>% summarize(total_checkouts = sum(Checkouts))

# Plotting the comparison
ggplot(materialtype_trend) + 
  geom_line(aes(x = CheckoutYear, y = total_checkouts, col = MaterialType)) +
  labs(title = "Comparing Book, eBook, and Audiobook checkouts from 2013 - 2022", x = "Checkout Year", y = "Total # of Checkouts") +
  scale_x_continuous(breaks = seq(2013, 2022, 1)) + 
  scale_y_continuous(labels = label_number_si())

####