#Load Libraries
library("dplyr")
library("treemap")
library("stringr")
library("ggplot2")
library("scales")

# data frame
df1 <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df1 <- df1 %>% filter(CheckoutYear < 2023)

# In what months do people check out books the most, or what time of year?
Checkouts_per_month <- spl_df1 %>% group_by(CheckoutMonth) %>% summarize(total_checkouts = sum((Checkouts)))

####

# BOOK checkouts per month
book_checkouts_yr <- spl_df1 %>% filter(MaterialType == "BOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# EBOOK checkouts per month
ebook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "EBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# AUDIOBOOK checkouts per month
audiobook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "AUDIOBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

####

# data frame 2
df2 <- read.csv("~/Downloads/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df2 <- df2 %>% filter(CheckoutYear < 2023)

# In 2022, of all books checked out, on average they were checked out 3 times over the past year.
avg_num_checkouts <- (sum(spl_df2$Checkouts))/(nrow(spl_df2))

# Filtering for the 30 most popular book subjects in 2022
subjects_checkouts <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>% summarize(total_checkouts = sum((Checkouts)))
top10subjects <- subjects_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

####

# Filtering for the top 10 Creators in 2022
creator_checkouts <- spl_df2 %>% group_by(Creator) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit
top10creators <- creator_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

####