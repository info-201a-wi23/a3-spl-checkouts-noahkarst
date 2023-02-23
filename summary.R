#Load Libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

# data frame
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df1 <- df %>% filter(CheckoutYear < 2023)

# In what months do people check out books the most, or what time of year?
Checkouts_per_month <- spl_df1 %>% group_by(CheckoutMonth) %>% summarize(total_checkouts = sum((Checkouts)))

####

# BOOK checkouts per year
book_checkouts_yr <- spl_df1 %>% filter(MaterialType == "BOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# EBOOK checkouts per year
ebook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "EBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# AUDIOBOOK checkouts per year
audiobook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "AUDIOBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

####

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# In 2020, of all books checked out, on average they were checked out 3 times over the past year. ###?????
avg_num_checkouts <- (sum(spl_df2$Checkouts))/(nrow(spl_df2))

# Filtering for the 10 most popular book subjects in 2020
subjects_checkouts <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>% summarize(total_checkouts = sum((Checkouts)))
top10subjects <- subjects_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

####

# Filtering for the top 10 Creators in 2020
creator_checkouts <- spl_df2 %>% group_by(Creator) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit
top10creators <- creator_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

####