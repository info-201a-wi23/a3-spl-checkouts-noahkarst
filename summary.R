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

# BOOK checkouts per year
book_checkouts_yr <- spl_df1 %>% filter(MaterialType == "BOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# EBOOK checkouts per year
ebook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "EBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# AUDIOBOOK checkouts per year
audiobook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "AUDIOBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

total_book_checkouts_2020 <- book_checkouts_yr %>% filter(CheckoutYear == 2020)%>% pull(total_checkouts)
# 609671

total_ebook_checkouts_2020 <- ebook_checkouts_yr %>% filter(CheckoutYear == 2020)%>% pull(total_checkouts)
# 1629338

total_audiobook_checkouts_2020 <- audiobook_checkouts_yr %>% filter(CheckoutYear == 2020)%>% pull(total_checkouts)
# 1040745

####

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# Filtering for the 10 most popular book subjects in 2020
subjects_checkouts <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>% summarize(total_checkouts = sum((Checkouts)))
top10subjects <- subjects_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

####

# Filtering for the top 10 Creators in 2020
creator_checkouts <- spl_df2 %>% group_by(Creator) %>% summarize(total_checkouts = sum((Checkouts))) %>% na_if("") %>% na.omit
top10creators <- creator_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

####

# Finding average checkout rates for 2020.
rows_books <- spl_df2 %>% filter(MaterialType == "BOOK")
avg_books_2020 <- (sum(total_book_checkouts_2020))/(nrow(rows_books))
# 10

rows_ebooks <- spl_df2 %>% filter(MaterialType == "EBOOK")
avg_ebooks_2020 <- (sum(total_ebook_checkouts_2020))/(nrow(rows_ebooks))
# 12

rows_audiobooks <- spl_df2 %>% filter(MaterialType == "AUDIOBOOK")
avg_audiobooks_2020 <- (sum(total_ebook_checkouts_2020))/(nrow(rows_audiobooks))
# 21

####