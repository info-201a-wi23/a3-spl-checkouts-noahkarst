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

# Book checkouts in 2020
total_book_checkouts_2020 <- materialtype_trend %>% filter(CheckoutYear == 2020 & MaterialType == "BOOK")%>% pull(total_checkouts)
# 609671

# eBook checkouts in 2020
total_ebook_checkouts_2020 <- materialtype_trend %>% filter(CheckoutYear == 2020 & MaterialType == "EBOOK")%>% pull(total_checkouts)
# 1629338

# Audiobook checkouts in 2020
total_audiobook_checkouts_2020 <- materialtype_trend %>% filter(CheckoutYear == 2020 & MaterialType == "AUDIOBOOK")%>% pull(total_checkouts)
# 1040745

####

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# Filtering for the 10 most popular book subjects in 2020
top10subjects <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>%
  summarize(total_checkouts = sum((Checkouts))) %>%
  arrange(desc(total_checkouts)) %>% slice(1:10)

####

# Filtering for top 3 creators each month of 2020
top3creators_2020 <- spl_df2 %>% group_by(CheckoutMonth, Creator) %>% summarize(total_checkouts = sum(Checkouts)) %>% na_if("") %>% na.omit %>% arrange(desc(total_checkouts)) %>% slice(1:3)

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