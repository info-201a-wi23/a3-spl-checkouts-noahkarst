#Load Libraries
library("dplyr")
library("treemap")
library("stringr")
library("ggplot2")
library("scales")

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# Filtering for the 10 most popular book subjects in 2022
subjects_checkouts <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>% summarize(total_checkouts = sum((Checkouts)))
top10subjects <- subjects_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

# Plotting the top 10 subjects in 2020
treemap(top10subjects,
        index=c("subjects"),
        vSize="total_checkouts",
        type="index",
        title = "Top 10 Book Subjects checked out in 2022",
        fontsize.labels = 10)

####