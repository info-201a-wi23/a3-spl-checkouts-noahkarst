#Load Libraries
library("dplyr")
library("treemap")
library("stringr")
library("ggplot2")
library("scales")

# data frame 2
df2 <- read.csv("~/Downloads/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df2 <- df2 %>% filter(CheckoutYear < 2023)

# Filtering for the 10 most popular book subjects in 2022
subjects_checkouts <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>% summarize(total_checkouts = sum((Checkouts)))
top10subjects <- subjects_checkouts %>% arrange(desc(total_checkouts)) %>% slice(1:10)

# Plotting the top 10 subjects in 2022
treemap(top10subjects,
        index=c("subjects"),
        vSize="total_checkouts",
        type="index",
        title = "Top 10 Book Subjects checked out in 2022",
        fontsize.labels = 10)

####