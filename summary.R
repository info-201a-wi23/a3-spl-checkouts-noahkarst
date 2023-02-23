#Load Libraries
library("dplyr")
library("stringr")
library("ggplot2")
library("scales")
library("stringr") 

# data frame
df1 <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df1 <- df1 %>% filter(CheckoutYear < 2023)

####

# BOOK checkouts per month
book_checkouts_yr <- spl_df1 %>% filter(MaterialType == "BOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# EBOOK checkouts per month
ebook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "EBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

# AUDIOBOOK checkouts per month
audiobook_checkouts_yr <- spl_df1 %>% filter(MaterialType == "AUDIOBOOK") %>% group_by(CheckoutYear) %>% summarize(total_checkouts = sum(Checkouts))

#### What was the total checkouts for each in 2021?

# Plotting the comparison
ggplot() + 
  geom_line(data = book_checkouts_yr, aes(x = CheckoutYear, y = total_checkouts, group = 1), colour = "blue") + 
  geom_line(data = ebook_checkouts_yr, aes(x = CheckoutYear, y = total_checkouts, group = 1), colour = "red") + 
  geom_line(data = audiobook_checkouts_yr, aes(x = CheckoutYear, y = total_checkouts, group = 1), colour = "green") +
  labs(title = "Comparing Book, eBook, and Audiobook checkouts from 2013 - 2022", x = "Checkout Year", y = "Total # of Checkouts") +
  scale_x_continuous(breaks = seq(2013, 2022, 1)) + 
  scale_y_continuous(labels = label_number_si())

####

# data frame 2
df2 <- read.csv("~/Downloads/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# filter to not include 2023
spl_df2 <- df2 %>% filter(CheckoutYear < 2023)


# In 2022, of all books checked out, on average they were checked out 3 times over the past year.
avg_num_checkouts <- (sum(spl_df2$Checkouts))/(nrow(spl_df2))


#most popular genre in 2022?

spl_df2 <- spl_df2 %>% mutate(Make = sub(" .*", "", Subjects))

subjects_checkouts <- spl_df2 %>% group_by(sub(" .*", "", Subjects)) %>% summarize(total_checkouts = sum((Checkouts)))



ggplot(subjects_checkouts, aes(area = value, fill = group)) +
  geom_treemap()

# In what months do people check out books the most, or what time of year?

# Most popular author in 2022 who had the most checkotus?


# 10 characters

####







