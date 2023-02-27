#Load Libraries
library("dplyr")
library("treemapify")
library("stringr")
library("ggplot2")
library("scales")

# data frame 2
df <- read.csv("~/Downloads/2013-2023-5-Checkouts-SPL.csv", stringsAsFactors = FALSE)

# filter to only include 2020
spl_df2 <- df %>% filter(CheckoutYear == 2020)

# Filtering for the 10 most popular book subjects in 2022
top10subjects <- spl_df2 %>% group_by(subjects = sub(", .*", "", Subjects)) %>%
  summarize(total_checkouts = sum((Checkouts))) %>%
  arrange(desc(total_checkouts)) %>% slice(1:10)

# Plotting the top 10 subjects in 2020

colores <- scales::brewer_pal(palette = "Paired")(10)
ggplot(top10subjects, aes(label = paste0(subjects, "\n", total_checkouts), area = total_checkouts, fill = subjects)) +
  geom_treemap() +
  labs(title = "Top Subjects of 2020", caption = paste0("African American Fiction", "\n", "48519")) +
  geom_treemap_text(colour = "white", place = "centre") +
  scale_fill_manual("", breaks = top10subjects$subjects[c(10)], values = setNames(colores, top10subjects$subjects)) +
  theme(legend.position = "none") +
  theme(plot.caption = element_text(color = "purple", face = "bold", size = 12))

####