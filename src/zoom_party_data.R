# Details -----------------------------------------------------------------
# Title: Zoom Party Analysis
# Author: Meshach A. Pierre
# Date: 16-June-2021

# Load packages -----------------------------------------------------------
library(plyr)
library(tidyverse)
library(ghibli)
library(wordcloud)

# Data processing ---------------------------------------------------------
# Read data
party_data <- read_csv("data/party_data.csv")

# Check tibble
glimpse(party_data)

# Remove extra row at bottom (with totals), add column for order of points
# awarded or taken away
party_data <- party_data %>% 
  filter(!row_number()==n()) %>% 
  mutate(cats_2 = factor(cats_2),
         order = seq(1:nrow(.))) %>% 
  glimpse()

# Turn data from wide to long with name as column containing score adjustments
party_data_long <- party_data %>% 
  pivot_longer(cols=(vanessa:guyana),
               names_to = "name",
               values_to = "score_adj")

# Calculate running totals (cumulative) for scores
party_data_long <- party_data_long %>% 
  group_by(name) %>% 
  dplyr::mutate(csum = cumsum(coalesce(score_adj, 0)))

# Remove Guyana from data
party_data_long_nogy <- party_data_long %>% 
  filter(name != "guyana")

# Calculate final scores
party_totals <- party_data_long %>% 
  group_by(name) %>%
  summarise(n=sum(score_adj))

# Remove Guyana from totals
party_totals_nogy <- party_totals %>% 
  filter(name != "guyana")

# Plot scores -------------------------------------------------------------
# Extract viridis palette hex codes
values <- scales::viridis_pal(option = "D")(6)

# First plot: final scores
(p1 <- ggplot(data = party_data_long_nogy %>% 
         group_by(name) %>% 
         filter(order == max(order)),
       aes(x=name, y = csum, fill=name)) +
  geom_col() + 
  scale_fill_manual(values = values) +  
  theme_bw() +
  theme(panel.background = element_rect(fill="white"), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = "none"))

# Second plot: running scores over the event
(p2 <- ggplot(data = party_data_long_nogy, mapping = aes(x = order, y = csum, group = name)) +
  geom_line(aes(color = name)) +
  geom_point(aes(color = name)) +
  theme_minimal() +
  scale_color_manual(values = values) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "Score"))

# Final plot: inset plot with running scores
(scores_plot <- p2 + annotation_custom(grob=ggplotGrob(p1),
                       ymin = 150, ymax=400, xmin=1, xmax=25) +
  ggtitle(label = "Scores from post-defense Zoom party (June 2021)",
          subtitle = "Final scores inset"))

ggsave("plots/scores.jpeg", scores_plot)

# Word cloud --------------------------------------------------------------
# Generate frequency data from categories
party_data_wc <- table(party_data$cats)

# Generate word cloud for categories
set.seed(2525)
wordcloud(names(party_data_wc), as.numeric(party_data_wc), min.freq=1, max.words=40,
          random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=values)
