#### Preamble ####
# Purpose: Visualize time allocations for Chapter 2
# Author: Inessa De Angelis
# Date: 17 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(ggplot2)

time_data <- read_csv("Outputs/Data/time_allocation_data.csv")

#### Data visualization ####
#jpeg("Ch2_figure1.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(time_data, aes(Election_year, Average_time)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = time_data, aes(label = Debate_number), size = 3.5) +
  labs(x = "Election year", y = "Average time per question per speaker (minutes)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.y.left = element_text(size = 10)) +
  theme(axis.title.y.left = element_text(size = 12))

p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black") 
#dev.off()
