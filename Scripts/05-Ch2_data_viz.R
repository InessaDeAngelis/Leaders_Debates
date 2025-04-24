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

## Fix 2015 Consotrium debate name ##
time_data <- time_data |>
    mutate("Debate_number" = case_when(
      Debate_number == "2008FrConsortium" ~ "2008 Consortium (FR)",
      Debate_number == "2008EnConsortium" ~ "2008 Consortium (EN)",
      Debate_number == "2011EnConsortium" ~ "2011 Consortium (EN)",
      Debate_number == "2011FrConsortium" ~ "2011 Consortium (FR)",
      Debate_number == "2015Macleans" ~ "2015 Maclean's",
      Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
      Debate_number == "2015Radio-Canada" ~ "2015 Consortium (FR)",
      Debate_number == "2015Munk" ~ "2015 Munk",
      Debate_number == "2015TVA" ~ "2015 TVA",
      Debate_number == "2019Macleans" ~ "2019 Maclean's",
      Debate_number == "2019TVA" ~ "2019 TVA",
      Debate_number == "2019EnLDC" ~ "2019 LDC (EN)",
      Debate_number == "2019FrLDC" ~ "2019 LDC (FR)",
      Debate_number == "2021TVA" ~ "2021 TVA",
      Debate_number == "2021FrLDC" ~ "2021 LDC (FR)",
      Debate_number == "2021EnLDC" ~ "2021 LDC (EN)"))

#### Data visualization ####
#jpeg("Ch2_figure1.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(time_data, aes(Election_year, Average_time)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = time_data, aes(label = Debate_number), size = 3) +
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
