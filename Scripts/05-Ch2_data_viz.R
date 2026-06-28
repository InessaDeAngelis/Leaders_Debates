#### Preamble ####
# Purpose: Visualize time allocations for Chapter 2
# Author: Inessa De Angelis
# Date: 17 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(hrbrthemes)

time_data <- read_csv("Outputs/Data/time_allocation_data.csv")

#### Prepare data ####
## Update names and fix 2015 Consortium debate name ##
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

## Prep for plotting: fit regression model ##
fit <- lm(Average_time ~ Election_year, data = time_data)

## Create new df for the regression line (stop at 2020.8) ##
# Code referenced from: https://stackoverflow.com/questions/15633714/adding-a-regression-line-on-a-ggplot
new_df <- data.frame(Election_year = seq(2008, 2020.8, length.out = 100))

## Predict values ##
new_df$Average_time <- predict(fit, newdata = new_df)

#### Data visualization ####
jpeg("Ch2_figure1.jpeg", units = "in", width = 9, height = 6, res = 300)
ggplot(time_data, aes(Election_year, Average_time)) +
  geom_line(data = new_df,
    aes(x = Election_year, y = Average_time),
    color = "#123A7A", linewidth = 0.8) +
  geom_point(color = "black", size = 1) +
  ggrepel::geom_text_repel(
    aes(label = Debate_number),
    size = 3, family = "Arial narrow",
    box.padding = 0.4, point.padding = 0.2,
    segment.color = "grey70", segment.size = 0.3, force = 1.5) +
  labs(x = "Year", y = "Average time per question per speaker (minutes)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_ipsum() +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "grey94", linewidth = 0.25),
    axis.line.x = element_line(color = "grey35", linewidth = 0.25),
    axis.line.y = element_line(color = "grey35", linewidth = 0.25),
    axis.text.x = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 13, face = "bold"),
    axis.text.y.left = element_text(size = 9, face = "bold"),
    axis.title.y.left = element_text(size = 12, face = "bold"))
dev.off()
