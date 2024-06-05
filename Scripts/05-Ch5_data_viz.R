#### Preamble ####
# Purpose: Data viz for Chapter 5
# Author: Inessa De Angelis
# Date: 5 June 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(scales)

#### Read in dataset ####
by_debates_cleaned <- read_csv(file = "Outputs/Data/by_debates_cleaned.csv")

#### Figure 5 (share of articles addressing debate substance over time) ####
## Create analysis dataset ##
by_debates_strategic_frame =
  by_debates_cleaned |>
  mutate("debate_number" = case_when(
    debate_number == "2008FrConsortium" ~ "2008 FR Consortium",
    debate_number == "2008EnConsortium" ~ "2008 EN Consortium",
    debate_number == "2011EnConsortium" ~ "2011 EN Consortium",
    debate_number == "2011FrConsortium" ~ "2011 FR Consortium",
    debate_number == "2015Macleans" ~ "2015 Macleans",
    debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    debate_number == "2015Radio-Canada" ~ "2015 Radio-Canada",
    debate_number == "2015Munk" ~ "2015 Munk",
    debate_number == "2015TVA" ~ "2015 TVA",
    debate_number == "2019Macleans" ~ "2019 Macleans",
    debate_number == "2019TVA" ~ "2019 TVA",
    debate_number == "2019EnLDC" ~ "2019 EN LDC",
    debate_number == "2019FrLDC" ~ "2019 FR LDC",
    debate_number == "2021TVA" ~ "2021 TVA",
    debate_number == "2021FrLDC" ~ "2021 FR LDC",
    debate_number == "2021EnLDC" ~ "2021 EN LDC"
  )) |>
  select(election_year, debate_number, news_strategic_frame)
by_debates_strategic_frame

## Data Visualization ##

# jpeg("Ch5_figure5.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_strategic_frame, aes(election_year, news_strategic_frame)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_strategic_frame, aes(label = debate_number)) +
  labs(x = "Election year", y = "Strategic Frame") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  guides(x = guide_axis(angle = 55)) +
  theme_minimal()

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 

# dev.off()

## Get r^2 to add ##
mod <- lm(election_year ~ news_strategic_frame, data = by_debates_strategic_frame)
s <- summary(mod)
s$r.squared # extract r^2

