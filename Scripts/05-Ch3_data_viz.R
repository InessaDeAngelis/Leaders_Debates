#### Preamble ####
# Purpose: Visualize who is asking for Ch3
# Author: Inessa De Angelis
# Date: 4 December 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
## Read in packages ##
library(tidyverse)
library(ggplot2)

## Read in dataset ##
all_qs <- read_csv("Outputs/Data/all_qs.csv")

## Fix up dataset for visualization ##
all_qs_viz <-
  all_qs |>
  #filter(Percentage >= 5.0) |>
  #filter(Whos_asking == "Moderator") |>
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

#### Visualize data ####
## Helpful suggestions: https://www.datacamp.com/tutorial/facets-ggplot-r

#jpeg("whos_asking.jpeg", units="in", width=9, height=5, res=300) 

ggplot(all_qs_viz, aes(Issue, Percentage/1000)) +
  geom_bar(stat='identity', fill="black") +
  facet_wrap(~Whos_asking) +
  labs(
    x = "Issue",
    y = "Percentage of questions asked",
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels=scales::percent) +
  theme(strip.text.x = element_text(size = 15)) +
  theme(axis.text.x =  element_text(size = 16, angle = 75, hjust=1)) +
  theme(axis.text.y = element_text(size = 12)) + 
  theme(axis.title.x = element_text(size = 16)) +
  theme(axis.title.y = element_text(size = 16)) +
  theme(legend.title = element_text(size = 13)) +
  theme(legend.text = element_text(size = 10)) 

#dev.off()
