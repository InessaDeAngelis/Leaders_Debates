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
library(ggpmisc)
library(ggpubr)

#### Read in dataset ####
by_debates_cleaned <- read_csv(file = "Outputs/Data/by_debates_cleaned.csv")

#### Figure 5 (Share of articles using strategic frames over time) ####
## Create analysis dataset ##
by_debates_strategic_frame =
  by_debates_cleaned |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 FR Consortium",
    Debate_number == "2008EnConsortium" ~ "2008 EN Consortium",
    Debate_number == "2011EnConsortium" ~ "2011 EN Consortium",
    Debate_number == "2011FrConsortium" ~ "2011 FR Consortium",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Radio-Canada",
    Debate_number == "2015Munk" ~ "2015 Munk",
    Debate_number == "2015TVA" ~ "2015 TVA",
    Debate_number == "2019Macleans" ~ "2019 Macleans",
    Debate_number == "2019TVA" ~ "2019 TVA",
    Debate_number == "2019EnLDC" ~ "2019 EN LDC",
    Debate_number == "2019FrLDC" ~ "2019 FR LDC",
    Debate_number == "2021TVA" ~ "2021 TVA",
    Debate_number == "2021FrLDC" ~ "2021 FR LDC",
    Debate_number == "2021EnLDC" ~ "2021 EN LDC"
  )) |>
  select(Election_year, Debate_number, news_strategic_frame)
by_debates_strategic_frame

## Data Visualization ##
# Code referenced from: https://stackoverflow.com/questions/73995249/how-to-fill-the-background-of-a-stat-poly-eq-equation-ggpmisc-using-ggplot2

# jpeg("Ch5_figure5.jpeg", units="in", width=9, height=7, res=500) 
p <- ggplot(by_debates_strategic_frame, aes(Election_year, news_strategic_frame/100)) + 
  geom_point() +
  ggrepel::geom_text_repel(
    data = by_debates_strategic_frame,
    aes(label = Debate_number),
    size = 4,
    alpha = 0.9,
    segment.size = .25,
    segment.alpha = .8,
    force = 1
  ) +
  labs(x = "Election year", y = "Strategic Frame") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") +
stat_poly_eq(rr.digits = 2, parse = TRUE, size = 4, label.x = 0.97, label.y = 0.97, geom = "label_npc", label.size = 0.25)
# dev.off()

#### Figure 6 (Share of articles using strategic frames and addressing substance over time) ####
## Create analysis dataset ##
by_debates_substance =
  by_debates_cleaned |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 FR Consortium",
    Debate_number == "2008EnConsortium" ~ "2008 EN Consortium",
    Debate_number == "2011EnConsortium" ~ "2011 EN Consortium",
    Debate_number == "2011FrConsortium" ~ "2011 FR Consortium",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Radio-Canada",
    Debate_number == "2015Munk" ~ "2015 Munk",
    Debate_number == "2015TVA" ~ "2015 TVA",
    Debate_number == "2019Macleans" ~ "2019 Macleans",
    Debate_number == "2019TVA" ~ "2019 TVA",
    Debate_number == "2019EnLDC" ~ "2019 EN LDC",
    Debate_number == "2019FrLDC" ~ "2019 FR LDC",
    Debate_number == "2021TVA" ~ "2021 TVA",
    Debate_number == "2021FrLDC" ~ "2021 FR LDC",
    Debate_number == "2021EnLDC" ~ "2021 EN LDC"
  )) |>
  select(Election_year, Debate_number, news_substance)
by_debates_substance

## Data Visualization ##
# jpeg("Ch5_figure6.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_substance, aes(Election_year, news_substance/100)) + 
  geom_point() +
  ggrepel::geom_text_repel(
    data = by_debates_substance,
    aes(label = Debate_number),
    size = 4,
    alpha = 0.9,
    segment.size = .25,
    segment.alpha = .8,
    force = 1
  ) +
  labs(x = "Election year", y = "Substance") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") +
stat_poly_eq(rr.digits = 2, parse = TRUE, size = 4, geom = "label_npc", label.size = 0.25)
# dev.off()

#### Figure 7 (Share of articles using strategic frames and addressing substance over time) ####
## Create analysis dataset ##
# Add in data from table #
strategic_substance_data <- 
  data.frame(
  Debate_number = c(
    "2008FrConsortium",
    "2008EnConsortium",
    "2011EnConsortium",
    "2011FrConsortium",
    "2015Macleans",
    "2015Globe&Mail",
    "2015Radio-Canada",
    "2015Munk",
    "2015TVA",
    "2019Macleans",
    "2019TVA",
    "2019EnLDC",
    "2019FrLDC",
    "2021TVA",
    "2021FrLDC",
    "2021EnLDC") ,
  strategic_and_substantive = c(
    24.4,
    25.6,
    20.5,
    25.6,
    17.5,
    33.3,
    22.6,
    15.9,
    38.7,
    25.0,
    31.7,
    17.3,
    17.1,
    40.0,
    20.9,
    15.9
  ))
strategic_substance_data

# Add with existing columns from dataset #
strategic_sub =
  by_debates_cleaned |>
  select(Election_year, Debate_number)
strategic_sub

by_debates_strategic_sub =
  merge(
    strategic_sub,
    strategic_substance_data,
    by = "Debate_number"
  ) |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 FR Consortium",
    Debate_number == "2008EnConsortium" ~ "2008 EN Consortium",
    Debate_number == "2011EnConsortium" ~ "2011 EN Consortium",
    Debate_number == "2011FrConsortium" ~ "2011 FR Consortium",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Radio-Canada",
    Debate_number == "2015Munk" ~ "2015 Munk",
    Debate_number == "2015TVA" ~ "2015 TVA",
    Debate_number == "2019Macleans" ~ "2019 Macleans",
    Debate_number == "2019TVA" ~ "2019 TVA",
    Debate_number == "2019EnLDC" ~ "2019 EN LDC",
    Debate_number == "2019FrLDC" ~ "2019 FR LDC",
    Debate_number == "2021TVA" ~ "2021 TVA",
    Debate_number == "2021FrLDC" ~ "2021 FR LDC",
    Debate_number == "2021EnLDC" ~ "2021 EN LDC"
  )) 
by_debates_strategic_sub 

## Data Visualization ##
# Code referenced from: https://stackoverflow.com/questions/77609363/stat-poly-eq-erroring-when-using-grouped-data-in-a-ggplot
# &: https://stackoverflow.com/questions/7549694/add-regression-line-equation-and-r2-on-graph

# jpeg("Ch5_figure7.jpeg", units="in", width=9, height=7, res=500) 
p <- ggplot(by_debates_strategic_sub, aes(Election_year, strategic_and_substantive/100)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_strategic_sub, aes(label = Debate_number)) +
  labs(x = "Election year", y = "Strategic and Substantive") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13)) 

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") +
stat_cor(aes(label =  ..rr.label..))
# dev.off()

#### Figure 8 (Share of articles focusing on news format) ####
## Create analysis dataset ##
by_debates_format =
  by_debates_cleaned |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 FR Consortium",
    Debate_number == "2008EnConsortium" ~ "2008 EN Consortium",
    Debate_number == "2011EnConsortium" ~ "2011 EN Consortium",
    Debate_number == "2011FrConsortium" ~ "2011 FR Consortium",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Radio-Canada",
    Debate_number == "2015Munk" ~ "2015 Munk",
    Debate_number == "2015TVA" ~ "2015 TVA",
    Debate_number == "2019Macleans" ~ "2019 Macleans",
    Debate_number == "2019TVA" ~ "2019 TVA",
    Debate_number == "2019EnLDC" ~ "2019 EN LDC",
    Debate_number == "2019FrLDC" ~ "2019 FR LDC",
    Debate_number == "2021TVA" ~ "2021 TVA",
    Debate_number == "2021FrLDC" ~ "2021 FR LDC",
    Debate_number == "2021EnLDC" ~ "2021 EN LDC"
  )) |>
  select(Election_year, Debate_number, news_format)
by_debates_substance

## Data Visualization ##
# jpeg("Ch5_figure8.jpeg", units="in", width=9, height=7, res=500) 
p <- ggplot(by_debates_format, aes(Election_year, news_format/100)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_format, aes(label = Debate_number)) +
  labs(x = "Election year", y = "News format") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13)) 

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") +
stat_poly_eq(rr.digits = 2, parse = TRUE, size = 4, geom = "label_npc", label.size = 0.25)
# dev.off()
