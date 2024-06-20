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
p <- ggplot(by_debates_strategic_frame, aes(election_year, news_strategic_frame/100)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_strategic_frame, aes(label = debate_number)) +
  labs(x = "Election year", y = "Strategic Frame") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 
# dev.off()

## Get r^2 to add ##
mod <- lm(election_year ~ news_strategic_frame, data = by_debates_strategic_frame)
s <- summary(mod)
s$r.squared # extract r^2

#### Figure 6 (Share of Articles Using Strategic Frames and Addressing Substance Over Time) ####
## Create analysis dataset ##
by_debates_substance =
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
  select(election_year, debate_number, news_substance)
by_debates_substance

## Data Visualization ##
# jpeg("Ch5_figure6.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_substance, aes(election_year, news_substance/100)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_substance, aes(label = debate_number)) +
  labs(x = "Election year", y = "Strategic Frame") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 
# dev.off()

## Get r^2 to add ##
mod2 <- lm(election_year ~ news_substance, data = by_debates_substance)
s2 <- summary(mod2)
s2$r.squared # extract r^2

#### Figure 7 (Share of Articles Using ...) ####
## Create analysis dataset ##

#### Figure 8 (Share of Articles Using Strategic Frame and News Format) ####
## Create analysis dataset ##
by_debates_format =
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
  select(election_year, debate_number, news_format)
by_debates_substance

## Data Visualization ##
# jpeg("Ch5_figure8.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_format, aes(election_year, news_format/100)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_format, aes(label = debate_number)) +
  labs(x = "Election year", y = "Strategic Frame") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 

# dev.off()

## Get r^2 to add ##
mod4 <- lm(election_year ~ news_format, data = by_debates_format)
s4 <- summary(mod4)
s4$r.squared # extract r^2
