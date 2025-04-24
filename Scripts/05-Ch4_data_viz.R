#### Preamble ####
# Purpose: Data viz for Chapter 4
# Author: Inessa De Angelis
# Date: 3 June 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(scales)

#### Read in dataset ####
by_debates_final <- read_csv(file = "Outputs/Data/by_debates_final.csv")

#### Figure 1 (presence of demands by debate) ####
## Create analysis dataset ##
by_debates_demands =
  by_debates_final |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 Consortium (FR)",
    Debate_number == "2008EnConsortium" ~ "2008 Consortium (EN)",
    Debate_number == "2011EnConsortium" ~ "2011 Consortium (EN)",
    Debate_number == "2011FrConsortium" ~ "2011 Consortium (FR)",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Consortium (FR)",
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
  select(Election_year, Debate_number, demands_in_words) 
by_debates_demands

## Data Visualization ##
# Code referenced from: https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/
# & https://ggplot2-book.org/annotations 
# & https://stackoverflow.com/questions/48692705/text-repel-with-a-position-argument-in-ggplot-r

#jpeg("Ch4_figure1.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_demands, aes(Election_year, demands_in_words / 100)) +
  geom_point() +
  ggrepel::geom_text_repel(
    data = by_debates_demands,
    aes(label = Debate_number),
    size = 3,
    alpha = 0.7,
    segment.size = .25,
    segment.alpha = .8,
    force = 20, 
    max.overlaps = 6,
    direction = "both"
  ) +
  labs(x = "Election year", y = "Demands in words") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black")
#dev.off()

#### Figure 2 (quality of justification over time) ####
## Create analysis dataset ##
by_debates_justification =
  by_debates_final |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 Consortium (FR)",
    Debate_number == "2008EnConsortium" ~ "2008 Consortium (EN)",
    Debate_number == "2011EnConsortium" ~ "2011 Consortium (EN)",
    Debate_number == "2011FrConsortium" ~ "2011 Consortium (FR)",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Consortium (FR)",
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
  select(Election_year, Debate_number, dqi_justification)
by_debates_justification

## Data Visualization ##
#jpeg("Ch4_figure2.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_justification, aes(Election_year, dqi_justification)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_justification, aes(label = Debate_number), size = 3.5) +
  labs(x = "Election year", y = "Level of justification") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 10)) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.text.y.left = element_text(size = 10)) +
  theme(axis.title.y.left = element_text(size = 12))

p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black") 
#dev.off()

#### Figure 3 (share of interrupted participation over time) ####
## Create analysis dataset ##
# Code referenced from: https://www.reddit.com/r/Rlanguage/comments/aw3nkb/subtracting_data_in_column_from_data_in_another/
by_debates_interrupted =
  by_debates_final |>
  select(Election_year, Debate_number, dqi_normal_participation) |>
  mutate(all_participation = c("100"),
         .after = Debate_number) |>
  mutate(all_participation = as.numeric(all_participation)) |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 Consortium (FR)",
    Debate_number == "2008EnConsortium" ~ "2008 Consortium (EN)",
    Debate_number == "2011EnConsortium" ~ "2011 Consortium (EN)",
    Debate_number == "2011FrConsortium" ~ "2011 Consortium (FR)",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Consortium (FR)",
    Debate_number == "2015Munk" ~ "2015 Munk",
    Debate_number == "2015TVA" ~ "2015 TVA",
    Debate_number == "2019Macleans" ~ "2019 Macleans",
    Debate_number == "2019TVA" ~ "2019 TVA",
    Debate_number == "2019EnLDC" ~ "2019 EN LDC",
    Debate_number == "2019FrLDC" ~ "2019 FR LDC",
    Debate_number == "2021TVA" ~ "2021 TVA",
    Debate_number == "2021FrLDC" ~ "2021 FR LDC",
    Debate_number == "2021EnLDC" ~ "2021 EN LDC")) |>
select(Election_year, Debate_number, all_participation, dqi_normal_participation)
by_debates_interrupted

by_debates_interrupted_final <- by_debates_interrupted |> mutate (all_participation - dqi_normal_participation) |>
  rename(
   dqi_interrupted_participation = "all_participation - dqi_normal_participation") |>
  select(Election_year, Debate_number, dqi_normal_participation, dqi_interrupted_participation)
by_debates_interrupted_final

## Data Visualization ##
#jpeg("Ch4_figure3.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_interrupted_final, aes(Election_year, dqi_interrupted_participation/100)) + 
  geom_point() + 
  ggrepel::geom_text_repel(
    data = by_debates_interrupted_final,
    aes(label = Debate_number), size = 3, box.padding = 0.4) +
  labs(x = "Election year", y = "Interrupted Participation") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black") 
#dev.off()

#### Figure 4 (respect for demands over time) ####
## Create analysis dataset ##
by_debates_respect =
  by_debates_final |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 Consortium (FR)",
    Debate_number == "2008EnConsortium" ~ "2008 Consortium (EN)",
    Debate_number == "2011EnConsortium" ~ "2011 Consortium (EN)",
    Debate_number == "2011FrConsortium" ~ "2011 Consortium (FR)",
    Debate_number == "2015Macleans" ~ "2015 Macleans",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Consortium (FR)",
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
  select(Election_year, Debate_number, dqi_respect_demands)
by_debates_respect

## Data Visualization ##
#jpeg("Ch4_figure4.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_respect, aes(Election_year, dqi_respect_demands/100)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_respect, aes(label = Debate_number), size = 3.5, box.padding = 0.4) +
  labs(x = "Election year", y = "Respect for demands") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(labels = scales::percent) +
  theme_linedraw() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(axis.title.y.left = element_text(size = 13))

p + geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "black") 
#dev.off()