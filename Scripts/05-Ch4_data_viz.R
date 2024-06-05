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
by_debates_cleaned <- read_csv(file = "Outputs/Data/by_debates_cleaned.csv")

#### Figure 1 (presence of demands by debate) ####
## Create analysis dataset ##
by_debates_demands =
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
  select(election_year, debate_number, demands_in_words)
by_debates_demands

## Data Visualization ##
# Code referenced from: https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/
# & https://ggplot2-book.org/annotations

# jpeg("Ch4_figure1.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_demands, aes(election_year, demands_in_words)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_demands, aes(label = debate_number)) +
  labs(x = "Election year", y = "Demands in words (%)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(breaks = breaks_width(5)) +
  guides(x = guide_axis(angle = 55)) +
  theme_minimal()

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 

# dev.off()

#### Figure 2 (quality of justification over time) ####
## Create analysis dataset ##
by_debates_justification =
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
  select(election_year, debate_number, dqi_justification)
by_debates_justification

## Data Visualization ##

# jpeg("Ch4_figure2.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_justification, aes(election_year, dqi_justification)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_justification, aes(label = debate_number)) +
  labs(x = "Election year", y = "Level of justification") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  guides(x = guide_axis(angle = 55)) +
  theme_minimal()

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 

# dev.off()

#### Figure 3 (share of interrupted participation over time) ####
## Create analysis dataset ##
# Code referenced from: https://www.reddit.com/r/Rlanguage/comments/aw3nkb/subtracting_data_in_column_from_data_in_another/
by_debates_interrupted =
  by_debates_cleaned |>
  select(election_year, debate_number, dqi_normal_participation) |>
  mutate(all_participation = c("100"),
         .after = debate_number) |>
  mutate(all_participation = as.numeric(all_participation)) |>
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
select(election_year, debate_number, all_participation, dqi_normal_participation)
by_debates_interrupted

by_debates_interrupted_final <- by_debates_interrupted |> mutate (all_participation - dqi_normal_participation) |>
  rename(
   dqi_interrupted_participation = "all_participation - dqi_normal_participation"
  ) |>
  select(election_year, debate_number, dqi_normal_participation, dqi_interrupted_participation)
by_debates_interrupted_final

## Data Visualization ##

# jpeg("Ch4_figure3.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_interrupted_final, aes(election_year, dqi_interrupted_participation)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_interrupted_final, aes(label = debate_number)) +
  labs(x = "Election year", y = "Interrupted Participation (%)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  guides(x = guide_axis(angle = 55)) +
  theme_minimal()

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 

# dev.off()

#### Figure 4 (respect for demands over time) ####
## Create analysis dataset ##
by_debates_respect =
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
  select(election_year, debate_number, dqi_respect_demands)
by_debates_respect

## Data Visualization ##

# jpeg("Ch4_figure4.jpeg", units="in", width=9, height=5, res=500) 
p <- ggplot(by_debates_respect, aes(election_year, dqi_respect_demands)) + 
  geom_point() + 
  ggrepel::geom_text_repel(data = by_debates_respect, aes(label = debate_number)) +
  labs(x = "Election year", y = "Respect for demands (%)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 13)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  guides(x = guide_axis(angle = 55)) +
  theme_minimal()

p + geom_smooth(method = "lm", se = FALSE, color = "royalblue4") 

# dev.off()