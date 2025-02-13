#### Preamble ####
# Purpose: Calculate various DQI things for Ch4
# Author: Inessa De Angelis
# Date: 10 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(summarytools)

dqi_by_speech_final <- read_csv("Outputs/Data/dqi_by_speech_final.csv")

#### Interruptions ####
interruptions_dqi <- dqi_by_speech_final |>
  filter(Interruption == "Interruption") |>
  mutate(Gender = if_else(Speaker %in% c("May", "Paul"), "Woman", "Man"), .after = Speaker) |>
  mutate(Background = if_else(Speaker %in% c("Singh", "Paul"), "Racialized", "White"), .after = Gender) |>
  mutate(Incumbent = if_else((Speaker == "Trudeau" & Election_year >= 2019) | 
      (Speaker == "Harper" & Election_year <= 2015), 
        "Incumbent", 
        "Not Incumbent"), .after = Background)

write_csv(interruptions_dqi, "Outputs/Data/interruptions_dqi.csv")

interruptions_dqi |> group_by(Gender, Incumbent) |> count() |> print(n=29)

#### Respect for demands ####
respect_dqi <- dqi_by_speech_final |>
  filter(Respect_for_demands == "Respect") |>
  mutate(Gender = if_else(Speaker %in% c("May", "Paul"), "Woman", "Man"), .after = Speaker) |>
  mutate(Background = if_else(Speaker %in% c("Singh", "Paul"), "Racialized", "White"), .after = Gender) |>
  mutate(Incumbent = if_else((Speaker == "Trudeau" & Election_year >= 2019) | 
    (Speaker == "Harper" & Election_year <= 2015), 
      "Incumbent", 
      "Not Incumbent"), .after = Background)

write_csv(respect_dqi, "Outputs/Data/respect_dqi.csv")
