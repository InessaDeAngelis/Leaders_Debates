#### Preamble ####
# Purpose: Gets summary stats for the YouTube comments analyzed for Chapter 5
# Author: Inessa De Angelis
# Date: 16 December 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)

#### Read in datasets ####
all_debate_comments <- read_csv("Outputs/Data/YouTube/all_debate_comments.csv")

## Re-code the coded comments so everything is on a binary ##
all_debate_comments <- all_debate_comments |>
  mutate(
    important_issues = ifelse(important_issues == "0", "0", "1"),
    moderation = ifelse(moderation == "0", "0", "1"),
    format = ifelse(format == "0", "0", "1"),
    production = ifelse(production == "0", "0", "1"),
    won = ifelse(won == "0", "0", "1"),
    lost = ifelse(lost == "0", "0", "1"))

#### Get summary stats ####
imp_issues <- all_debate_comments  |>
  filter(important_issues == 1) |>
  count() |>
  mutate(proportion = n / (sum(11832))* 100) 
imp_issues