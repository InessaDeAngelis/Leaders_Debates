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

#### Get summary stats ####
imp_issues <- all_debate_comments  |>
  filter(moderation == 1) |>
  count() |>
  mutate(proportion = n / (sum(11832))* 100) 
imp_issues

## By debate ##
by_debate <- all_debate_comments |>
  filter(Debate_number == "2021FrLDC") |>
  filter(lost == 1) |>
  count() |>
  mutate(proportion = n / (sum(1192))* 100) 
by_debate