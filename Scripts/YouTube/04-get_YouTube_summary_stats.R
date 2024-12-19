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
# note to self: I wonder if I can do this with 0 < = 0 / 1 > = 1, etc. #
all_debate_comments <-
  all_debate_comments |>
  mutate("important_issues" = case_when( 
    important_issues == "0" ~ "0",
    important_issues == "1" ~ "1",
    important_issues == "2" ~ "1",
    important_issues == "3" ~ "1",
    important_issues == "4" ~ "1",
    important_issues == "5" ~ "1",
    important_issues == "6" ~ "1",
    important_issues == "7" ~ "1",
    important_issues == "8" ~ "1",
    important_issues == "9" ~ "1",
    important_issues == "10" ~ "1",
    important_issues == "11" ~ "1",
    important_issues == "14" ~ "1")) |>
  mutate("moderation" = case_when( 
    moderation == "0" ~ "0",
    moderation == "1" ~ "1",
    moderation == "2" ~ "1",
    moderation == "3" ~ "1",
    moderation == "4" ~ "1",
    moderation == "12" ~ "1")) |>
  mutate("format" = case_when( 
    format == "0" ~ "0",
    format == "1" ~ "1",
    format == "2" ~ "1",
    format == "3" ~ "1")) |>
  mutate("production" = case_when( 
    production == "0" ~ "0",
    production == "1" ~ "1",
    production == "2" ~ "1",
    production == "3" ~ "1")) |>
  mutate("won" = case_when( 
    won == "0" ~ "0",
    won == "1" ~ "1",
    won == "2" ~ "1",
    won == "3" ~ "1",
    won == "4" ~ "1",
    won == "5" ~ "1")) |>
  mutate("lost" = case_when( 
    lost == "0" ~ "0",
    lost == "1" ~ "1",
    lost == "2" ~ "1",
    lost == "3" ~ "1",
    lost == "4" ~ "1",
    lost == "5" ~ "1"))

#### Get summary stats ####
imp_issues <- all_debate_comments  |>
  filter(lost == 1) |>
  count() |>
  mutate(proportion = n / (sum(12915))* 100) 
imp_issues