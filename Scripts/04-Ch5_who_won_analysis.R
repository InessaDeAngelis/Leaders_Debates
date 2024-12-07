#### Preamble ####
# Purpose: Analyzes YouTube, Newspaper, and CES data to see who "won" and who "lost" for Chapter 5
# Author: Inessa De Angelis
# Date: 7 December 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

## Read in the datasets ##
# YouTube #
debate_comments <- read_csv("Outputs/Data/YouTube/debate_comments_analyzed.csv")
debate_comments_fr <- read_csv("Outputs/Data/YouTube/debate_comments_fr_analyzed.csv")

# 2008 CES #
ces2008_who_won <- read_csv("Outputs/Data/CES/ces2008_who_won.csv")

# Newspaper #
newspaper_data_final_new <- read_csv("Outputs/Data/newspaper_data_final_new.csv")

#### CES Analysis ####
## Who won - EN ##
who_won_EN <- ces2008_who_won |>
  drop_na(your_opinion_best_EN) |>
  group_by(your_opinion_best_EN) |>
  count() |>
  mutate(proportion = n / (sum(456))* 100) 
who_won_EN

## Who won - FR ##
who_won_FR <- ces2008_who_won |>
  drop_na(your_opinion_best_FR) |>
  group_by(your_opinion_best_FR) |>
  count() |>
  mutate(proportion = n / (sum(205))* 100) 
who_won_FR

## Who lost - EN ##
who_lost_EN <- ces2008_who_won |>
  drop_na(your_opinion_worst_EN) |>
  group_by(your_opinion_worst_EN) |>
  count() |>
  mutate(proportion = n / (sum(437))* 100) 
who_lost_EN

## Who lost - FR ##
who_lost_FR <- ces2008_who_won |>
  drop_na(your_opinion_worst_FR) |>
  group_by(your_opinion_worst_FR) |>
  count() |>
  mutate(proportion = n / (sum(213))* 100) 
who_lost_FR