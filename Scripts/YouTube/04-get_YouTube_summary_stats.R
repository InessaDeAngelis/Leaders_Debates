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
## All debates ##
imp_issues <- all_debate_comments  |>
  filter(leaders_party == 1) |>
  count() |>
  mutate(proportion = n / (sum(11832))* 100) 
imp_issues

## 2015 Macleans ##
by_debate <- all_debate_comments |>
  filter(Debate_number == "2015Macleans") |>
  filter(leaders_party == 1) |>
  count() |>
  mutate(proportion = n / (sum(1331))* 100) 
by_debate

## 2015 Globe & Mail ##
by_debate <- all_debate_comments |>
  filter(Debate_number == "2015Globe&Mail") |>
  filter(lost == 1) |>
  count() |>
  mutate(proportion = n / (sum(750))* 100) 
by_debate

## 2019 LDC EN ##
by_debate <- all_debate_comments |>
  filter(Debate_number == "2019EnLDC") |>
  filter(leaders_party == 1) |>
  count() |>
  mutate(proportion = n / (sum(4525))* 100) 
by_debate

## 2019 LDC FR ##
by_debate <- all_debate_comments |>
  filter(Debate_number == "2019FrLDC") |>
  filter(leaders_party == 1) |>
  count() |>
  mutate(proportion = n / (sum(551))* 100) 
by_debate

## 2021 LDC EN ##
by_debate <- all_debate_comments |>
  filter(Debate_number == "2021EnLDC") |>
  filter(lost == 1) |>
  count() |>
  mutate(proportion = n / (sum(3483))* 100) 
by_debate

## 2021 LDC FR ##
by_debate <- all_debate_comments |>
  filter(Debate_number == "2021FrLDC") |>
  filter(leaders_party == 1) |>
  count() |>
  mutate(proportion = n / (sum(1192))* 100) 
by_debate