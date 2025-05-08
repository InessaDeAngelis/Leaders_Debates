#### Preamble ####
# Purpose: Download CES data, again
# Author: Inessa De Angelis
# Date: 1 May 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(labelled)
library(haven)
library(cesR) 

#### Download data using cesR package ####
## Call all surveys
get_cescodes()

## Get 2019 web and phone survey
get_ces("ces2019_web")
get_ces("ces2019_phone")

#### Convert values to factor type ####
ces2019_web <- to_factor(ces2019_web)
head(ces2019_web)

ces2019_phone <- to_factor(ces2019_phone)
head(ces2019_phone)

#### Select columns of interest ####
#     cps19_news_cons,

raw_ces2019_web <-
  ces2019_web |>
  select(
    cps19_debate_en,
    cps19_debate_fr,
    pes19_follow_pol,
    pes19_interest_1,
    cps19_govt_confusing,
    cps19_premier_name,
    cps19_finmin_name) 
raw_ces2019_web

#### EDA to figure out how I want to clean this dataset ####
raw_ces2019_EN_debate <- raw_ces2019_web |>
  drop_na(cps19_debate_en) |>
  drop_na(pes19_follow_pol) |>
  drop_na(pes19_interest_1) |>
select(- cps19_debate_fr)

raw_ces2019_EN_debate |> 
  group_by(cps19_debate_en, pes19_follow_pol) |> 
  count() |> 
  ungroup() |> 
  mutate(percentage = n / sum(n) * 100)

raw_ces2019_FR_debate <- raw_ces2019_web |>
  drop_na(cps19_debate_fr) |>
  drop_na(pes19_follow_pol) |>
  select(cps19_debate_fr, pes19_follow_pol)

write_csv(raw_ces2019_EN_debate, "ces2019_EN_debate.csv")

# Read in general 2021 data 
raw_ces2021_web <- read_dta(here::here("Inputs/Data/CES/2021_CES_v2.dta"))
raw_ces2021_web <- to_factor(raw_ces2021_web)

##
raw_ces2021_web <-
  raw_ces2021_web |>
  select(
    cps21_debate_en,
    cps21_debate_fr,
    pes21_follow_pol,
    pes21_emb4_1,
    pes21_emb4_2,
    pes21_emb4_3,
    pes21_emb4_4,
    pes21_emb4_5,
    pes21_emb4_6,
    pes21_emb4_7,
    pes21_emb4_8,
    pes21_emb4_9,
    pes21_emb4_10,
    pes21_emb4_11,
    pes21_emb4_12,
    pes21_emb4_13,
    pes21_emb4_14,
    pes21_emb4_15,
    pes21_emb4_16,
    pes21_emb4_11_TEXT,
    pes21_emb4_14_TEXT
  ) 

raw_ces2021_EN_debate <- raw_ces2021_web |>
  drop_na(cps21_debate_en) |>
  drop_na(pes21_follow_pol) |>
  select(cps21_debate_en, pes21_follow_pol) 

write_csv(raw_ces2021_EN_debate, "ces2021_EN_debate.csv")

raw_ces2021_FR_debate <- raw_ces2021_web |>
  drop_na(cps21_debate_fr) |>
  drop_na(pes21_follow_pol) |>
  select(cps21_debate_fr, pes21_follow_pol) 

write_csv(raw_ces2021_FR_debate, "ces2021_FR_debate.csv")

raw_ces2021_EN_debate |> 
  group_by(cps21_debate_en, pes21_follow_pol) |> 
  count() |> 
  ungroup() |> 
  mutate(percentage = n / sum(n) * 100)
