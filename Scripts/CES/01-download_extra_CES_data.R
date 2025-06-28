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

#### Download and clean 2019 data ####
## Call all surveys
get_cescodes()

## Get 2019 web and phone survey
get_ces("ces2019_web")

## Convert values to factor type ##
ces2019_web <- to_factor(ces2019_web)
head(ces2019_web)

## Select columns of interest ##
raw_ces2019_web <- ces2019_web |>
  select(
    cps19_gender,
    cps19_age,
    cps19_yob,
    cps19_debate_en,
    cps19_debate_fr,
    pes19_follow_pol,
    pes19_interest_1,
    cps19_interest_gen_1,
    cps19_interest_elxn_1,
    cps19_govt_confusing,
    cps19_premier_name,
    cps19_finmin_name) 
raw_ces2019_web

## Save dataset ##
write_csv(raw_ces2019_web, "raw_ces2019_pol_interest.csv")

#### Download and clean 2021 data ####
raw_ces2021_web <- read_dta(here::here("Inputs/Data/CES/2021_CES_v2.dta"))
raw_ces2021_web <- to_factor(raw_ces2021_web)

## Select columns of interest ##
raw_ces2021_web <- raw_ces2021_web |>
  select(
    cps21_genderid,
    cps21_age,
    cps21_yob,
    cps21_debate_en,
    cps21_debate_fr,
    pes21_follow_pol,
    cps21_interest_elxn_1,
    cps21_interest_gen_1,
    cps21_govt_confusing,
    cps21_premier_name,
    cps21_finmin_name) 

## Save dataset ##
write_csv(raw_ces2021_web, "raw_ces2021_pol_interest.csv")
