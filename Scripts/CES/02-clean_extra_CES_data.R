#### Preamble ####
# Purpose: Clean extra CES data for Chapter 2
# Author: Inessa De Angelis
# Date: 9 May 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

ces2019_pol_interest <- read_csv("raw_ces2019_pol_interest.csv")
ces2021_pol_interest <- read_csv("raw_ces2021_pol_interest.csv")

#### Clean 2019 data ####
## EN ##
ces2019_EN_debate <- ces2019_pol_interest |>
  select(-c(cps19_debate_fr, cps19_premier_name, cps19_finmin_name, cps19_yob)) |>
  drop_na(cps19_debate_en, pes19_follow_pol, pes19_interest_1, cps19_interest_gen_1, cps19_interest_elxn_1) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps19_debate_en)) 

# Save #
write_csv(ces2019_EN_debate, "ces2019_EN_debate.csv")

## FR ##
ces2019_FR_debate <- ces2019_pol_interest |>
  select(-c(cps19_debate_en, cps19_premier_name, cps19_finmin_name, cps19_yob)) |>
  drop_na(cps19_debate_fr, pes19_follow_pol, pes19_interest_1, cps19_interest_gen_1, cps19_interest_elxn_1) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps19_debate_fr)) 

# Save #
write_csv(ces2019_FR_debate, "ces2019_FR_debate.csv")

#### Clean 2021 data ####
## EN ##
ces2021_EN_debate <- ces2021_pol_interest |>
  select(-c(cps21_debate_fr, cps21_premier_name, cps21_finmin_name, cps21_yob)) |>
  drop_na(cps21_debate_en, pes21_follow_pol, cps21_interest_gen_1, cps21_interest_elxn_1) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps21_debate_en)) 

# Save #
write_csv(ces2021_EN_debate, "ces2021_EN_debate.csv")

## FR ##
ces2021_FR_debate <- ces2021_pol_interest |>
  select(-c(cps21_debate_en, cps21_premier_name, cps21_finmin_name, cps21_yob)) |>
  drop_na(cps21_debate_fr, pes21_follow_pol, cps21_interest_gen_1, cps21_interest_elxn_1) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps21_debate_fr)) 

# Save #
write_csv(ces2021_FR_debate, "ces2021_FR_debate.csv")