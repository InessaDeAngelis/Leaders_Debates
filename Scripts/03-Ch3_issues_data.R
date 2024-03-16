#### Preamble ####
# Purpose: Gets issues stats for Chapter 3
# Author: Inessa De Angelis
# Date: 14 March 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
  # 02-clean_data.R

#### Workspace setup ####
library(tidyverse)

#### Read in cleaned dataset ####
# Outputs/Data/
debate_questions_cleaned <- read_csv("Outputs/Data/debate_questions_cleaned.csv")

#### Create specific datasets by year ####
## 2008 debate ##
debate_questions_2008 = 
  debate_questions_cleaned |>
  filter(year == "2008") |>
  filter(!lead_followup == "Followup") |>
  select(unique_id,
         year,
         debate_number,
         language_of_question,
         lead_followup,
         primary_issue,
         secondary_issue,
         territory1,
         territory2,
         debate_lang) 
debate_questions_2008

## 2011 debate ##
debate_questions_2011 = 
  debate_questions_cleaned |>
  filter(year == "2011") |>
  filter(!lead_followup == "Followup") |>
  select(unique_id,
         year,
         debate_number,
         language_of_question,
         lead_followup,
         primary_issue,
         secondary_issue,
         territory1,
         territory2,
         debate_lang) 
debate_questions_2011

## 2015 debate ##
debate_questions_2015 = 
  debate_questions_cleaned |>
  filter(year == "2015") |>
  filter(!lead_followup == "Followup") |>
  select(unique_id,
         year,
         debate_number,
         language_of_question,
         lead_followup,
         primary_issue,
         secondary_issue,
         territory1,
         territory2,
         debate_lang) 
debate_questions_2015

## 2019 debate ##
debate_questions_2019 = 
  debate_questions_cleaned |>
  filter(year == "2019") |>
  filter(!lead_followup == "Followup") |>
  select(unique_id,
         year,
         debate_number,
         language_of_question,
         lead_followup,
         primary_issue,
         secondary_issue,
         territory1,
         territory2,
         debate_lang)
debate_questions_2019

## 2021 debate ##
debate_questions_2021 = 
  debate_questions_cleaned |>
  filter(year == "2021") |>
  filter(!lead_followup == "Followup") |>
  select(unique_id,
         year,
         debate_number,
         language_of_question,
         lead_followup,
         primary_issue,
         secondary_issue,
         territory1,
         territory2,
         debate_lang) 
debate_questions_2021

#### Primary & Secondary Issue Stats ####
# Code referenced from: https://stackoverflow.com/questions/76064350/count-number-of-rows-where-a-value-appears-in-any-of-two-columns-in-r
## 2008 ##
# EN #
debate_questions_2008_EN =
  debate_questions_2008 |>
  filter(debate_number == "2008_Consortium_en") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(8))) 
debate_questions_2008_EN

# FR #
debate_questions_2008_FR =
  debate_questions_2008 |>
  filter(debate_number == "2008_Consortium_Fr") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(14))) 
debate_questions_2008_FR

# All 2008 debates ##
debate_questions_2008_all =
  debate_questions_2008 |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(22))) 
debate_questions_2008_all

## 2011 ##
# EN #
debate_questions_2011_EN =
  debate_questions_2011 |>
  filter(debate_number == "2011_Consortium_en") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))) 
debate_questions_2011_EN 

# FR #
debate_questions_2011_FR =
  debate_questions_2011 |>
  filter(debate_number == "2011_Consortium_Fr") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))) 
debate_questions_2011_FR 

# All 2011 debates #
debate_questions_2011_all =
  debate_questions_2011 |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(16))) 
debate_questions_2011_all 

## 2015 ##
# TVA #
debate_questions_2015_TVA =
  debate_questions_2015 |>
  filter(debate_number == "2015_TVA") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(33))) 
debate_questions_2015_TVA

# Radio-Canada #
debate_questions_2015_RC =
  debate_questions_2015 |>
  filter(debate_number == "2015_Radio-Canada") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(17))) 
debate_questions_2015_RC

# Munk #
debate_questions_2015_Munk =
  debate_questions_2015 |>
  filter(debate_number == "2015_Munk") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(9))) 
debate_questions_2015_Munk

# Macleans #
debate_questions_2015_Mac =
  debate_questions_2015 |>
  filter(debate_number == "2015_Macleans") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(7))) 
debate_questions_2015_Mac

# Globe & Mail #
debate_questions_2015_GM =
  debate_questions_2015 |>
  filter(debate_number == "2015_Globe&Mail") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))) 
debate_questions_2015_GM

# All 2015 debates #
debate_questions_2015_all =
  debate_questions_2015 |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(76))) 
debate_questions_2015_all

## 2019 ##
# LDC EN #
debate_questions_2019_EN =
  debate_questions_2019 |>
  filter(debate_number == "2019_LDC_en") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(18))) 
debate_questions_2019_EN

# LDC FR #
debate_questions_2019_FR =
  debate_questions_2019 |>
  filter(debate_number == "2019_LDC_fr") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))) 
debate_questions_2019_FR 

# TVA #
debate_questions_2019_TVA =
  debate_questions_2019 |>
  filter(debate_number == "2019_TVA") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))) 
debate_questions_2019_TVA 

# Macleans #
debate_questions_2019_Mac =
  debate_questions_2019 |>
  filter(debate_number == "2019_Macleans") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(11))) 
debate_questions_2019_Mac

# All 2019 debates #
debate_questions_2019_all =
  debate_questions_2019 |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(108))) 
debate_questions_2019_all |> print(n=23)

## 2021 ##
# LDC EN #
debate_questions_2021_EN =
  debate_questions_2021 |>
  filter(debate_number == "2021_LDC_en") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(45)))
debate_questions_2021_EN 

# LDC FR #
debate_questions_2021_FR =
  debate_questions_2021 |>
  filter(debate_number == "2021_LDC_fr") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(50)))
debate_questions_2021_FR 

# TVA #
debate_questions_2021_TVA =
  debate_questions_2021 |>
  filter(debate_number == "2021_TVA") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40)))
debate_questions_2021_TVA 

# All 2021 debates #
debate_questions_2021_all =
  debate_questions_2021 |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(135)))
debate_questions_2021_all |> print(n =25)

#### Territorial Issue Stats ####
## 2008 ##
# EN #
debate_qs_2008_EN_territory =
  debate_questions_2008 |>
  filter(debate_number == "2008_Consortium_en") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(8))) 
debate_qs_2008_EN_territory

# FR #
debate_qs_2008_FR_territory =
  debate_questions_2008 |>
  filter(debate_number == "2008_Consortium_Fr") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(14))) 
debate_qs_2008_FR_territory

# All 2008 debates #
debate_qs_2008_all_territory =
  debate_questions_2008 |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(22))) 
debate_qs_2008_all_territory

## 2011 ##
# EN #
debate_qs_2011_EN_territory =
  debate_questions_2011 |>
  filter(debate_number == "2011_Consortium_en") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))) 
debate_qs_2011_EN_territory

# FR #
debate_qs_2011_FR_territory =
  debate_questions_2011 |>
  filter(debate_number == "2011_Consortium_Fr") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))) 
debate_qs_2011_FR_territory

# All 2011 debates #
debate_qs_2011_all_territory =
  debate_questions_2011 |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(16))) 
debate_qs_2011_all_territory

## 2015 ##
# TVA #
debate_qs_2015_TVA_territory =
  debate_questions_2015 |>
  filter(debate_number == "2015_TVA") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(33))) 
debate_qs_2015_TVA_territory

# Radio-Canada #
debate_qs_2015_RC_territory =
  debate_questions_2015 |>
  filter(debate_number == "2015_Radio-Canada") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(17))) 
debate_qs_2015_RC_territory 

# Munk #
debate_qs_2015_Munk_territory =
  debate_questions_2015 |>
  filter(debate_number == "2015_Munk") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(9))) 
debate_qs_2015_Munk_territory

# Macleans #
debate_qs_2015_Mac_territory =
  debate_questions_2015 |>
  filter(debate_number == "2015_Macleans") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(7))) 
debate_qs_2015_Mac_territory

# Globe & Mail #
debate_qs_2015_GM_territory =
  debate_questions_2015 |>
  filter(debate_number == "2015_Globe&Mail") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))) 
debate_qs_2015_GM_territory

# All 2015 debates #
debate_qs_2015_all_territory =
  debate_questions_2015 |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(76))) 
debate_qs_2015_all_territory

## 2019 ##
# LDC EN #
debate_qs_2019_EN_territory =
  debate_questions_2019 |>
  filter(debate_number == "2019_LDC_en") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(18))) 
debate_qs_2019_EN_territory

# LDC FR #
debate_qs_2019_FR_territory =
  debate_questions_2019 |>
  filter(debate_number == "2019_LDC_fr") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))) 
debate_qs_2019_FR_territory

# TVA #
debate_qs_2019_TVA_territory =
  debate_questions_2019 |>
  filter(debate_number == "2019_TVA") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))) 
debate_qs_2019_TVA_territory 

# Macleans #
debate_qs_2019_Mac_territory =
  debate_questions_2019 |>
  filter(debate_number == "2019_Macleans") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(11))) 
debate_qs_2019_Mac_territory

# All 2019 debates #
debate_qs_2019_all_territory =
  debate_questions_2019 |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(108))) 
debate_qs_2019_all_territory |> print(n = 23)

## 2021 ##
# LDC EN #
debate_qs_2021_EN_territory =
  debate_questions_2021 |>
  filter(debate_number == "2021_LDC_en") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(45)))
debate_qs_2021_EN_territory

# LDC FR #
debate_qs_2021_FR_territory =
  debate_questions_2021 |>
  filter(debate_number == "2021_LDC_fr") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(50)))
debate_qs_2021_FR_territory

# TVA #
debate_qs_2021_TVA_territory =
  debate_questions_2021 |>
  filter(debate_number == "2021_TVA") |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40)))
debate_qs_2021_TVA_territory

# All 2021 debates #
debate_qs_2021_all_territory =
  debate_questions_2021 |>
  select(territory1, territory2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(135)))
debate_qs_2021_all_territory 
