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

#### Issue stats ####
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
# EN #
debate_questions_2015_EN =
  debate_questions_2015 |>
  filter(language_of_question == "English") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(26))) 
debate_questions_2015_EN

# FR #
debate_questions_2015_FR =
  debate_questions_2015 |>
  filter(language_of_question == "French") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(50))) 
debate_questions_2015_FR

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
# EN #
debate_questions_2019_EN =
  debate_questions_2019 |>
  filter(language_of_question == "English") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(29))) 
debate_questions_2019_EN

# FR #
debate_questions_2019_FR =
  debate_questions_2019 |>
  filter(language_of_question == "French") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(79))) 
debate_questions_2019_FR

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
debate_questions_2019_all

## 2021 ##
# EN #
debate_questions_2021_EN =
  debate_questions_2021 |>
  filter(language_of_question == "English") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(45)))
debate_questions_2021_EN 

# FR #
debate_questions_2021_FR =
  debate_questions_2021 |>
  filter(language_of_question == "French") |>
  select(primary_issue, secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(90)))
debate_questions_2021_FR

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
debate_questions_2021_all