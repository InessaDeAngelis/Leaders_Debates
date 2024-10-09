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
debate_questions_final <- read_csv("Outputs/Data/debate_questions_final_recoded.csv")

#### Create specific datasets by year ####
## 2008 debate ##
debate_questions_2008 = 
  debate_questions_final |>
  filter(Year == "2008") |>
  filter(!Lead_followup == "Followup") |>
  select(ID,
         Year,
         Debate_number,
         Language_of_question,
         Questioner_id,
         Lead_followup,
         Primary_issue,
         Secondary_issue,
         Territory_1,
         Territory_2,
         Debate_lang) 
debate_questions_2008

## 2011 debate ##
debate_questions_2011 = 
  debate_questions_final |>
  filter(Year == "2011") |>
  filter(!Lead_followup == "Followup") |>
  select(ID,
         Year,
         Debate_number,
         Language_of_question,
         Questioner_id,
         Lead_followup,
         Primary_issue,
         Secondary_issue,
         Territory_1,
         Territory_2,
         Debate_lang) 
debate_questions_2011

## 2015 debate ##
debate_questions_2015 = 
  debate_questions_final |>
  filter(Year == "2015") |>
  filter(!Lead_followup == "Followup") |>
  select(ID,
         Year,
         Debate_number,
         Language_of_question,
         Questioner_id,
         Lead_followup,
         Primary_issue,
         Secondary_issue,
         Territory_1,
         Territory_2,
         Debate_lang) 
debate_questions_2015

## 2019 debate ##
debate_questions_2019 = 
  debate_questions_final |>
  filter(Year == "2019") |>
  filter(!Lead_followup == "Followup") |>
  select(ID,
         Year,
         Debate_number,
         Language_of_question,
         Questioner_id,
         Lead_followup,
         Primary_issue,
         Secondary_issue,
         Territory_1,
         Territory_2,
         Debate_lang)
debate_questions_2019

## 2021 debate ##
debate_questions_2021 = 
  debate_questions_final |>
  filter(Year == "2021") |>
  filter(!Lead_followup == "Followup") |>
  select(ID,
         Year,
         Debate_number,
         Language_of_question,
         Questioner_id,
         Lead_followup,
         Primary_issue,
         Secondary_issue,
         Territory_1,
         Territory_2,
         Debate_lang) 
debate_questions_2021

#### Primary & Secondary Issue Stats ####
# Code referenced from: https://stackoverflow.com/questions/76064350/count-number-of-rows-where-a-value-appears-in-any-of-two-columns-in-r
## 2008 ##
# EN #
debate_questions_2008_EN =
  debate_questions_2008 |>
  filter(Debate_number == "2008EnConsortium") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(8))* 100)
debate_questions_2008_EN

# FR #
debate_questions_2008_FR =
  debate_questions_2008 |>
  filter(Debate_number == "2008FrConsortium") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(14))* 100) 
debate_questions_2008_FR

# All 2008 debates ##
debate_questions_2008_all =
  debate_questions_2008 |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(22))* 100) 
debate_questions_2008_all

## 2011 ##
# EN #
debate_questions_2011_EN =
  debate_questions_2011 |>
  filter(Debate_number == "2011EnConsortium") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) 
debate_questions_2011_EN 

# FR #
debate_questions_2011_FR =
  debate_questions_2011 |>
  filter(Debate_number == "2011FrConsortium") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))* 100)
debate_questions_2011_FR 

# All 2011 debates #
debate_questions_2011_all =
  debate_questions_2011 |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(16))* 100)
debate_questions_2011_all 

## 2015 ##
# TVA #
debate_questions_2015_TVA =
  debate_questions_2015 |>
  filter(Debate_number == "2015TVA") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(33))* 100) 
debate_questions_2015_TVA

# Radio-Canada #
debate_questions_2015_RC =
  debate_questions_2015 |>
  filter(Debate_number == "2015Radio-Canada") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(17))* 100) 
debate_questions_2015_RC

# Munk #
debate_questions_2015_Munk =
  debate_questions_2015 |>
  filter(Debate_number == "2015Munk") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(9))* 100)
debate_questions_2015_Munk

# Macleans #
debate_questions_2015_Mac =
  debate_questions_2015 |>
  filter(Debate_number == "2015Macleans") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(7))* 100) 
debate_questions_2015_Mac

# Globe & Mail #
debate_questions_2015_GM =
  debate_questions_2015 |>
  filter(Debate_number == "2015Globe&Mail") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))* 100) 
debate_questions_2015_GM

# All 2015 debates #
debate_questions_2015_all =
  debate_questions_2015 |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(76))* 100)
debate_questions_2015_all

## 2019 ##
# LDC EN #
debate_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019EnLDC") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(18))* 100) 
debate_questions_2019_EN

# LDC FR #
debate_questions_2019_FR =
  debate_questions_2019 |>
  filter(Debate_number == "2019FrLDC") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))* 100) 
debate_questions_2019_FR 

# TVA #
debate_questions_2019_TVA =
  debate_questions_2019 |>
  filter(Debate_number == "2019TVA") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(39))* 100) 
debate_questions_2019_TVA 

# Macleans #
debate_questions_2019_Mac =
  debate_questions_2019 |>
  filter(Debate_number == "2019Macleans") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(11))* 100) 
debate_questions_2019_Mac

# All 2019 debates #
debate_questions_2019_all =
  debate_questions_2019 |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(108))* 100) 
debate_questions_2019_all |> print(n=23)

## 2021 ##
# LDC EN #
debate_questions_2021_EN =
  debate_questions_2021 |>
  filter(Debate_number == "2021EnLDC") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(45))* 100)
debate_questions_2021_EN 

# LDC FR #
debate_questions_2021_FR =
  debate_questions_2021 |>
  filter(Debate_number == "2021FrLDC") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(50))* 100)
debate_questions_2021_FR 

# TVA #
debate_questions_2021_TVA =
  debate_questions_2021 |>
  filter(Debate_number == "2021TVA") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))* 100)
debate_questions_2021_TVA 

# All 2021 debates #
debate_questions_2021_all =
  debate_questions_2021 |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(135))* 100)
debate_questions_2021_all |> print(n =25)

#### Territorial Issue Stats ####
## 2008 ##
# EN #
debate_qs_2008_EN_territory =
  debate_questions_2008 |>
  filter(Debate_number == "2008EnConsortium") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(8))* 100) 
debate_qs_2008_EN_territory

# FR #
debate_qs_2008_FR_territory =
  debate_questions_2008 |>
  filter(Debate_number == "2008FrConsortium") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(14))* 100) 
debate_qs_2008_FR_territory

# All 2008 debates #
debate_qs_2008_all_territory =
  debate_questions_2008 |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(22))* 100) 
debate_qs_2008_all_territory

## 2011 ##
# EN #
debate_qs_2011_EN_territory =
  debate_questions_2011 |>
  filter(Debate_number == "2011EnConsortium") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) 
debate_qs_2011_EN_territory

# FR #
debate_qs_2011_FR_territory =
  debate_questions_2011 |>
  filter(Debate_number == "2011FrConsortium") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))* 100) 
debate_qs_2011_FR_territory

# All 2011 debates #
debate_qs_2011_all_territory =
  debate_questions_2011 |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(16))* 100) 
debate_qs_2011_all_territory

## 2015 ##
# TVA #
debate_qs_2015_TVA_territory =
  debate_questions_2015 |>
  filter(Debate_number == "2015TVA") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(33))* 100) 
debate_qs_2015_TVA_territory

# Radio-Canada #
debate_qs_2015_RC_territory =
  debate_questions_2015 |>
  filter(Debate_number == "2015_Radio-Canada") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(17))* 100) 
debate_qs_2015_RC_territory 

# Munk #
debate_qs_2015_Munk_territory =
  debate_questions_2015 |>
  filter(Debate_number == "2015Munk") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(9))* 100) 
debate_qs_2015_Munk_territory

# Macleans #
debate_qs_2015_Mac_territory =
  debate_questions_2015 |>
  filter(Debate_number == "2015Macleans") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(7))* 100) 
debate_qs_2015_Mac_territory

# Globe & Mail #
debate_qs_2015_GM_territory =
  debate_questions_2015 |>
  filter(Debate_number == "2015_Globe&Mail") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))* 100) 
debate_qs_2015_GM_territory

# All 2015 debates #
debate_qs_2015_all_territory =
  debate_questions_2015 |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(76))* 100) 
debate_qs_2015_all_territory

## 2019 ##
# LDC EN #
debate_qs_2019_EN_territory =
  debate_questions_2019 |>
  filter(Debate_number == "2019EnLDC") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(18))* 100) 
debate_qs_2019_EN_territory

# LDC FR #
debate_qs_2019_FR_territory =
  debate_questions_2019 |>
  filter(Debate_number == "2019FrLDC") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))* 100)
debate_qs_2019_FR_territory

# TVA #
debate_qs_2019_TVA_territory =
  debate_questions_2019 |>
  filter(Debate_number == "2019TVA") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(39))* 100)
debate_qs_2019_TVA_territory 

# Macleans #
debate_qs_2019_Mac_territory =
  debate_questions_2019 |>
  filter(Debate_number == "2019Macleans") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(11))* 100) 
debate_qs_2019_Mac_territory

# All 2019 debates #
debate_qs_2019_all_territory =
  debate_questions_2019 |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(108))* 100) 
debate_qs_2019_all_territory |> print(n = 23)

## 2021 ##
# LDC EN #
debate_qs_2021_EN_territory =
  debate_questions_2021 |>
  filter(Debate_number == "2019EnLDC") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(45))* 100)
debate_qs_2021_EN_territory

# LDC FR #
debate_qs_2021_FR_territory =
  debate_questions_2021 |>
  filter(Debate_number == "2021FrLDC") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(50))* 100)
debate_qs_2021_FR_territory

# TVA #
debate_qs_2021_TVA_territory =
  debate_questions_2021 |>
  filter(Debate_number == "2021TVA") |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))* 100)
debate_qs_2021_TVA_territory

# All 2021 debates #
debate_qs_2021_all_territory =
  debate_questions_2021 |>
  select(Territory_1, Territory_2) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(135))* 100)
debate_qs_2021_all_territory 
