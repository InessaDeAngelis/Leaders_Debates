#### Preamble ####
# Purpose: Chapter 3 Followup Questions Contingency table
# Author: Inessa De Angelis
# Date: 30 July 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in dataset ####
debate_questions_final <- read_csv("Outputs/Data/debate_questions_final.csv")

#### Create specific datasets by year ####
## 2008 debate ##
debate_questions_2008 = 
  debate_questions_final |>
  filter(Year == "2008") |>
  filter(Lead_followup == "Followup") |>
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
  filter(Lead_followup == "Followup") |>
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
  filter(Lead_followup == "Followup") |>
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
  filter(Lead_followup == "Followup") |>
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
  filter(Lead_followup == "Followup") |>
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
  mutate(proportion = n / (sum(23))* 100)
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
  mutate(proportion = n / (sum(8))* 100) 
debate_questions_2008_FR

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
  mutate(proportion = n / (sum(1))* 100) 
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
  mutate(proportion = n / (sum(9))* 100)
debate_questions_2011_FR 

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
  mutate(proportion = n / (sum(7))* 100) 
debate_questions_2015_TVA

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
  mutate(proportion = n / (sum(2))* 100)
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
  mutate(proportion = n / (sum(33))* 100) 
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
  mutate(proportion = n / (sum(24))* 100) 
debate_questions_2015_GM

## 2019 ##
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
  mutate(proportion = n / (sum(12))* 100) 
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
  mutate(proportion = n / (sum(6))* 100) 
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
  mutate(proportion = n / (sum(21))* 100) 
debate_questions_2019_Mac

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
  mutate(proportion = n / (sum(15))* 100)
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
  mutate(proportion = n / (sum(22))* 100)
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
  mutate(proportion = n / (sum(47))* 100)
debate_questions_2021_TVA 
