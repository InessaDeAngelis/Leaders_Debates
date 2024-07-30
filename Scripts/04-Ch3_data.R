#### Preamble ####
# Purpose: Citizens vs moderators vs journalists & issues for Ch3
# Author: Inessa De Angelis
# Date: 2 May 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in cleaned dataset ####
debate_questions_final <- read_csv("Outputs/Data/debate_questions_final.csv")
print(debate_questions_final)

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
         Question_source,
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
         Question_source,
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
         Question_source,
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
         Question_source,
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
         Question_source,
         Questioner_id,
         Lead_followup,
         Primary_issue,
         Secondary_issue,
         Territory_1,
         Territory_2,
         Debate_lang) 
debate_questions_2021

#### Citizens vs journalists vs moderators & issues #####
# Which group asks questions which better reflect issues of public interest? From pg. 73 #

## 2008 ##
# EN - Citizen Qs #
citizen_questions_2008_EN =
  debate_questions_2008 |>
  filter(Debate_number == "2008EnConsortium") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(8))* 100) 
citizen_questions_2008_EN

# FR - Moderator Qs #
moderator_questions_2008_FR =
  debate_questions_2008 |>
  filter(Debate_number == "2008FrConsortium") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100)
moderator_questions_2008_FR

# FR - Citizen Qs #
citizen_questions_2008_FR =
  debate_questions_2008 |>
  filter(Debate_number == "2008_Consortium_Fr") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(8))* 100)
citizen_questions_2008_FR

## 2011 ##
# EN - Citizen Qs #
citizen_questions_2011_EN =
  debate_questions_2011 |>
  filter(Debate_number == "2011_Consortium_en") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) 
citizen_questions_2011_EN

# FR - Citizen Qs #
citizen_questions_2011_FR =
  debate_questions_2011 |>
  filter(Debate_number == "2011_Consortium_Fr") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100)
citizen_questions_2011_FR 

# FR - Moderator Qs #
moderator_questions_2011_FR =
  debate_questions_2011 |>
  filter(Debate_number == "2011_Consortium_Fr") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(4))* 100) 
moderator_questions_2011_FR 

## 2015 ##
# TVA - Moderator Qs #
moderator_questions_2015_TVA =
  debate_questions_2015 |>
  filter(Debate_number == "2015_TVA") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(33))* 100) 
moderator_questions_2015_TVA

# Radio-Canada - Journalist Qs #
journalist_questions_2015_RC =
  debate_questions_2015 |>
  filter(Debate_number == "2015_Radio-Canada") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(17))* 100) 
journalist_questions_2015_RC

# Munk - Moderator Qs #
moderator_questions_2015_Munk =
  debate_questions_2015 |>
  filter(Debate_number == "2015_Munk") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(9))* 100)
moderator_questions_2015_Munk

# Macleans - Moderator Qs  #
moderator_questions_2015_Mac =
  debate_questions_2015 |>
  filter(Debate_number == "2015_Macleans") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(7))* 100) 
moderator_questions_2015_Mac

# Globe & Mail - Moderator Qs #
moderator_questions_2015_GM =
  debate_questions_2015 |>
  filter(Debate_number == "2015_Globe&Mail") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(10))* 100) 
moderator_questions_2015_GM

## 2019 ##
# LDC EN - Citizen Qs #
citizen_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019_LDC_en") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) 
citizen_questions_2019_EN

# LDC EN - Moderator Qs #
moderator_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019_LDC_en") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(4))* 100) 
moderator_questions_2019_EN

# LDC EN - Journalist Qs #
journalist_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019_LDC_en") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(2))* 100) 
journalist_questions_2019_EN

# LDC FR - Citizen Qs #
citizen_questions_2019_FR =
  debate_questions_2019 |>
  filter(Debate_number == "2019_LDC_fr") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(5))* 100) 
citizen_questions_2019_FR 

# LDC FR - Moderator Qs #
moderator_questions_2019_FR =
  debate_questions_2019 |>
  filter(Debate_number == "2019_LDC_fr") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(15))* 100) 
moderator_questions_2019_FR 

# LDC FR - Journalist Qs #
journalist_questions_2019_FR =
  debate_questions_2019 |>
  filter(Debate_number == "2019_LDC_fr") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(20))* 100) 
journalist_questions_2019_FR 

# TVA - Moderator Qs #
moderator_questions_2019_TVA =
  debate_questions_2019 |>
  filter(Debate_number == "2019_TVA") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(39))* 100) 
moderator_questions_2019_TVA 

# Macleans - Moderator Qs #
moderator_questions_2019_Mac =
  debate_questions_2019 |>
  filter(Debate_number == "2019_Macleans") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(11))* 100) 
moderator_questions_2019_Mac

## 2021 ##
# LDC EN - Citizen Qs #
citizen_questions_2021_EN =
  debate_questions_2021 |>
  filter(Debate_number == "2021_LDC_en") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(4))* 100)
citizen_questions_2021_EN 

# LDC EN - Moderator Qs #
moderator_questions_2021_EN =
  debate_questions_2021 |>
  filter(Debate_number == "2021_LDC_en") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(17))* 100)
moderator_questions_2021_EN 

# LDC EN - Journalist Qs #
journalist_questions_2021_EN =
  debate_questions_2021 |>
  filter(Debate_number == "2021_LDC_en") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(24))* 100)
journalist_questions_2021_EN 

# LDC FR - Citizen Qs #
citizen_questions_2021_FR =
  debate_questions_2021 |>
  filter(Debate_number == "2021_LDC_fr") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(5))* 100)
citizen_questions_2021_FR 

# LDC FR - Moderator Qs #
moderator_questions_2021_FR =
  debate_questions_2021 |>
  filter(Debate_number == "2021_LDC_fr") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(25))* 100)
moderator_questions_2021_FR 

# LDC FR - Journalist Qs #
journalist_questions_2021_FR =
  debate_questions_2021 |>
  filter(Debate_number == "2021_LDC_fr") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(20))* 100)
journalist_questions_2021_FR 

# TVA - Moderator Qs #
moderator_questions_2021_TVA =
  debate_questions_2021 |>
  filter(Debate_number == "2021_TVA") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))* 100)
moderator_questions_2021_TVA 
