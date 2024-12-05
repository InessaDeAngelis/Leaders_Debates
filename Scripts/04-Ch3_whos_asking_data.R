#### Preamble ####
# Purpose: Citizens vs moderators vs journalists & issues for Ch3
# Author: Inessa De Angelis
# Date: 1 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in cleaned dataset ####
debate_questions_final <- read_csv("Outputs/Data/debate_questions_final_recoded.csv")

#### Create specific datasets by year ####
## 2008 debate ##
debate_questions_2008 = 
  debate_questions_final |>
  filter(Year == "2008") |>
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
# Which group asks questions which better reflect issues of public interest? From pg. 89 #

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
  mutate(proportion = n / (sum(8))* 100) |>
  mutate(Debate_number = c("2008EnConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2008_EN

# EN - Moderator Qs #
moderator_questions_2008_EN =
  debate_questions_2008 |>
  filter(Debate_number == "2008EnConsortium") |>
  filter(Questioner_id== "Paikin") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(23))* 100) |>
  mutate(Debate_number = c("2008EnConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2008_EN

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
  mutate(proportion = n / (sum(14))* 100) |>
  mutate(Debate_number = c("2008FrConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2008_FR

# FR - Citizen Qs #
citizen_questions_2008_FR =
  debate_questions_2008 |>
  filter(Debate_number == "2008FrConsortium") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(8))* 100) |>
  mutate(Debate_number = c("2008FrConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2008_FR

## 2011 ##
# EN - Citizen Qs #
citizen_questions_2011_EN =
  debate_questions_2011 |>
  filter(Debate_number == "2011EnConsortium") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) |>
  mutate(Debate_number = c("2011EnConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2011_EN

# EN - Moderator Qs #
moderator_questions_2011_EN =
  debate_questions_2011 |>
  filter(Debate_number == "2011EnConsortium") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(1))* 100) |>
  mutate(Debate_number = c("2011EnConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2011_EN

# FR - Citizen Qs #
citizen_questions_2011_FR =
  debate_questions_2011 |>
  filter(Debate_number == "2011FrConsortium") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) |>
  mutate(Debate_number = c("2011FrConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2011_FR 

# FR - Moderator Qs #
moderator_questions_2011_FR =
  debate_questions_2011 |>
  filter(Debate_number == "2011FrConsortium") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(13))* 100) |>
  mutate(Debate_number = c("2011FrConsortium"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2011_FR 

## 2015 ##
# TVA - Moderator Qs #
moderator_questions_2015_TVA =
  debate_questions_2015 |>
  filter(Debate_number == "2015TVA") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))* 100) |>
  mutate(Debate_number = c("2015TVA"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2015_TVA

# Radio-Canada - Journalist Qs #
journalist_questions_2015_RC =
  debate_questions_2015 |>
  filter(Debate_number == "2015Radio-Canada") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(17))* 100) |>
  mutate(Debate_number = c("2015Radio-Canada"),
         .before = value) |>
  mutate(Whos_asking = c("Journalist"),
         .after = Debate_number)
journalist_questions_2015_RC

# Munk - Moderator Qs #
moderator_questions_2015_Munk =
  debate_questions_2015 |>
  filter(Debate_number == "2015Munk") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(11))* 100) |>
  mutate(Debate_number = c("2015Munk"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2015_Munk

# Macleans - Moderator Qs  #
moderator_questions_2015_Mac =
  debate_questions_2015 |>
  filter(Debate_number == "2015Macleans") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(40))* 100) |>
  mutate(Debate_number = c("2015Macleans"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2015_Mac

# Globe & Mail - Moderator Qs #
moderator_questions_2015_GM =
  debate_questions_2015 |>
  filter(Debate_number == "2015Globe&Mail") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(34))* 100) |>
  mutate(Debate_number = c("2015Globe&Mail"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2015_GM

## 2019 ##
# LDC EN - Citizen Qs #
citizen_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019EnLDC") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) |>
  mutate(Debate_number = c("2019EnLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2019_EN

# LDC EN - Moderator Qs #
moderator_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019EnLDC") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(4))* 100) |>
  mutate(Debate_number = c("2019EnLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2019_EN

# LDC EN - Journalist Qs #
journalist_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019EnLDC") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(2))* 100) |>
  mutate(Debate_number = c("2019EnLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Journalist"),
         .after = Debate_number)
journalist_questions_2019_EN

# LDC EN - Leader Qs #
leader_questions_2019_EN =
  debate_questions_2019 |>
  filter(Debate_number == "2019EnLDC") |>
  filter(Question_source == "Leader") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(6))* 100) |>
  mutate(Debate_number = c("2019EnLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Leader"),
         .after = Debate_number)
leader_questions_2019_EN

# LDC FR - Citizen Qs #
citizen_questions_2019_FR =
  debate_questions_2019 |>
  filter(Debate_number == "2019FrLDC") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(5))* 100) |>
  mutate(Debate_number = c("2019FrLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2019_FR 

# LDC FR - Moderator Qs #
moderator_questions_2019_FR =
  debate_questions_2019 |>
  filter(Debate_number == "2019FrLDC") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(20))* 100) |>
  mutate(Debate_number = c("2019FrLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2019_FR 

# LDC FR - Journalist Qs #
journalist_questions_2019_FR =
  debate_questions_2019 |>
  filter(Debate_number == "2019FrLDC") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(27))* 100) |>
  mutate(Debate_number = c("2019FrLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Journalist"),
         .after = Debate_number)
journalist_questions_2019_FR 

# TVA - Moderator Qs #
moderator_questions_2019_TVA =
  debate_questions_2019 |>
  filter(Debate_number == "2019TVA") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(45))* 100) |>
  mutate(Debate_number = c("2019TVA"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2019_TVA 

# Macleans - Moderator Qs #
moderator_questions_2019_Mac =
  debate_questions_2019 |>
  filter(Debate_number == "2019Macleans") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(32))* 100) |>
  mutate(Debate_number = c("2019Macleans"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2019_Mac

## 2021 ##
# LDC EN - Citizen Qs #
citizen_questions_2021_EN =
  debate_questions_2021 |>
  filter(Debate_number == "2021EnLDC") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(4))* 100) |>
  mutate(Debate_number = c("2021EnLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2021_EN 

# LDC EN - Moderator Qs #
moderator_questions_2021_EN =
  debate_questions_2021 |>
  filter(Debate_number == "2021EnLDC") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(24))* 100) |>
  mutate(Debate_number = c("2021EnLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2021_EN 

# LDC EN - Journalist Qs #
journalist_questions_2021_EN =
  debate_questions_2021 |>
  filter(Debate_number == "2021EnLDC") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(32))* 100) |>
  mutate(Debate_number = c("2021EnLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Journalist"),
         .after = Debate_number)
journalist_questions_2021_EN 

# LDC FR - Citizen Qs #
citizen_questions_2021_FR =
  debate_questions_2021 |>
  filter(Debate_number == "2021FrLDC") |>
  filter(Question_source == "Citizen") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(5))* 100) |>
  mutate(Debate_number = c("2021FrLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Citizen"),
         .after = Debate_number)
citizen_questions_2021_FR 

# LDC FR - Moderator Qs #
moderator_questions_2021_FR =
  debate_questions_2021 |>
  filter(Debate_number == "2021FrLDC") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(30))* 100) |>
  mutate(Debate_number = c("2021FrLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2021_FR 

# LDC FR - Journalist Qs #
journalist_questions_2021_FR =
  debate_questions_2021 |>
  filter(Debate_number == "2021FrLDC") |>
  filter(Question_source == "Journalist") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(37))* 100) |>
  mutate(Debate_number = c("2021FrLDC"),
         .before = value) |>
  mutate(Whos_asking = c("Journalist"),
         .after = Debate_number)
journalist_questions_2021_FR 

# TVA - Moderator Qs #
moderator_questions_2021_TVA =
  debate_questions_2021 |>
  filter(Debate_number == "2021TVA") |>
  filter(Question_source == "Moderator") |>
  select(Primary_issue, Secondary_issue) |>
  mutate(across(everything(), trimws)) |>
  rowid_to_column() |>
  pivot_longer(-rowid) |>
  group_by(value) |>
  summarise(n = n_distinct(rowid)) |>
  mutate(proportion = n / (sum(87))* 100) |>
  mutate(Debate_number = c("2021TVA"),
         .before = value) |>
  mutate(Whos_asking = c("Moderator"),
         .after = Debate_number)
moderator_questions_2021_TVA 

#### Combine ####
## Combine all individual datasets ##
all_qs <-
  rbind(
    leader_questions_2019_EN,
    journalist_questions_2015_RC,
    journalist_questions_2019_EN,
    journalist_questions_2019_FR,
    journalist_questions_2021_EN,
    journalist_questions_2021_FR,
    citizen_questions_2021_EN,
    citizen_questions_2021_FR,
    citizen_questions_2019_EN,
    citizen_questions_2019_FR,
    citizen_questions_2011_EN,
    citizen_questions_2011_FR,
    citizen_questions_2008_EN,
    citizen_questions_2008_FR,
    moderator_questions_2008_EN,
    moderator_questions_2008_FR,
    moderator_questions_2011_EN,
    moderator_questions_2011_FR,
    moderator_questions_2015_GM,
    moderator_questions_2015_Mac,
    moderator_questions_2015_Munk,
    moderator_questions_2015_TVA,
    moderator_questions_2019_EN,
    moderator_questions_2019_FR,
    moderator_questions_2019_Mac,
    moderator_questions_2019_TVA,
    moderator_questions_2021_TVA,
    moderator_questions_2021_FR,
    moderator_questions_2021_EN
  )

## Clean up column names, rounding ##
all_qs <- all_qs |>
  drop_na(value) |>
  rename(Issue = value,
         Percentage = proportion,
         Number_of_Qs = n) |>
  dplyr::mutate(across(where(is.numeric), round, 2))

## Save dataset ##
write_csv(all_qs, "all_qs.csv")
