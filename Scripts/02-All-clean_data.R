#### Preamble ####
# Purpose: Saves datasets in csv format and other basic cleaning
# Author: Inessa De Angelis
# Date: 9 February 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(haven)
library(here)
library(janitor)
library(labelled)

#### Read in sav data ####
## By debates dataset ##
by_debates <- read_sav(here::here("Inputs/Data/By_Debates_dataset.sav"))

## DQI by speech dataset ##
dqi_by_speech <- read_sav(here::here("Inputs/Data/DQI_by_speech_act_dataset.sav"))

## Newspaper dataset ##
newspaper_data <- read_sav(here::here("Inputs/Data/Newspaper_dataset.sav"))

## Debate questions dataset - March 13, 2024 version ##
debate_questions <- read_sav(here::here("Inputs/Data/Debate_Questions_dataset.sav")) 

#### Cleaning ####
#### By debates dataset ####
## Organize names ##
by_debates_cleaned <-
  clean_names(by_debates)

## Reunite labels ##
by_debates_cleaned <-
  to_factor(by_debates_cleaned)

#### DQI by speech dataset ####
## Organize names ##
dqi_by_speech_cleaned <-
  clean_names(dqi_by_speech)

## Reunite labels ##
dqi_by_speech_cleaned <-
  to_factor(dqi_by_speech)

#### Newspaper dataset ####
## Organize names ##
newspaper_data_cleaned <-
  clean_names(newspaper_data)

## Reunite labels ##
newspaper_data_cleaned <-
  to_factor(newspaper_data)

#### Debate questions dataset ####
## Organize names ##
debate_questions_cleaned <-
  clean_names(debate_questions)

## Reunite labels ##
debate_questions_cleaned <-
  to_factor(debate_questions_cleaned)

## Re-code primary & secondary issues columns ##
debate_questions_cleaned2 =
  debate_questions_cleaned |>
  mutate("primary_issue" = case_when(
    primary_issue == "Indigenous" ~ "Indigenous",
    primary_issue == "Women" ~ "Women",
    primary_issue == "Heritage" ~ "Culture and heritage",
    primary_issue == "Economy" ~ "Economy",
    primary_issue == "Environment" ~ "Environment",
    primary_issue == "Justice" ~ "Justice",
    primary_issue == "Defence" ~ "Defence",
    primary_issue == "Healthcare" ~ "Health care",
    primary_issue == "Other" ~ "Other",
    primary_issue == "Immigration" ~ "Immigration",
    primary_issue == "Constitution" ~ "Constitution",
    primary_issue == "Energy" ~ "Energy",
    primary_issue == "Ethics" ~ "Ethics",
    primary_issue == "Transport" ~ "Transport",
    primary_issue == "Welfare" ~ "Social welfare",
    primary_issue == "Agriculture" ~ "Agriculture",
    primary_issue == "PublicSafety" ~ "Crime and public safety",
    primary_issue == "Foreign_affairs" ~ "Foreign affairs",
    primary_issue == "PublicFinance" ~ "Public finance",
    primary_issue == "GovFormation" ~ "Government formation",
    primary_issue == "OffiicialLang" ~ "Official Languages",
    primary_issue == 22 ~ "Housing",
    primary_issue == 23 ~ "Covid",
    primary_issue == 24 ~ "Federalism"
)) 
debate_questions_cleaned2

debate_questions_cleaned2 =
  debate_questions_cleaned2 |>
  mutate("secondary_issue" = case_when(
    secondary_issue == "Indigenous" ~ "Indigenous",
    secondary_issue == "Women" ~ "Women",
    secondary_issue == "Heritage" ~ "Culture and heritage",
    secondary_issue == "Economy" ~ "Economy",
    secondary_issue == "Environment" ~ "Environment",
    secondary_issue == "Justice" ~ "Justice",
    secondary_issue == "Defence" ~ "Defence",
    secondary_issue == "Healthcare" ~ "Health care",
    secondary_issue == "Other" ~ "Other",
    secondary_issue == "Immigration" ~ "Immigration",
    secondary_issue == "Constitution" ~ "Constitution",
    secondary_issue == "Energy" ~ "Energy",
    secondary_issue == "Ethics" ~ "Ethics",
    secondary_issue == "Transport" ~ "Transport",
    secondary_issue == "Welfare" ~ "Social welfare",
    secondary_issue == "Agriculture" ~ "Agriculture",
    secondary_issue == "PublicSafety" ~ "Crime and public safety",
    secondary_issue == "Foreign_affairs" ~ "Foreign affairs",
    secondary_issue == "PublicFinance" ~ "Public finance",
    secondary_issue == "GovFormation" ~ "Government formation",
    secondary_issue == "OffiicialLang" ~ "Official Languages",
    secondary_issue == 22 ~ "Housing",
    secondary_issue == 23 ~ "Covid",
    secondary_issue == 24 ~ "Federalism"
  ))
debate_questions_cleaned2

#### Save as CSV ####
## By debates dataset ##
write_csv(x = by_debates_cleaned, file = "Outputs/Data/by_debates_cleaned.csv")

## DQI by speech dataset ##
write_csv(x = dqi_by_speech_cleaned, file = "Outputs/Data/dqi_by_speech_cleaned.csv")

## Newspaper dataset ##
write_csv(x = newspaper_data_cleaned, file = "Outputs/Data/newspaper_data_cleaned.csv")

## Debate questions dataset ##
write_csv(x = debate_questions_cleaned2, file = "Outputs/Data/debate_questions_cleaned.csv")
