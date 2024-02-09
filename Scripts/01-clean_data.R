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

#### Read in sav data ####
## By debates dataset ##
by_debates <- read_sav(here::here("inputs/Data/By_Debates_dataset.sav"))

## Debate questions dataset ##
debate_questions <- read_sav(here::here("inputs/Data/Debate_Questions_dataset.sav"))

## DQI by speech dataset ##
dqi_by_speech <- read_sav(here::here("inputs/Data/DQI_by_speech_act_dataset.sav"))

## Newspaper dataset ##
newspaper_data <- read_sav(here::here("inputs/Data/Newspaper_dataset.sav"))

#### Cleaning ####
## Organize names ##
## By debates dataset ##
by_debates_cleaned <-
  clean_names(by_debates)

## Debate questions dataset ##
debate_questions_cleaned <-
  clean_names(debate_questions)

## DQI by speech dataset ##
dqi_by_speech_cleaned <-
  clean_names(dqi_by_speech)

## Newspaper dataset ##
newspaper_data_cleaned <-
  clean_names(newspaper_data)

#### Save as CSV ####
## By debates dataset ##
write_csv(x = by_debates_cleaned, file = "Outputs/Data/by_debates_cleaned.csv")

## Debate questions dataset ##
write_csv(x = debate_questions_cleaned, file = "Outputs/Data/debate_questions_cleaned.csv")

## DQI by speech dataset ##
write_csv(x = dqi_by_speech_cleaned, file = "Outputs/Data/dqi_by_speech_cleaned.csv")

## Newspaper dataset ##
write_csv(x = newspaper_data_cleaned, file = "Outputs/Data/newspaper_data_cleaned.csv")