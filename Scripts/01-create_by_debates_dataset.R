#### Preamble ####
# Purpose: Replicate how the by_debates dataset was calculated 
# Author: Inessa De Angelis
# Date: 28 June 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in datasets ####
## DQI by speech dataset ##
dqi_by_speech_final <- read_csv(file = "Inputs/Data/dqi_by_speech_final.csv")

## Debate questions dataset ##
debate_questions_cleaned <- read_csv(file = "Outputs/Data/debate_questions_cleaned.csv")

## Newspaper dataset ##
newspaper_data_final <- read_csv(file = "Outputs/Data/newspaper_data_final.csv")

#### Calculate "dqi_percent_demands" column for all debates ####
## Count total number of interventions per debate ##
dqi_by_speech_all =
  dqi_by_speech_final |>
  group_by(Debate_number) |>
  count() |>
  rename(total_number = n)

## Count number of interventions coded as "having demands" per debate ##
dqi_by_speech_all_demands =
  dqi_by_speech_final |>
  filter(Presence_of_demands == 1) |>
  group_by(Debate_number) |>
  count() |>
  rename(number_of_demands = n)

## Combine number of demands and total number of interventions datasets by debate number ##
dqi_by_speech_all_combined <- merge(dqi_by_speech_all_demands, dqi_by_speech_all)

## Divide number of demands by the total number of interventions by debate ##
dqi_by_speech_all_final <- dqi_by_speech_all_combined |> mutate (number_of_demands / total_number) |>
  rename(
    dqi_percent_demands = "number_of_demands/total_number"
  ) |>
  select(Debate_number, dqi_percent_demands)
dqi_by_speech_all_final


