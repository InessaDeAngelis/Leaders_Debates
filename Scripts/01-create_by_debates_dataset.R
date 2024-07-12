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
dqi_by_speech_all_final <- 
  dqi_by_speech_all_combined |> 
  mutate (dqi_percent_demands = number_of_demands / total_number) |>
  select(Debate_number, dqi_percent_demands)
dqi_by_speech_all_final

#### Calculate "dqi_positional_politics" column for all debates ####
## Count total number of positions per debate ##
dqi_by_speech_positional =
  dqi_by_speech_final |>
  drop_na(Positional) |>
  group_by(Debate_number) |>
  count() |>
  rename(number_of_positions = n)
dqi_by_speech_positional

## Count number of interventions coded as "Positional_Politics" per debate ##
dqi_by_speech_positional_pol =
  dqi_by_speech_final |>
  filter(Positional == "Positional_Politics") |>
  group_by(Debate_number) |>
  count() |>
  rename(positional_pol_total = n)
dqi_by_speech_positional_pol

## Combine number of positions and "Positional_Politics" datasets ##
dqi_positional_pol <- merge(dqi_by_speech_positional_pol, dqi_by_speech_positional)

## Divide number of interventions coded as "Positional_Politics" by the total number of positions by debate ##
dqi_positional_pol_final <-
  dqi_positional_pol |>
  mutate(dqi_positional_politics = positional_pol_total / (number_of_positions) * 100) |>
  select(Debate_number, dqi_positional_politics)
dqi_positional_pol_final

#### Calculate "dqi_respect_demands" column for all debates ####
## Count total number of respect options per debate ##
dqi_by_speech_respect_demands =
  dqi_by_speech_final |>
  drop_na(Respect_for_demands) |>
  group_by(Debate_number) |>
  count() |>
  rename(respect_for_demands_total = n)
dqi_by_speech_respect_demands

## Count total number of interventions coded as "Respect" per debate ##
dqi_by_speech_respect =
  dqi_by_speech_final |>
  filter(Respect_for_demands == "Respect") |>
  group_by(Debate_number) |>
  count() |>
  rename(respect_total = n)
dqi_by_speech_respect

## Combine number of respect options and "Respect" datasets ##
dqi_by_all_respect <- merge(dqi_by_speech_respect, dqi_by_speech_respect_demands)

## Divide number of interventions coded as "Respect" by the total number of respect options by debate ##
dqi_respect_final <-
  dqi_by_all_respect |>
  mutate(dqi_respect_demands = respect_total / (respect_for_demands_total) * 100) |>
  select(Debate_number, dqi_respect_demands)
dqi_respect_final

#### Calculate "dqi_normal_participation" column for all debates ####
## Count total number of participation options per debate ##
dqi_by_speech_interruptions =
  dqi_by_speech_final |>
  drop_na(Interruption) |>
  group_by(Debate_number) |>
  count() |>
  rename(total_interruptions = n)
dqi_by_speech_interruptions

## Count total number of interventions coded as "Normal_Participation" per debate ##
dqi_by_speech_normal_participation =
  dqi_by_speech_final |>
  filter(Interruption == "Normal_Participation") |>
  group_by(Debate_number) |>
  count() |>
  rename(normal_participation_total = n)
dqi_by_speech_normal_participation

## Combine "Normal_Participation" and total number of participation options datasets ##
dqi_by_all_participation <- merge(dqi_by_speech_normal_participation, dqi_by_speech_interruptions)

## Divide number of interventions coded as "Normal_Participation" by the total number of participation options by debate ##
dqi_participation_final <-
  dqi_by_all_participation |>
  mutate(dqi_normal_participation = normal_participation_total / (total_interruptions) * 100) |>
  select(Debate_number, dqi_normal_participation)
dqi_participation_final