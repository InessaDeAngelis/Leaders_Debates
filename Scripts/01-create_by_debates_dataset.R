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
dqi_by_speech_final <- read_csv(file = "Outputs/Data/dqi_by_speech_final.csv")

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
  rename(debate_number = Debate_number) |>
  select(debate_number, dqi_percent_demands)
dqi_by_speech_all_final

## Round all numbers ##
# Code from: https://stackoverflow.com/questions/68626912/r-how-to-round-values-of-a-data-frame #
dqi_by_speech_all_final <-
dqi_by_speech_all_final |>
  dplyr::mutate(across(where(is.numeric), round, 2))

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
  rename(debate_number = Debate_number) |>
  select(debate_number, dqi_positional_politics)
dqi_positional_pol_final

## Round all numbers ##
dqi_positional_pol_final <-
dqi_positional_pol_final |>
  dplyr::mutate(across(where(is.numeric), round, 1))

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
  rename(debate_number = Debate_number) |>
  select(debate_number, dqi_respect_demands)
dqi_respect_final

## Round all numbers ##
dqi_respect_final <-
dqi_respect_final |>
  dplyr::mutate(across(where(is.numeric), round, 1))

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
  rename(debate_number = Debate_number) |>
  select(debate_number, dqi_normal_participation)
dqi_participation_final

## Round all numbers ##
dqi_participation_final <-
dqi_participation_final |>
  dplyr::mutate(across(where(is.numeric), round, 1))

#### Calculate "news_substance" column for all debates ####
## 2008 - FR ##
newspaper_substance_2008_FR =
  newspaper_data_final |>
  filter(Fr08 == 1)|>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2008FrConsortium") |>
  select(debate_number, news_substance)
newspaper_substance_2008_FR

## 2008 - EN ##
newspaper_substance_2008_EN =
  newspaper_data_final |>
  filter(En08 == 1)|>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2008EnConsortium") |>
  select(debate_number, news_substance)
newspaper_substance_2008_EN

## 2011 - EN ##
newspaper_substance_2011_EN =
  newspaper_data_final |>
  filter(En11 == 1)|>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2011EnConsortium") |>
  select(debate_number, news_substance)
newspaper_substance_2011_EN

## 2011 - FR ##
newspaper_substance_2011_FR =
  newspaper_data_final |>
  filter(Fr11 == 1)|>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2011FrConsortium") |>
  select(debate_number, news_substance)
newspaper_substance_2011_FR

## 2015 - Macleans ##
newspaper_substance_2015_Mac =
  newspaper_data_final |>
  filter(Macleans15 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2015Macleans") |>
  select(debate_number, news_substance)
newspaper_substance_2015_Mac

## 2015 - Globe & Mail ##
newspaper_substance_2015_GM =
  newspaper_data_final |>
  filter(Globe15 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2015Globe&Mail") |>
  select(debate_number, news_substance)
newspaper_substance_2015_GM

## 2015 - Radio-Canada ##
newspaper_substance_2015_RC =
  newspaper_data_final |>
  filter(RC15 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2015Radio-Canada") |>
  select(debate_number, news_substance)
newspaper_substance_2015_RC

## 2015 - Munk ##
newspaper_substance_2015_Munk =
  newspaper_data_final |>
  filter(Munk15 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2015Munk") |>
  select(debate_number, news_substance)
newspaper_substance_2015_Munk

## 2015 - TVA ##
newspaper_substance_2015_TVA =
  newspaper_data_final |>
  filter(TVA15 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2015TVA") |>
  select(debate_number, news_substance)
newspaper_substance_2015_TVA

## 2019 - Macleans ##
newspaper_substance_2019_Mac =
  newspaper_data_final |>
  filter(Macleans19 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2019Macleans") |>
  select(debate_number, news_substance)
newspaper_substance_2019_Mac

## 2019 - TVA ##
newspaper_substance_2019_TVA =
  newspaper_data_final |>
  filter(TVA19 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2019TVA") |>
  select(debate_number, news_substance)
newspaper_substance_2019_TVA

## 2019 - LCD EN ##
newspaper_substance_2019_EN =
  newspaper_data_final |>
  filter(LDCen19 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2019EnLDC") |>
  select(debate_number, news_substance)
newspaper_substance_2019_EN

## 2019 - LCD FR ##
newspaper_substance_2019_FR =
  newspaper_data_final |>
  filter(LDCfr19 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2019FrLDC") |>
  select(debate_number, news_substance)
newspaper_substance_2019_FR

## 2021 - TVA ##
newspaper_substance_2021_TVA =
  newspaper_data_final |>
  filter(TVA21 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2021TVA") |>
  select(debate_number, news_substance)
newspaper_substance_2021_TVA

## 2021 - LCD FR ##
newspaper_substance_2021_FR =
  newspaper_data_final |>
  filter(LDCfr21 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2021FrLDC") |>
  select(debate_number, news_substance)
newspaper_substance_2021_FR

## 2021 - LCD EN ##
newspaper_substance_2021_EN =
  newspaper_data_final |>
  filter(LDCen21 == 1) |>
  summarise(substance_total = n(),
            substance_1_total = sum(Substance == 1)) |>
  mutate(news_substance = substance_1_total / substance_total,
         debate_number = "2021EnLDC") |>
  select(debate_number, news_substance)
newspaper_substance_2021_EN

## Combine all ##
newspaper_substance_all <-
  rbind(
    newspaper_substance_2008_FR,
    newspaper_substance_2008_EN,
    newspaper_substance_2011_EN,
    newspaper_substance_2011_FR,
    newspaper_substance_2015_Mac,
    newspaper_substance_2015_GM,
    newspaper_substance_2015_RC,
    newspaper_substance_2015_Munk,
    newspaper_substance_2015_TVA,
    newspaper_substance_2019_Mac,
    newspaper_substance_2019_TVA,
    newspaper_substance_2019_EN,
    newspaper_substance_2019_FR,
    newspaper_substance_2021_TVA,
    newspaper_substance_2021_FR,
    newspaper_substance_2021_EN)
newspaper_substance_all 

## Fix rounding ##
newspaper_substance_all_final = 
newspaper_substance_all |>
  mutate(news_substance*100) |>
  select(debate_number, `news_substance * 100`) |>
  rename(`news_substance` = `news_substance * 100`) |>
  dplyr::mutate(across(where(is.numeric), round, 1))
newspaper_substance_all_final

#### Calculate "news_format" column for all debates ####
## 2008 - FR ##
newspaper_format_2008_FR =
  newspaper_data_final |>
  filter(Fr08 == 1)|>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2008FrConsortium") |>
  select(debate_number, news_format)
newspaper_format_2008_FR

## 2008 - EN ##
newspaper_format_2008_EN =
  newspaper_data_final |>
  filter(En08 == 1)|>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2008EnConsortium") |>
  select(debate_number, news_format)
newspaper_format_2008_EN

## 2011 - EN ##
newspaper_format_2011_EN =
  newspaper_data_final |>
  filter(En11 == 1)|>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2011EnConsortium") |>
  select(debate_number, news_format)
newspaper_format_2011_EN

## 2011 - FR ##
newspaper_format_2011_FR =
  newspaper_data_final |>
  filter(Fr11 == 1)|>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2011FrConsortium") |>
  select(debate_number, news_format)
newspaper_format_2011_FR

## 2015 - Macleans ##
newspaper_format_2015_Mac =
  newspaper_data_final |>
  filter(Macleans15 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2015Macleans") |>
  select(debate_number, news_format)
newspaper_format_2015_Mac

## 2015 - Globe & Mail ##
newspaper_format_2015_GM =
  newspaper_data_final |>
  filter(Globe15 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2015Globe&Mail") |>
  select(debate_number, news_format)
newspaper_format_2015_GM

## 2015 - Radio-Canada ##
newspaper_format_2015_RC =
  newspaper_data_final |>
  filter(RC15 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2015Radio-Canada") |>
  select(debate_number, news_format)
newspaper_format_2015_RC

## 2015 - Munk ##
newspaper_format_2015_Munk =
  newspaper_data_final |>
  filter(Munk15 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2015Munk") |>
  select(debate_number, news_format)
newspaper_format_2015_Munk

## 2015 - TVA ##
newspaper_format_2015_TVA =
  newspaper_data_final |>
  filter(TVA15 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2015TVA") |>
  select(debate_number, news_format)
newspaper_format_2015_TVA

## 2019 - Macleans ##
newspaper_format_2019_Mac =
  newspaper_data_final |>
  filter(Macleans19 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2019Macleans") |>
  select(debate_number, news_format)
newspaper_format_2019_Mac

## 2019 - TVA ##
newspaper_format_2019_TVA =
  newspaper_data_final |>
  filter(TVA19 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2019TVA") |>
  select(debate_number, news_format)
newspaper_format_2019_TVA

## 2019 - LCD EN ##
newspaper_format_2019_EN =
  newspaper_data_final |>
  filter(LDCen19 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2019EnLDC") |>
  select(debate_number, news_format)
newspaper_format_2019_EN

## 2019 - LCD FR ##
newspaper_format_2019_FR =
  newspaper_data_final |>
  filter(LDCfr19 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2019FrLDC") |>
  select(debate_number, news_format)
newspaper_format_2019_FR

## 2021 - TVA ##
newspaper_format_2021_TVA =
  newspaper_data_final |>
  filter(TVA21 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2021TVA") |>
  select(debate_number, news_format)
newspaper_format_2021_TVA

## 2021 - LCD FR ##
newspaper_format_2021_FR =
  newspaper_data_final |>
  filter(LDCfr21 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2021FrLDC") |>
  select(debate_number, news_format)
newspaper_format_2021_FR

## 2021 - LCD EN ##
newspaper_format_2021_EN =
  newspaper_data_final |>
  filter(LDCen21 == 1) |>
  summarise(format_total = n(),
            format_1_total = sum(Format == 1)) |>
  mutate(news_format = format_1_total / format_total,
         debate_number = "2021EnLDC") |>
  select(debate_number, news_format)
newspaper_format_2021_EN

newspaper_format_all <-
  rbind(
    newspaper_format_2008_FR,
    newspaper_format_2008_EN,
    newspaper_format_2011_EN,
    newspaper_format_2011_FR,
    newspaper_format_2015_Mac,
    newspaper_format_2015_GM,
    newspaper_format_2015_RC,
    newspaper_format_2015_Munk,
    newspaper_format_2015_TVA,
    newspaper_format_2019_Mac,
    newspaper_format_2019_TVA,
    newspaper_format_2019_EN,
    newspaper_format_2019_FR,
    newspaper_format_2021_TVA,
    newspaper_format_2021_FR,
    newspaper_format_2021_EN)
newspaper_format_all 

## Fix rounding ##
newspaper_format_all_final = 
  newspaper_format_all |>
  mutate(news_format*100) |>
  select(debate_number, `news_format * 100`) |>
  rename(`news_format` = `news_format * 100`) |>
  dplyr::mutate(across(where(is.numeric), round, 1))
newspaper_format_all_final

#### Calculate "news_moderation_more" column for all debates ####
## 2008 - FR ##
newspaper_moderation_more_2008_FR =
  newspaper_data_final |>
  filter(Fr08 == 1)|>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2008FrConsortium") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2008_FR

## 2008 - EN ##
newspaper_moderation_more_2008_EN =
  newspaper_data_final |>
  filter(En08 == 1)|>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2008EnConsortium") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2008_EN

## 2011 - EN ##
newspaper_moderation_more_2011_EN =
  newspaper_data_final |>
  filter(En11 == 1)|>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2011EnConsortium") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2011_EN

## 2011 - FR ##
newspaper_moderation_more_2011_FR =
  newspaper_data_final |>
  filter(Fr11 == 1)|>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2011FrConsortium") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2011_FR

## 2015 - Macleans ##
newspaper_moderation_more_2015_Mac =
  newspaper_data_final |>
  filter(Macleans15 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2015Macleans") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2015_Mac

## 2015 - Globe & Mail ##
newspaper_moderation_more_2015_GM =
  newspaper_data_final |>
  filter(Globe15 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2015Globe&Mail") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2015_GM

## 2015 - Radio-Canada ##
newspaper_moderation_more_2015_RC =
  newspaper_data_final |>
  filter(RC15 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2015Radio-Canada") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2015_RC

## 2015 - Munk ##
newspaper_moderation_more_2015_Munk =
  newspaper_data_final |>
  filter(Munk15 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2015Munk") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2015_Munk

## 2015 - TVA ##
newspaper_moderation_more_2015_TVA =
  newspaper_data_final |>
  filter(TVA15 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2015TVA") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2015_TVA

## 2019 - Macleans ##
newspaper_moderation_more_2019_Mac =
  newspaper_data_final |>
  filter(Macleans19 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2019Macleans") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2019_Mac

## 2019 - TVA ##
newspaper_moderation_more_2019_TVA =
  newspaper_data_final |>
  filter(TVA19 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2019TVA") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2019_TVA

## 2019 - LCD EN ##
newspaper_moderation_more_2019_EN =
  newspaper_data_final |>
  filter(LDCen19 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2019EnLDC") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2019_EN

## 2019 - LCD FR ##
newspaper_moderation_more_2019_FR =
  newspaper_data_final |>
  filter(LDCfr19 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2019FrLDC") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2019_FR

## 2021 - TVA ##
newspaper_moderation_more_2021_TVA =
  newspaper_data_final |>
  filter(TVA21 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2021TVA") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2021_TVA

## 2021 - LCD FR ##
newspaper_moderation_more_2021_FR =
  newspaper_data_final |>
  filter(LDCfr21 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2021FrLDC") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2021_FR

## 2021 - LCD EN ##
newspaper_moderation_more_2021_EN =
  newspaper_data_final |>
  filter(LDCen21 == 1) |>
  summarise(moderation_more_total = n(),
            moderation_more_1_total = sum(More_moderation == 1)) |>
  mutate(news_moderation_more = moderation_more_1_total / moderation_more_total,
         debate_number = "2021EnLDC") |>
  select(debate_number, news_moderation_more)
newspaper_moderation_more_2021_EN

## Combine all ##
newspaper_moderation_more_all <-
  rbind(
    newspaper_moderation_more_2008_FR,
    newspaper_moderation_more_2008_EN,
    newspaper_moderation_more_2011_EN,
    newspaper_moderation_more_2011_FR,
    newspaper_moderation_more_2015_Mac,
    newspaper_moderation_more_2015_GM,
    newspaper_moderation_more_2015_RC,
    newspaper_moderation_more_2015_Munk,
    newspaper_moderation_more_2015_TVA,
    newspaper_moderation_more_2019_Mac,
    newspaper_moderation_more_2019_TVA,
    newspaper_moderation_more_2019_EN,
    newspaper_moderation_more_2019_FR,
    newspaper_moderation_more_2021_TVA,
    newspaper_moderation_more_2021_FR,
    newspaper_moderation_more_2021_EN)
newspaper_moderation_more_all 

## Fix rounding ##
newspaper_moderation_more_all_final = 
  newspaper_moderation_more_all |>
  mutate(news_moderation_more*100) |>
  select(debate_number, `news_moderation_more * 100`) |>
  rename(`news_moderation_more` = `news_moderation_more * 100`) |>
  dplyr::mutate(across(where(is.numeric), round, 1))
newspaper_moderation_more_all_final

#### Calculate "news_moderation_less" column for all debates ####
## 2008 - FR ##
newspaper_moderation_less_2008_FR =
  newspaper_data_final |>
  filter(Fr08 == 1)|>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2008FrConsortium") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2008_FR

## 2008 - EN ##
newspaper_moderation_less_2008_EN =
  newspaper_data_final |>
  filter(En08 == 1)|>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2008EnConsortium") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2008_EN

## 2011 - EN ##
newspaper_moderation_less_2011_EN =
  newspaper_data_final |>
  filter(En11 == 1)|>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2011EnConsortium") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2011_EN

## 2011 - FR ##
newspaper_moderation_less_2011_FR =
  newspaper_data_final |>
  filter(Fr11 == 1)|>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2011FrConsortium") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2011_FR

## 2015 - Macleans ##
newspaper_moderation_less_2015_Mac =
  newspaper_data_final |>
  filter(Macleans15 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2015Macleans") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2015_Mac

## 2015 - Globe & Mail ##
newspaper_moderation_less_2015_GM =
  newspaper_data_final |>
  filter(Globe15 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2015Globe&Mail") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2015_GM

## 2015 - Radio-Canada ##
newspaper_moderation_less_2015_RC =
  newspaper_data_final |>
  filter(RC15 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2015Radio-Canada") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2015_RC

## 2015 - Munk ##
newspaper_moderation_less_2015_Munk =
  newspaper_data_final |>
  filter(Munk15 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2015Munk") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2015_Munk

## 2015 - TVA ##
newspaper_moderation_less_2015_TVA =
  newspaper_data_final |>
  filter(TVA15 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2015TVA") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2015_TVA

## 2019 - Macleans ##
newspaper_moderation_less_2019_Mac =
  newspaper_data_final |>
  filter(Macleans19 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2019Macleans") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2019_Mac

## 2019 - TVA ##
newspaper_moderation_less_2019_TVA =
  newspaper_data_final |>
  filter(TVA19 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2019TVA") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2019_TVA

## 2019 - LCD EN ##
newspaper_moderation_less_2019_EN =
  newspaper_data_final |>
  filter(LDCen19 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2019EnLDC") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2019_EN

## 2019 - LCD FR ##
newspaper_moderation_less_2019_FR =
  newspaper_data_final |>
  filter(LDCfr19 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2019FrLDC") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2019_FR

## 2021 - TVA ##
newspaper_moderation_less_2021_TVA =
  newspaper_data_final |>
  filter(TVA21 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2021TVA") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2021_TVA

## 2021 - LCD FR ##
newspaper_moderation_less_2021_FR =
  newspaper_data_final |>
  filter(LDCfr21 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2021FrLDC") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2021_FR

## 2021 - LCD EN ##
newspaper_moderation_less_2021_EN =
  newspaper_data_final |>
  filter(LDCen21 == 1) |>
  summarise(moderation_less_total = n(),
            moderation_less_1_total = sum(Less_moderation == 1)) |>
  mutate(news_moderation_less = moderation_less_1_total / moderation_less_total,
         debate_number = "2021EnLDC") |>
  select(debate_number, news_moderation_less)
newspaper_moderation_less_2021_EN

## Combine all seperate datasets ##
newspaper_moderation_less_all <-
  rbind(
    newspaper_moderation_less_2008_FR,
    newspaper_moderation_less_2008_EN,
    newspaper_moderation_less_2011_EN,
    newspaper_moderation_less_2011_FR,
    newspaper_moderation_less_2015_Mac,
    newspaper_moderation_less_2015_GM,
    newspaper_moderation_less_2015_RC,
    newspaper_moderation_less_2015_Munk,
    newspaper_moderation_less_2015_TVA,
    newspaper_moderation_less_2019_Mac,
    newspaper_moderation_less_2019_TVA,
    newspaper_moderation_less_2019_EN,
    newspaper_moderation_less_2019_FR,
    newspaper_moderation_less_2021_TVA,
    newspaper_moderation_less_2021_FR,
    newspaper_moderation_less_2021_EN)
newspaper_moderation_less_all 

## Fix rounding ##
newspaper_moderation_less_all_final = 
  newspaper_moderation_less_all |>
  mutate(news_moderation_less*100) |>
  select(debate_number, `news_moderation_less * 100`) |>
  rename(`news_moderation_less` = `news_moderation_less * 100`) |>
  dplyr::mutate(across(where(is.numeric), round, 1))
newspaper_moderation_less_all_final

#### Calculate "news_moderation_praise" column for all debates ####
## 2008 - FR ##
newspaper_moderation_praise_2008_FR =
  newspaper_data_final |>
  filter(Fr08 == 1)|>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2008FrConsortium") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2008_FR

## 2008 - EN ##
newspaper_moderation_praise_2008_EN =
  newspaper_data_final |>
  filter(En08 == 1)|>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2008EnConsortium") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2008_EN

## 2011 - EN ##
newspaper_moderation_praise_2011_EN =
  newspaper_data_final |>
  filter(En11 == 1)|>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2011EnConsortium") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2011_EN

## 2011 - FR ##
newspaper_moderation_praise_2011_FR =
  newspaper_data_final |>
  filter(Fr11 == 1)|>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2011FrConsortium") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2011_FR

## 2015 - Macleans ##
newspaper_moderation_praise_2015_Mac =
  newspaper_data_final |>
  filter(Macleans15 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2015Macleans") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2015_Mac

## 2015 - Globe & Mail ##
newspaper_moderation_praise_2015_GM =
  newspaper_data_final |>
  filter(Globe15 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2015Globe&Mail") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2015_GM

## 2015 - Radio-Canada ##
newspaper_moderation_praise_2015_RC =
  newspaper_data_final |>
  filter(RC15 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2015Radio-Canada") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2015_RC

## 2015 - Munk ##
newspaper_moderation_praise_2015_Munk =
  newspaper_data_final |>
  filter(Munk15 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2015Munk") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2015_Munk

## 2015 - TVA ##
newspaper_moderation_praise_2015_TVA =
  newspaper_data_final |>
  filter(TVA15 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2015TVA") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2015_TVA

## 2019 - Macleans ##
newspaper_moderation_praise_2019_Mac =
  newspaper_data_final |>
  filter(Macleans19 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2019Macleans") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2019_Mac

## 2019 - TVA ##
newspaper_moderation_praise_2019_TVA =
  newspaper_data_final |>
  filter(TVA19 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2019TVA") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2019_TVA

## 2019 - LCD EN ##
newspaper_moderation_praise_2019_EN =
  newspaper_data_final |>
  filter(LDCen19 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2019EnLDC") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2019_EN

## 2019 - LCD FR ##
newspaper_moderation_praise_2019_FR =
  newspaper_data_final |>
  filter(LDCfr19 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2019FrLDC") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2019_FR

## 2021 - TVA ##
newspaper_moderation_praise_2021_TVA =
  newspaper_data_final |>
  filter(TVA21 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2021TVA") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2021_TVA

## 2021 - LCD FR ##
newspaper_moderation_praise_2021_FR =
  newspaper_data_final |>
  filter(LDCfr21 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2021FrLDC") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2021_FR

## 2021 - LCD EN ##
newspaper_moderation_praise_2021_EN =
  newspaper_data_final |>
  filter(LDCen21 == 1) |>
  summarise(moderation_praise_total = n(),
            moderation_praise_1_total = sum(Moderation_good == 1)) |>
  mutate(news_moderation_praise = moderation_praise_1_total / moderation_praise_total,
         debate_number = "2021EnLDC") |>
  select(debate_number, news_moderation_praise)
newspaper_moderation_praise_2021_EN

## Combine all ##
newspaper_moderation_praise_all <-
  rbind(
    newspaper_moderation_praise_2008_FR,
    newspaper_moderation_praise_2008_EN,
    newspaper_moderation_praise_2011_EN,
    newspaper_moderation_praise_2011_FR,
    newspaper_moderation_praise_2015_Mac,
    newspaper_moderation_praise_2015_GM,
    newspaper_moderation_praise_2015_RC,
    newspaper_moderation_praise_2015_Munk,
    newspaper_moderation_praise_2015_TVA,
    newspaper_moderation_praise_2019_Mac,
    newspaper_moderation_praise_2019_TVA,
    newspaper_moderation_praise_2019_EN,
    newspaper_moderation_praise_2019_FR,
    newspaper_moderation_praise_2021_TVA,
    newspaper_moderation_praise_2021_FR,
    newspaper_moderation_praise_2021_EN)
newspaper_moderation_praise_all 

## Fix rounding ##
newspaper_moderation_praise_all_final = 
  newspaper_moderation_praise_all |>
  mutate(news_moderation_praise*100) |>
  select(debate_number, `news_moderation_praise * 100`) |>
  rename(`news_moderation_praise` = `news_moderation_praise * 100`) |>
  dplyr::mutate(across(where(is.numeric), round, 1))
newspaper_moderation_praise_all_final

#### Add "video_point_count" column for all debates ####
# Add in data from table #
video_point_data <- 
  data <- data.frame(
    debate_number = c(
      "2008EnConsortium",
      "2008FrConsortium",
      "2011EnConsortium",
      "2011FrConsortium",
      "2015Macleans",
      "2015Globe&Mail",
      "2015Radio-Canada",
      "2015Munk",
      "2015TVA",
      "2019Macleans",
      "2019TVA",
      "2019EnLDC",
      "2019FrLDC",
      "2021TVA",
      "2021FrLDC",
      "2021EnLDC") ,
    video_point_count = c(15, NA, 9, NA, 1, 6, 11, 11, NA, 9, 9, 23, 5, 17, 8, 8))
video_point_data

#### Add "video_fist_count" column for all debates ####
# Add in data from table #
video_fist_data <- 
  data <- data.frame(
    debate_number = c(
      "2008EnConsortium",
      "2008FrConsortium",
      "2011EnConsortium",
      "2011FrConsortium",
      "2015Macleans",
      "2015Globe&Mail",
      "2015Radio-Canada",
      "2015Munk",
      "2015TVA",
      "2019Macleans",
      "2019TVA",
      "2019EnLDC",
      "2019FrLDC",
      "2021TVA",
      "2021FrLDC",
      "2021EnLDC") ,
    video_fist_count = c(1, NA, 0, NA, 0, 0, 0, 0, NA, 1, 0, 0, 0, 0, 0, 0))
video_fist_data

#### Add "video_crosstalk" column for all debates ####
# Add in data from table #
video_crosstalk_data <- 
  data <- data.frame(
    debate_number = c(
      "2008EnConsortium",
      "2008FrConsortium",
      "2011EnConsortium",
      "2011FrConsortium",
      "2015Macleans",
      "2015Globe&Mail",
      "2015Radio-Canada",
      "2015Munk",
      "2015TVA",
      "2019Macleans",
      "2019TVA",
      "2019EnLDC",
      "2019FrLDC",
      "2021TVA",
      "2021FrLDC",
      "2021EnLDC") ,
    video_crosscount = c(5.5, NA, 5.6, NA, 3.2, 5.2, 4.4, 1.7, 8.5, 9.3, 9.5, 12.0, 3.25, 7.4, 4.9, 5.1))
video_crosstalk_data

#### Add "demands_in_words" column for all debates ####
# Add in data from table #
demands_in_words_data <- 
  data <- data.frame(
    debate_number = c(
      "2008EnConsortium",
      "2008FrConsortium",
      "2011EnConsortium",
      "2011FrConsortium",
      "2015Macleans",
      "2015Globe&Mail",
      "2015Radio-Canada",
      "2015Munk",
      "2015TVA",
      "2019Macleans",
      "2019TVA",
      "2019EnLDC",
      "2019FrLDC",
      "2021TVA",
      "2021FrLDC",
      "2021EnLDC") ,
    demands_in_words = c(NA, 64, 66, NA, 67, 74, 71, 67, 70, 83, 80, 63, 78, 59, 80, 70))
demands_in_words_data

#### Add "moderator_objections" column for all debates ####
# Add in data from table #
moderator_objections_data <- 
  data <- data.frame(
    debate_number = c(
      "2008EnConsortium",
      "2008FrConsortium",
      "2011EnConsortium",
      "2011FrConsortium",
      "2015Macleans",
      "2015Globe&Mail",
      "2015Radio-Canada",
      "2015Munk",
      "2015TVA",
      "2019Macleans",
      "2019TVA",
      "2019EnLDC",
      "2019FrLDC",
      "2021TVA",
      "2021FrLDC",
      "2021EnLDC") ,
    moderator_objections = c(NA, 1, 0, NA, 1, 2, 3, 2, 3, 3, 2, 3, 1, 11, 9, 13))
moderator_objections_data

#### Add "leader_objections" column for all debates ####
# Add in data from table #
leader_objections_data <- 
  data <- data.frame(
    debate_number = c(
      "2008EnConsortium",
      "2008FrConsortium",
      "2011EnConsortium",
      "2011FrConsortium",
      "2015Macleans",
      "2015Globe&Mail",
      "2015Radio-Canada",
      "2015Munk",
      "2015TVA",
      "2019Macleans",
      "2019TVA",
      "2019EnLDC",
      "2019FrLDC",
      "2021TVA",
      "2021FrLDC",
      "2021EnLDC") ,
    leader_objections = c(NA, 3, 4, NA, 1, 0, 5, 0, 2, 1, 2, 6, 0, 2, 0, 2))
leader_objections_data

#### Join all individual datasets together to create full "by_debates" dataset ####
# Code referenced from: https://stackoverflow.com/questions/70093718/how-to-use-left-join-on-several-data-frames
by_debates_new <-
  list(
    dqi_by_speech_all_final,
    dqi_positional_pol_final,
    dqi_respect_final,
    dqi_participation_final,
    newspaper_substance_all_final,
    newspaper_format_all_final,
    newspaper_moderation_more_all_final,
    newspaper_moderation_less_all_final,
    newspaper_moderation_praise_all_final,
    video_point_data,
    video_fist_data,
    video_crosstalk_data,
    demands_in_words_data,
    leader_objections_data,
    moderator_objections_data
  ) |>
  reduce(left_join, by = "debate_number") |>
  mutate(election_year = case_when(
    startsWith(debate_number, "2008") ~ "2008",
    startsWith(debate_number, "2011") ~ "2011",
    startsWith(debate_number, "2015") ~ "2015",
    startsWith(debate_number, "2019") ~ "2019",
    startsWith(debate_number, "2021") ~ "2021"),
    .before = debate_number) 

#### Save replicated dataset ####
write_csv(x = by_debates_new, file = "Outputs/Data/by_debates_replicated.csv")