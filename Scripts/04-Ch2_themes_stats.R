#### Preamble ####
# Purpose: Get summary stats on main debate themes for Chapter 2
# Author: Inessa De Angelis
# Date: 17 September 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(formattable)

#### Read in dataset ####
debate_questions_final <- read_csv(file = "Outputs/Data/debate_questions_final.csv")

#### Re-code categories (to align with Bastien & CES) ####
debate_questions_themes <-
  debate_questions_final |>
  select(Year, Debate_number, Theme) |>
  mutate("Theme" = case_when(
    Theme == "None" ~ "None",
    Theme == "none" ~ "None",
    Theme == "Economy" ~ "Economy",
    Theme == "The economy" ~ "Economy",
    Theme == "Economy & Public Finances" ~ "Economy",
    Theme == "Economy and spending" ~ "Economy",
    Theme == "Cost of Living & Public Finances" ~ "Economy",
    Theme == "Consumers/Cost of Living" ~ "Economy",
    Theme == "The Pandemic" ~ "Covid", 
    Theme == "Pandemic & Health" ~ "Covid", 
    Theme == "COVID RECOVERY" ~ "Covid", 
    Theme == "Canada in the Future" ~ "Other",
    Theme == "energy and the environment." ~ "Environment",
    Theme == "The environment and energy" ~ "Environment",
    Theme == "Energy and Environment" ~ "Energy",
    Theme == "Economy & Environment" ~ "Economy",
    Theme == "Canada in the world" ~ "Foreign affairs",
    Theme == "leadership in Canada and the world" ~ "Foreign affairs",
    Theme == "Canada on the world stage" ~ "Foreign affairs",
    Theme == "foreign policy and security" ~ "Foreign affairs",
    Theme == "Canada's Democracy" ~ "Ethics",
    Theme == "Jobs" ~ "Jobs", 
    Theme == "housing" ~ "Housing",
    Theme == "infrastructure" ~ "Infrastructure",
    Theme == "taxation" ~ "Economy",
    Theme == "immigration" ~ "Immigration",
    Theme == "Immigration, Social Politics" ~ "Immigration",
    Theme == "Governance, democracy, and institutions" ~ "Constitution",
    Theme == "Leadership and governance" ~ "Government formation",
    Theme == "Governance and democracy" ~ "Government formation",
    Theme == "Government services" ~ "Other",
    Theme == "Canadian Security" ~ "Defence",
    Theme == "Social Policy & Governance" ~ "Social welfare",
    Theme == "Environment" ~ "Environment",
    Theme == "Climate Change" ~ "Environment",
    Theme == "polarization, human rights, and immigration" ~ "Immigration",
    Theme == "affordability and income security"  ~ "Economy",
    Theme == "Foreign Affairs & Immigration" ~ "Foreign affairs",
    Theme == "Citizen Services" ~ "Other",
    Theme == "Identity, Ethics, & Governence" ~ "Ethics",
    Theme == "Justice & Foreign Affairs" ~ "Justice",
    Theme == "Indigenous Identity & Culture" ~ "Indigenous",
    Theme == "Indigenous Issues" ~ "Indigenous",
    Theme == "Reconciliation" ~ "Indigenous",
    Theme == "Crime" ~ "Crime",
    Theme == "Culture and national identity" ~ "Culture and heritage",
    Theme == "Health care" ~ "Health care",
    Theme == "Quebec's place in Canada" ~ "Constitution",
    Theme == "Values" ~ "Other",
    Theme == "Social policy" ~ "Social welfare",
    Theme == "Social Policy" ~ "Social welfare",
    Theme == "Governance & Quebec" ~"Constitution",
    Theme == "Affordability" ~ "Economy",
    Theme == "LEADERSHIP AND ACCOUNTABILITY" ~ "Ethics")) 

## Save dataset ##
write_csv(debate_questions_themes, "Outputs/Data/debate_questions_themes.csv")

## Generate summary stats ##
themes_summary_stats =
debate_questions_themes |>
  drop_na(Theme) |>
  filter(!Theme == "None") |>
  group_by(Theme) |>
  count() |>
  ungroup() |>
  rename(Number = n) |>
  mutate (Percent = percent(Number / sum(Number), digit = 1)) |>
  print(n = 20) 

new_df <- themes_summary_stats |> 
  arrange(desc(Percent)) |>
  print(n = 20)

#### Old code ####
## Re-code categories (only clean up existing categories) ##
debate_questions_themes2 <-
  debate_questions_final |>
  select(Year, Debate_number, Theme) |>
  mutate("Theme" = case_when(
    Theme == "None" ~ "None",
    Theme == "none" ~ "None",
    Theme == "Economy" ~ "Economy",
    Theme == "The economy" ~ "Economy",
    Theme == "Economy & Public Finances" ~ "Economy and public finances",
    Theme == "Economy and spending" ~ "Economy and spending",
    Theme == "Cost of Living & Public Finances" ~ "Cost of living and public finances",
    Theme == "Consumers/Cost of Living" ~ "Consumers and cost of living",
    Theme == "The Pandemic" ~ "The Pandemic", 
    Theme == "Pandemic & Health" ~ "Pandemic and health", 
    Theme == "COVID RECOVERY" ~ "Covid recovery", 
    Theme == "Canada in the Future" ~ "Canada in the future",
    Theme == "energy and the environment." ~ "Energy and the environment",
    Theme == "The environment and energy" ~ "The environment and energy",
    Theme == "Energy and Environment" ~ "Energy and environment",
    Theme == "Economy & Environment" ~ "Economy and environment",
    Theme == "Canada in the Future" ~ "Canada in the future",
    Theme == "Canada in the world" ~ "Canada in the world",
    Theme == "leadership in Canada and the world" ~ "Leadership in Canada and the world",
    Theme == "Canada on the world stage" ~ "Canada on the world stage",
    Theme == "foreign policy and security" ~ "Foreign policy and security",
    Theme == "Canada's Democracy" ~ "Canada's democracy",
    Theme == "Jobs" ~ "Jobs", 
    Theme == "housing" ~ "Housing",
    Theme == "infrastructure" ~ "Infrastructure",
    Theme == "taxation" ~ "Taxation",
    Theme == "immigration" ~ "Immigration",
    Theme == "Immigration, Social Politics" ~ "Immigration and social politics",
    Theme == "Governance, democracy, and institutions" ~ "Governance, democracy and institutions",
    Theme == "Leadership and governance" ~ "Leadership and governance",
    Theme == "Governance and democracy" ~ "Governance and democracy",
    Theme == "Government services" ~ "Government services",
    Theme == "Canadian Security" ~ "Canadian security",
    Theme == "Social Policy & Governance" ~ "Social policy and governance",
    Theme == "Environment" ~ "Environment",
    Theme == "Climate Change" ~ "Climate change",
    Theme == "polarization, human rights, and immigration" ~ "Polarization, human rights and immigration",
    Theme == "affordability and income security"  ~ "Affordability and income security",
    Theme == "Foreign Affairs & Immigration" ~ "Foreign affairs and immigration",
    Theme == "Citizen Services" ~ "Citizen services",
    Theme == "Identity, Ethics, & Governence" ~ "Identity, ethics and governance",
    Theme == "Justice & Foreign Affairs" ~ "Justice and foreign affairs",
    Theme == "Indigenous Identity & Culture" ~ "Indigenous identity and culture",
    Theme == "Indigenous Issues" ~ "Indigenous issues",
    Theme == "Reconciliation" ~ "Reconciliation",
    Theme == "Crime" ~ "Crime",
    Theme == "Culture and national identity" ~ "Culture and national identity",
    Theme == "Health care" ~ "Health care",
    Theme == "Quebec's place in Canada" ~ "Quebec's place in Canada",
    Theme == "Values" ~ "Values",
    Theme == "Social policy" ~ "Social policy",
    Theme == "Social Policy" ~ "Social policy",
    Theme == "Governance & Quebec" ~"Governance and Quebec",
    Theme == "Affordability" ~ "Affordability",
    Theme == "LEADERSHIP AND ACCOUNTABILITY" ~ "Leadership and accountability")) 

## Save dataset ##
write_csv(debate_questions_themes2, "Outputs/Data/debate_questions_themes2.csv")

## Generate summary stats ##
themes_summary_stats2 =
  debate_questions_themes2 |>
  drop_na(Theme) |>
  filter(!Theme == "None") |>
  group_by(Debate_number, Theme) |>
  count() |>
  ungroup() |>
  rename(Number = n) |>
  mutate (Percent = percent(Number / sum(Number), digit = 1)) |>
  print(n = 65) 

new_df2 <- themes_summary_stats2 |> 
  arrange(desc(Percent)) |>
  print(n = 65)