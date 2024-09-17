#### Preamble ####
# Purpose: Get summary stats on main debate themes for Chapter 2
# Author: Inessa De Angelis
# Date: 17 September 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in dataset ####
debate_questions_final <- read_csv(file = "Outputs/Data/debate_questions_final.csv")

#### Re-code categories ####
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
    Theme == "Canada in the Future" ~ "Canada in the future",
    Theme == "energy and the environment." ~ "Environment and energy",
    Theme == "The environment and energy" ~ "Environment and energy",
    Theme == "Energy and Environment" ~ "Environment and energy",
    Theme == "Economy & Environment" ~ "Economy and environment",
    Theme == "Canada in the Future" ~ "Canada in the future",
    Theme == "Canada in the world" ~ "Canada in the world",
    Theme == "leadership in Canada and the world" ~ "Canada in the world",
    Theme == "Canada on the world stage" ~ "Canada in the world",
    Theme == "foreign policy and security" ~ "Foreign policy and security",
    Theme == "Canada's Democracy" ~ "Canada's democracy",
    Theme == "Jobs" ~ "Jobs", 
    Theme == "housing" ~ "Housing",
    Theme == "infrastructure" ~ "Infrastructure",
    Theme == "taxation" ~ "Taxation",
    Theme == "immigration" ~ "Immigration",
    Theme == "Immigration, Social Politics" ~ "Immigration",
    Theme == "Governance, democracy, and institutions" ~ "Governance",
    Theme == "Leadership and governance" ~ "Governance",
    Theme == "Governance and democracy" ~ "Governance",
    Theme == "Government services" ~ "Government services",
    Theme == "Canadian Security" ~ "Canadian security",
    Theme == "Social Policy & Governance" ~ "Social policy",
    Theme == "Environment" ~ "Environment",
    Theme == "Climate Change" ~ "Environment",
    Theme == "polarization, human rights, and immigration" ~ "Polarization, human rights, and immigration",
    Theme == "affordability and income security" ~ "Affordability and income security",
    Theme == "Foreign Affairs & Immigration" ~ "Foreign affairs and immigration",
    Theme == "Citizen Services" ~ "Citizen services",
    Theme == "Identity, Ethics, & Governence" ~ "Identity, ethics and governence",
    Theme == "Justice & Foreign Affairs" ~ "Justice and foreign affairs",
    Theme == "Indigenous Identity & Culture" ~ "Indigenous",
    Theme == "Indigenous Issues" ~ "Indigenous",
    Theme == "Reconciliation" ~ "Indigenous",
    Theme == "Crime" ~ "Crime",
    Theme == "Culture and national identity" ~ "Culture and national identity",
    Theme == "Health care" ~ "Health care",
    Theme == "Quebec's place in Canada" ~ "Quebec's place in Canada",
    Theme == "Values" ~ "Values",
    Theme == "Social policy" ~ "Social policy",
    Theme == "Social Policy" ~ "Social policy",
    Theme == "Governance & Quebec" ~"Governance and Quebec",
    Theme == "Affordability" ~ "Affordability",
    Theme == "LEADERSHIP AND ACCOUNTABILITY" ~ "Governance"
    )) 