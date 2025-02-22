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
debate_questions_final <- read_csv(file = "Outputs/Data/debate_questions_final_recoded.csv")

#### Re-code categories (to align with Bastien & CES) ####
debate_questions_themes <-
  debate_questions_final |>
  select(ID, Year, Debate_number, Theme) |>
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

## Generate summary stats - old ##
themes_summary_stats <- debate_questions_themes |>
  group_by(Theme) |>
  count() |>
  ungroup() |>
  rename(Number = n) |>
  mutate (Percent = percent(Number / sum(Number), digit = 1)) |>
  print(n = 65) 

new_df <- themes_summary_stats |> 
  arrange(desc(Percent)) |>
  print(n = 65)

themes_again <- debate_questions_themes |>
  filter(!Theme == "None") |>
  group_by(Debate_number, Theme) |>
  summarise(covered = 1) |> # Mark it as '1' if covered
  ungroup()

## Crosstabs - new ##
crosstabs_df <- table(debate_questions_themes$Debate_number, debate_questions_themes$Theme)
print(crosstabs_df)

df <- data.frame(crosstabs_df)

df <- df |>
  rename(Debate_number = Var1,
         Theme = Var2) |>
  arrange(Debate_number)

write_csv(df, "crosstabs_df2.csv")

df2 <- df |>
  filter(!grepl('0', Freq))

write_csv(df2, "themes_crosstabs.csv")

## Binary version ##
my_data_binary <- debate_questions_themes |>
  group_by(Debate_number, Theme) |>
  summarise(covered = 1) |> # Mark it as '1' if covered
  ungroup()

crosstab_df <- xtabs(~ Debate_number + Theme, data = my_data_binary)
print(crosstab_df)

df3 <- data.frame(crosstab_df) 

df3 <- df3 |>
  filter(Freq == 1) |>
  filter(!Theme == "None") |>
  arrange(Debate_number) |>
  select(Debate_number, Theme)

df4 <- df3 |> 
  group_by(Theme) |>
count() |>
  ungroup() |>
  rename(Number = n) |>
  mutate (Percent = percent(Number / sum(Number), digit = 1)) |>
  arrange(Number) |>
  print(n = 20) 

write_csv(df3, "df3.csv")

write_csv(df4, "df4.csv")