#### Preamble ####
# Purpose: Calculate various DQI things for Ch4
# Author: Inessa De Angelis
# Date: 10 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

dqi_by_speech_final <- read_csv("Outputs/Data/dqi_by_speech_final.csv")

## Add in demographic data ##
dqi_by_speech_dem <- dqi_by_speech_final |>
  filter(Speaker == "Harper" | Speaker == "Dion" | Speaker == "May"| Speaker == "Layton" | Speaker == "Duceppe" |
           Speaker == "Ignatieff" | Speaker == "Trudeau" | Speaker == "Singh" | Speaker == "Paul" |
           Speaker == "Mulcair" | Speaker == "Blanchet" | Speaker == "Scheer" | Speaker == "O'Toole" | Speaker == "Bernier") |>
  mutate(Gender = if_else(Speaker %in% c("May", "Paul"), "Woman", "Man"), .after = Speaker) |>
  mutate(Background = if_else(Speaker %in% c("Singh", "Paul"), "Racialized", "White"), .after = Gender) |>
  mutate(Incumbent = if_else((Speaker == "Trudeau" & Election_year >= 2019) | 
                               (Speaker == "Harper" & Election_year <= 2015), 
                             "Incumbent", 
                             "Not Incumbent"), .after = Background) |> 
  mutate(Political_affiliation = case_when(
    Speaker == "Harper" ~ "Conservative",
    Speaker == "Dion" ~ "Liberal",
    Speaker == "May" ~ "Green",
    Speaker == "Layton" ~ "NDP",
    Speaker == "Duceppe" ~ "Bloc Québécois",
    Speaker == "Ignatieff" ~ "Liberal",
    Speaker == "Trudeau" ~ "Liberal",
    Speaker == "Singh" ~ "NDP",
    Speaker == "Paul" ~ "Green",
    Speaker == "Mulcair" ~ "NDP",
    Speaker == "Blanchet" ~ "Bloc Québécois",
    Speaker == "Scheer" ~ "Conservative",
    Speaker == "O'Toole" ~ "Conservative",
    Speaker == "Bernier" ~ "People's Party"), .after = Incumbent)

#### Summary stats ####
## All speeches and interrupted speeches given by party leaders ##
total_speeches <- dqi_by_speech_dem |> 
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
total_speeches

## All speeches and interrupted speeches by incumbency ##
speeches_incumbency <- dqi_by_speech_dem |>
  group_by(Incumbent) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_incumbency

## All speeches and interrupted speeches by gender ##
speeches_gender <- dqi_by_speech_dem |>
  group_by(Gender) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_gender

## All speeches and interrupted speeches by background ##
speeches_bg <- dqi_by_speech_dem |>
  group_by(Background) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_bg

## All speeches and interrupted speeches by gender + background ##
speeches_gender_bg <- dqi_by_speech_dem |>
  group_by(Gender, Background) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_gender_bg

## All speeches and interrupted speeches by political party affiliation ##
speeches_party <- dqi_by_speech_dem |>
  group_by(Political_affiliation) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_party

#### Make models ####
## Analysis data ##
dqi_in <-dqi_by_speech_dem |>
  drop_na(Interruption) |>
  mutate(Political_affiliation = case_when(
    Political_affiliation == "Conservative" ~ 1,
    Political_affiliation == "Liberal" ~ 2,
    Political_affiliation == "NDP" ~ 3,
    Political_affiliation == "Green" ~ 4,
    Political_affiliation == "Bloc Québécois" ~ 5,
    Political_affiliation == "People's Party" ~ 6))

## Re-code variables ##
dqi_in$Gender <- ifelse(dqi_in$Gender == "Man", 1, 0)
dqi_in$Background <- ifelse(dqi_in$Background == "White", 1, 0)
dqi_in$Incumbent <- ifelse(dqi_in$Incumbent == "Incumbent", 1, 0)
dqi_in$Interruption <- ifelse(dqi_in$Interruption == "Interruption", 1, 0)
dqi_in$Political_affiliation <- factor(dqi_in$Political_affiliation)

## Model interruptions with gender and background as predictors (with interaction) ##
model <- lm(Interruption ~ Gender * Background, data = dqi_in)
summary(model)

## Model interruptions with gender and background as predictors (without interaction) ##
model2 <- lm(Interruption ~ Gender + Background, data = dqi_in)
summary(model2)

## Model interruptions by incumbency ##
model3 <- lm(Interruption ~ Incumbent, data = dqi_in)
summary(model3)

## Model interruptions by gender ##
model4 <- lm(Interruption ~ Gender, data = dqi_in)
summary(model4)

## Model interruptions by background ##
model5 <- lm(Interruption ~ Background, data = dqi_in)
summary(model5)

## Model interruptions by political party affiliation ##
model6 <- lm(Interruption ~ Political_affiliation, data = dqi_in)
summary(model6)

## Model interruptions by gender, background, incumbency AND politicial affiliation ##
model7 <- lm(Interruption ~ Gender + Background + Incumbent + Political_affiliation, data = dqi_in)
summary(model7)
