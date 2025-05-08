#### Preamble ####
# Purpose: Calculate various DQI things for Ch4
# Author: Inessa De Angelis
# Date: 10 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(ggplot2)

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

#### Summary stats - Interruptions ####
## All speeches and interrupted speeches given by party leaders ##
total_speeches <- dqi_by_speech_dem |> 
  drop_na(Interruption) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
total_speeches

## All speeches and interrupted speeches by incumbency ##
speeches_incumbency <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  group_by(Incumbent) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_incumbency

## All speeches and interrupted speeches by gender ##
speeches_gender <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  group_by(Gender) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_gender

## All speeches and interrupted speeches by background ##
speeches_bg <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  group_by(Background) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_bg

## All speeches and interrupted speeches by gender + background ##
speeches_gender_bg <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  group_by(Gender, Background) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_gender_bg

## All speeches and interrupted speeches by political party affiliation ##
speeches_party <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  group_by(Political_affiliation) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_party

## By individual debate + speaker + incumbency ##
speeches_by_debate <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  group_by(Debate_number, Speaker, Incumbent) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_by_debate

## By individual speaker ##
speeches_by_speaker <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  group_by(Speaker) |>
  summarise(
    interrupted_speeches = sum(Interruption == "Interruption", na.rm = TRUE),
    total_speeches = n(),
    percent_interrupted = round((sum(Interruption == "Interruption", na.rm = TRUE) / n()) * 100, 2))
speeches_by_speaker

#### Make models - Interruptions ####
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

## Model interruptions by gender, background, incumbency AND political affiliation ##
model7 <- lm(Interruption ~ Gender + Background + Incumbent + Political_affiliation, data = dqi_in)
summary(model7)

#### Data visualization ####
dqi_viz <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  mutate("Interruption" = case_when(
    Interruption == "Normal_Participation" ~ "Normal participation",
    Interruption == "Interruption" ~ "Interruption"))

#jpeg("interruptions_party.jpeg", units="in", width=9, height=5, res=300) 
ggplot(dqi_viz, aes(Interruption, Election_year/1000000)) +
  geom_bar(stat='identity', fill="black") +
  facet_wrap(~Political_affiliation) +
  labs(
    x = "Type of participation",
    y = "Percentage of speeches",
  ) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels=scales::percent) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text.x =  element_text(size = 10)) +
  theme(axis.text.y = element_text(size = 10)) + 
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(legend.title = element_text(size = 12)) +
  theme(legend.text = element_text(size = 10)) 
#dev.off()

#### Summary stats - Respect for demands ####
total_speeches_disrespect <- dqi_by_speech_dem |> 
  drop_na(Respect_for_demands) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
total_speeches_disrespect

## All speeches and respect for demands ##
total_speeches_respect <- dqi_by_speech_dem |> 
  drop_na(Respect_for_demands) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
total_speeches_respect

## All speeches and respect by incumbency ##
speeches_incumbency_respect <- dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  group_by(Incumbent) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
speeches_incumbency_respect 

## All speeches and respect by gender ##
speeches_gender_respect <- dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  group_by(Gender) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
speeches_gender_respect

## All speeches and respect by background ##
speeches_bg_respect <- dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  group_by(Background) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
speeches_bg_respect

## All speeches and respect by gender + background ##
speeches_genderbg_respect <- dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  group_by(Gender, Background) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
speeches_genderbg_respect

## All speeches and respect by political party affiliation ##
speeches_party_respect <- dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  group_by(Political_affiliation) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
speeches_party_respect

## All speeches and respect by individual leader, incumbency, and debate ##
speeches_speaker_respect <- dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  group_by(Debate_number, Speaker, Incumbent) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
speeches_speaker_respect

## All speeches and respect by individual leader only ##
speeches_leader_respect <- dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  group_by(Speaker) |>
  summarise(
    respected_speeches = sum(Respect_for_demands == "Respect", na.rm = TRUE),
    total_speeches = n(),
    percent_respect = round((sum(Respect_for_demands == "Respect", na.rm = TRUE) / n()) * 100, 2))
speeches_leader_respect

#### Make models - Respect for demands ####
## Analysis data ##
dqi_respect <-dqi_by_speech_dem |>
  drop_na(Respect_for_demands) |>
  mutate(Political_affiliation = case_when(
    Political_affiliation == "Conservative" ~ 1,
    Political_affiliation == "Liberal" ~ 2,
    Political_affiliation == "NDP" ~ 3,
    Political_affiliation == "Green" ~ 4,
    Political_affiliation == "Bloc Québécois" ~ 5,
    Political_affiliation == "People's Party" ~ 6))

## Re-code variables ##
dqi_respect$Gender <- ifelse(dqi_respect$Gender == "Man", 1, 0)
dqi_respect$Background <- ifelse(dqi_respect$Background == "White", 1, 0)
dqi_respect$Incumbent <- ifelse(dqi_respect$Incumbent == "Incumbent", 1, 0)
dqi_respect$Respect_for_demands <- ifelse(dqi_respect$Respect_for_demands == "Respect", 1, 0)
dqi_respect$Political_affiliation <- factor(dqi_respect$Political_affiliation)

## Model respect with gender and background as predictors (with interaction) ##
mod <- lm(Respect_for_demands ~ Gender * Background, data = dqi_respect)
summary(mod)

## Model respect with gender and background as predictors (without interaction) ##
mod2 <- lm(Respect_for_demands ~ Gender + Background, data = dqi_respect)
summary(mod2)

## Model respect by incumbency ##
mod3 <- lm(Respect_for_demands ~ Incumbent, data = dqi_respect)
summary(mod3)

## Model respect by gender ##
mod4 <- lm(Respect_for_demands ~ Gender, data = dqi_respect)
summary(mod4)

## Model respect by background ##
mod5 <- lm(Respect_for_demands ~ Background, data = dqi_respect)
summary(mod5)

## Model respect by political party affiliation ##
mod6 <- lm(Respect_for_demands ~ Political_affiliation, data = dqi_respect)
summary(mod6)

## Model respect by gender, background, incumbency AND political affiliation ##
mod7 <- lm(Respect_for_demands ~ Gender + Background + Incumbent + Political_affiliation, data = dqi_respect)
summary(mod7)

#### Make models - justification ####
m1 <- lm(Interruption ~ Debate_organizer, Number_of_Debaters_in_Segment, Presence_of_demands, Political_affiliation, data = dqi_in)
summary(m1)
