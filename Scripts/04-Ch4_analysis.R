#### Preamble ####
# Purpose: Calculate various DQI things for Ch4
# Author: Inessa De Angelis
# Date: 10 February 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(tinytable)
library(marginaleffects)
library(kableExtra)

dqi_by_speech_final <- read_csv("Outputs/Data/dqi_by_speech_final.csv")
by_debates_final <- read_csv("Outputs/Data/by_debates_final.csv")

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

## Analysis data ##
dqi_in <- dqi_by_speech_dem |>
  drop_na(Interruption) |>
  mutate(Political_affiliation = case_when(
    Political_affiliation == "Conservative" ~ 1,
    Political_affiliation == "Liberal" ~ 2,
    Political_affiliation == "NDP" ~ 3,
    Political_affiliation == "Green" ~ 4,
    Political_affiliation == "Bloc Québécois" ~ 5,
    Political_affiliation == "People's Party" ~ 6)) |>
  mutate(Speaker = case_when(
    Speaker == "Trudeau" ~ 1,
    Speaker == "Singh" ~ 2,
    Speaker == "May" ~ 3,
    Speaker == "Scheer" ~ 4,
    Speaker == "O'Toole" ~ 5,
    Speaker == "Harper" ~ 6,
    Speaker == "Blanchet" ~ 7,
    Speaker == "Duceppe" ~ 8,
    Speaker == "Mulcair" ~ 9,
    Speaker == "Layton" ~ 10,
    Speaker == "Ignatieff" ~ 11,
    Speaker == "Dion" ~ 12,
    Speaker == "Paul" ~ 13,
    Speaker == "Bernier" ~ 14)) |>
  #filter(!Number_of_Debaters_in_Segment == "99")
  
## Add in data ##
dqi_2 <- time_dataset |>
  select(Debate_number, Number_of_participants)

dqi_in <- dqi_in |>
  left_join(participant_data, by = join_by(Debate_number))

## Re-code variables ##
dqi_in$Language <- ifelse(dqi_in$Language == "French", 1, 0)
dqi_in$Gender <- ifelse(dqi_in$Gender == "Man", 1, 0)
dqi_in$Background <- ifelse(dqi_in$Background == "White", 1, 0)
dqi_in$Incumbent <- ifelse(dqi_in$Incumbent == "Incumbent", 1, 0)
dqi_in$Interruption <- ifelse(dqi_in$Interruption == "Interruption", 1, 0)
dqi_in$Political_affiliation <- factor(dqi_in$Political_affiliation)
dqi_in$Number_of_Debaters_in_Segment <- factor(dqi_in$Number_of_Debaters_in_Segment)
#dqi_in$Number_of_participants <- factor(dqi_in$Number_of_participants)
dqi_in$Organizer <- factor(dqi_in$Organizer)
dqi_in$Speaker <- factor(dqi_in$Speaker)
#dqi_in$Actual_length <- factor(dqi_in$Actual_length)

dqi_in <- dqi_in |>
  mutate("Justification" = case_when(
    Justification == "No_Justification" ~ "0",
    Justification == "Inferior_Justification" ~ "1",
    Justification == "Qualified_Justification" ~ "2"))
  
dqi_in$Justification <- as.numeric(dqi_in$Justification)

## Add all in data from Table 3 in Chapter 2 ##
time_dataset <- data.frame(
  Debate_number = c("2008FrConsortium", "2008EnConsortium", "2011EnConsortium", "2011FrConsortium",
                    "2015Macleans", "2015Globe&Mail", "2015FrConsortium", "2015Munk", "2015TVA",
                    "2019Macleans", "2019TVA", "2019EnLDC", "2019FrLDC", "2021TVA", "2021FrLDC", "2021EnLDC"),
  Lead_questions = c(14, 8, 6, 10, 7, 10, 17, 9, 33, 11, 39, 18, 40, 40, 50, 45),
  Number_of_participants = c(5, 5, 4, 4, 4, 3, 5, 3, 4, 3, 4, 6, 6, 4, 5, 5),
  Average_time_per_Q_per_speaker = c(1.7, 2.7, 4.5, 3.0, 3.6, 2.5, 1.2, 3.6, 0.8, 2.2, 0.6, 0.9, 0.4, 0.6, 0.3, 0.4),
  Total_time_per_Q = c(8.5, 13.5, 18.1, 12.0, 14.3, 7.6, 6.0, 10.7, 3.1, 6.6, 2.6, 5.6, 2.6, 2.3, 1.7, 2.2),
  Non_leader_participation = c(19.8, 9.8, 9.7, 18.6, 19.7, 12.3, 17.2, 11.7, 16.7, 13.2, 26.6, 21.4, 16.8, 
                               34.9, 36.8, 21.7),
  Actual_length = c(119, 118, 118, 120, 120, 88, 120, 108, 120, 86, 128, 122, 120, 126, 120, 120))
time_dataset


both <- by_debates_final |> 
  left_join(time_dataset, by = join_by(Debate_number))

dqi_final <-  dqi_by_speech_dem |> 
  left_join(both, by = join_by(Debate_number))

## Create a new column "Lead_questions" ##
dqi_in$new_Lead_questions <- with(dqi_in, ifelse(Lead_questions < 10, 'less_than_10',
                                                 ifelse(Lead_questions >= 10 & Lead_questions <= 20, '10_20',
                                                 ifelse(Lead_questions > 20 & Lead_questions <= 30, '20_30',
                                                 ifelse(Lead_questions > 30 & Lead_questions <= 40, '30_40',
                                                 ifelse(Lead_questions > 40 & Lead_questions <= 50, '40_50', 'above_50'))))))

dqi_in$new_Lead_questions <- factor(dqi_in$new_Lead_questions)

## Add in select data from Table 3, Chapter 2 ##
participant_data <- data.frame(
  Debate_number = c("2008FrConsortium", "2008EnConsortium", "2011EnConsortium", "2011FrConsortium",
                    "2015Macleans", "2015Globe&Mail", "2015FrConsortium", "2015Munk", "2015TVA",
                    "2019Macleans", "2019TVA", "2019EnLDC", "2019FrLDC", "2021TVA", "2021FrLDC", "2021EnLDC"),
  Lead_questions = c(14, 8, 6, 10, 7, 10, 17, 9, 33, 11, 39, 18, 40, 40, 50, 45),
  Number_of_participants = c(5, 5, 4, 4, 4, 3, 5, 3, 4, 3, 4, 6, 6, 4, 5, 5))

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

correlation <- cor(dqi_in$Justification, dqi_in$Language, method = 'pearson')
print(correlation)

test_mod <- lm(Interruption ~ Language + Number_of_participants, data = dqi_in)
summary(test_mod)

dqi_in$Number_of_participants <- factor(dqi_in$Number_of_participants)
test_logit <- glm(Interruption ~ Language + Number_of_participants,
                data = dqi_in, family = binomial(link = "logit"))

comp <- slopes(test_logit, newdata = "median")

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

#### Make models - Interruptions + Format ####
m1 <- lm(Interruption ~ Organizer + Number_of_Debaters_in_Segment + Political_affiliation, data = dqi_in)
summary(m1)

m1_logit <- glm(Interruption ~ Organizer + Number_of_Debaters_in_Segment +Number_of_participants + Language,
                data = dqi_in, family = binomial(link = "logit"))
summary(m1_logit)

slopes(m1_logit, newdata = "median") |>
  select(term, contrast, estimate, p.value) |>
  kable(col.names = c("Term", "Contrast", "Estimate", "Pr(>|z|)"),
    digits = 3, booktabs = TRUE)

comp <- slopes(m1_logit, newdata = "median") |>
  select(term, contrast, estimate, p.value) |>
  mutate(contrast = case_when(
    contrast == "1 - 0" & term == "Language" ~ "French - English",
    TRUE ~ contrast),
    estimate = round(estimate, 3),
    p.value= round(p.value, 3)) 

tt(comp)

m1_predictions <-
  predictions(m1_logit) |>
  as_tibble()
  m1_predictions
  