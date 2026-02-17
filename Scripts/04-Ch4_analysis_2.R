#### Preamble ####
# Purpose: Calculate various DQI things for Ch4
# Author: Inessa De Angelis
# Date: 19 May 2025
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

#### Make models - By Debates dataset ####
# Add in data from table 3 in Ch2 #
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
  left_join(by_debates_final, by = join_by(Debate_number))

## Factor number of participants column ##
both$Number_of_participants <- factor(both$Number_of_participants)

## Create a new column "dqi_justification" ##
both$new_dqi_justification <- ifelse(both$dqi_justification >= 1.15, 'more_justification', 'less_justification')
both$new_dqi_justification <- ifelse(both$new_dqi_justification == "more_justification", 1, 0)

## Create a new column "dqi_percent_demands" ##
both$new_dqi_percent_demands <- ifelse(both$dqi_percent_demands >= 0.20, 'more_demands', 'less_demands')
both$new_dqi_percent_demands <- ifelse(both$new_dqi_percent_demands == "more_demands", 1, 0)
both$new_dqi_percent_demands <- factor(both$new_dqi_percent_demands)

## Create a new column "dqi_percent_demands" ##
both$new_dqi_positional_politics <- ifelse(both$dqi_positional_politics == 100, '100', 'less_than_100')
both$new_dqi_positional_politics <- ifelse(both$new_dqi_positional_politics == "100", 1, 0)
both$new_dqi_positional_politics <- factor(both$new_dqi_positional_politics)

## Create a new column "Lead_questions" ##
both$new_Lead_questions <- with(both, ifelse(Lead_questions < 10, 'less_than_10',
                                      ifelse(Lead_questions >= 10 & Lead_questions <= 20, '10_20',
                                      ifelse(Lead_questions > 20 & Lead_questions <= 30, '20_30',
                                      ifelse(Lead_questions > 30 & Lead_questions <= 40, '30_40',
                                      ifelse(Lead_questions > 40 & Lead_questions <= 50, '40_50', 'above_50'))))))

both$new_Lead_questions <- factor(both$new_Lead_questions)

#### Make model ####
m2_logit <- glm(new_dqi_justification ~ Number_of_participants + new_Lead_questions,
                data = both, family = binomial(link = "logit"))
summary(m2_logit)

comp2 <- slopes(m2_logit, newdata = "median") |>
  select(term, contrast, estimate, p.value) |>
  mutate(
    estimate = round(estimate, 7),
    p.value= round(p.value, 7)) 

tt(comp2)

m2_predictions <-
  predictions(m2_logit) |>
  mutate(
    estimate = round(estimate, 7),
    p.value= round(p.value, 7)) |>
  as_tibble()

m2_predictions

table_data <- table(both$new_dqi_justification, both$new_Lead_questions)
fisher.test(table_data)
