#### Preamble ####
# Purpose: Update and streamline models for Ch4
# Author: Inessa De Angelis
# Date: 11 August 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(marginaleffects)
library(nnet)
library(broom)

dqi_by_speech_final <- read_csv("Outputs/Data/dqi_by_speech_final.csv")
by_debates_final <- read_csv("Outputs/Data/by_debates_final.csv")

#### Prepare analysis data ####
analysis_data <- dqi_by_speech_final |>
  drop_na(Interruption) |>
  filter(!Number_of_Debaters_in_Segment == "99")

## Re-code variables ##
analysis_data$Language <- ifelse(analysis_data$Language == "French", 1, 0)
analysis_data$Interruption <- ifelse(analysis_data$Interruption == "Interruption", 1, 0)
analysis_data$Number_of_Debaters_in_Segment <- factor(analysis_data$Number_of_Debaters_in_Segment)
analysis_data$Organizer <- factor(analysis_data$Organizer)
analysis_data$Justification <- factor(analysis_data$Justification,
                           levels = c("No_Justification", "Inferior_Justification", "Qualified_Justification"))


#### Make multinomial logistic regression - Justification ####
justification_model <- multinom(Justification ~ Organizer + Number_of_Debaters_in_Segment + Language, data = analysis_data)
summary(justification_model)

z <- summary(justification_model)$coefficients / summary(justification_model)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
round(p, 4)

tidy(justification_model, conf.int = TRUE) |> print(n=25)

sl <- slopes(justification_model, newdata = "median")
head(sl, 25)
tail(sl, 10)

#### Make logistic regression - Interruption ####
interruption_model <- glm(Interruption ~ Organizer + Number_of_Debaters_in_Segment + Language,
                data = dqi_in, family = binomial(link = "logit"))
summary(interruption_model)

slopes(interruption_model, newdata = "median")

#### Re-run interruption logistic regression model with only 2 or more participants in a segment ####
## Further prepare data ##
analysis_data_reduced <- analysis_data |>
  filter(!Number_of_Debaters_in_Segment == "1") 

analysis_data_reduced$Number_of_Debaters_in_Segment <- 
  factor(analysis_data_reduced$Number_of_Debaters_in_Segment)

interruption_model_reduced <- glm(Interruption ~ Organizer + Number_of_Debaters_in_Segment + Language,
                                  data = analysis_data_reduced, family = binomial(link = "logit"))
summary(interruption_model_reduced)

slopes(interruption_model_reduced, newdata = "median")

#### Save models ####
write_rds(justification_model, "Outputs/Models/justification_model.rds")
write_rds(interruption_model, "Outputs/Models/interruption_model.rds")
write_rds(interruption_model_reduced, "Outputs/Models/interruption_model_reduced.rds")

#### Prepare dataset 
## Add all in data from Table 3 in Chapter 2 ##
time_dataset <- data.frame(
  Debate_number = c(
    "2008FrConsortium",
    "2008EnConsortium",
    "2011EnConsortium",
    "2011FrConsortium",
    "2015Macleans",
    "2015Globe&Mail",
    "2015FrConsortium",
    "2015Munk",
    "2015TVA",
    "2019Macleans",
    "2019TVA",
    "2019EnLDC",
    "2019FrLDC",
    "2021TVA",
    "2021FrLDC",
    "2021EnLDC"
  ),
  Lead_questions = c(14, 8, 6, 10, 7, 10, 17, 9, 33, 11, 39, 18, 40, 40, 50, 45),
  Number_of_participants = c(5, 5, 4, 4, 4, 3, 5, 3, 4, 3, 4, 6, 6, 4, 5, 5),
  Average_time_per_Q_per_speaker = c(
    1.7,
    2.7,
    4.5,
    3.0,
    3.6,
    2.5,
    1.2,
    3.6,
    0.8,
    2.2,
    0.6,
    0.9,
    0.4,
    0.6,
    0.3,
    0.4
  ),
  Total_time_per_Q = c(
    8.5,
    13.5,
    18.1,
    12.0,
    14.3,
    7.6,
    6.0,
    10.7,
    3.1,
    6.6,
    2.6,
    5.6,
    2.6,
    2.3,
    1.7,
    2.2
  ),
  Non_leader_participation = c(
    19.8,
    9.8,
    9.7,
    18.6,
    19.7,
    12.3,
    17.2,
    11.7,
    16.7,
    13.2,
    26.6,
    21.4,
    16.8,
    34.9,
    36.8,
    21.7
  ),
  Actual_length = c(
    119,
    118,
    118,
    120,
    120,
    88,
    120,
    108,
    120,
    86,
    128,
    122,
    120,
    126,
    120,
    120
  ),
  Number_of_Segments = c(
  19,
  16,
  13,
  13,
  15,
  12,
  18,
  13,
  25,
  5,
  27,
  18,
  22,
  29,
  5,
  25)
)
time_dataset

## Join with the "By Debates" dataset ##
both <- by_debates_final |> 
  left_join(time_dataset, by = join_by(Debate_number))

## Test variable class ##
class(both$Number_of_participants)
class(both$video_crosstalk)
class(both$Number_of_Segments)
class(both$dqi_percent_demands)
class(both$demands_in_words)

#### Correlation Tests - Cross Talk ####
cor.test(both$video_crosstalk, both$Number_of_participants, method = "pearson")

#### Correlation Tests - Segments and Demands ####
cor.test(both$dqi_percent_demands, both$Number_of_Segments, method = "pearson")
cor.test(both$demands_in_words, both$Number_of_Segments, method = "pearson")
