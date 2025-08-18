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


#### Make multinomial logistic regression ####
model <- multinom(Justification ~ Organizer + Number_of_Debaters_in_Segment + Language, data = analysis_data)
summary(model)

z <- summary(model)$coefficients / summary(model)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))
round(p, 4)

tidy(model, conf.int = TRUE) |> print(n=25)
