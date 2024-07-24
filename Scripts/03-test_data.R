#### Preamble ####
# Purpose: Tests the questions by debate dataset
# Author: Inessa De Angelis
# Date: 14 March 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
  # 02-clean_data.R

#### Workspace setup ####
library(tidyverse)
library(validate)

#### Read in cleaned dataset ####
debate_questions_cleaned <- read_csv("Outputs/Data/debate_questions_cleaned.csv")

#### Test dataset ####
# Check class #
rules <- validator(
  is.character(primary_issue),
  is.character(secondary_issue),
  primary_issue %vin% c(
    "Crime and public safety",
    "Social welfare",
    "Economy",
    "Environment",
    "Other",
    "Public Finances",
    "Health care",
    "Ethics",
    "Foreign affairs",
    "Public finance",
    "Immigration",
    "Government formation",
    "Constitution",
    "Defence",
    "Culture and heritage",
    "Energy",
    "Agriculture",
    "Housing",
    "Covid",
    "Federalism",
    "Indigenous",
    "Transport",
    "Official Languages",
    "Women",
    "Justice"
  ),
  secondary_issue %vin% c(
    "Crime and public safety",
    "Social welfare",
    "Economy",
    "Environment",
    "Other",
    "Public Finances",
    "Health care",
    "Ethics",
    "Foreign affairs",
    "Public finance",
    "Immigration",
    "Government formation",
    "Constitution",
    "Defence",
    "Culture and heritage",
    "Energy",
    "Agriculture",
    "Housing",
    "Covid",
    "Federalism",
    "Indigenous",
    "Transport",
    "Official Languages",
    "Women",
    "Justice"
  )
)

out <-
  confront(debate_questions_cleaned, rules)

summary(out)

# Check for completeness #
rules <- validator(
  is_complete(primary_issue),
  is_complete(secondary_issue)
)

out <- confront(debate_questions_cleaned, rules)
summary(out)

# Check number of rows is correct #
nrow(debate_questions_cleaned) == 587