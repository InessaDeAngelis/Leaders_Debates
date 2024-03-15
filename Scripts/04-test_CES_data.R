#### Preamble ####
# Purpose: Tests the cleaned 2008-2021 CES datasets
# Author: Inessa De Angelis
# Date: 29 February 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites:
  # 01-download_CES_data.R
  # 02-clean_CES_data.R

#### Workspace setup ####
library(tidyverse)
library(validate)

## Read in datasets ##
## 2008 survey ##
cleaned_ces2008 <- read_csv("Outputs/Data/CES/cleaned_ces2008.csv")

# Re-coded issues #
recoded_ces2008 <- read_csv("Outputs/Data/CES/recoding/recoded_ces2008.csv")

## 2011 survey ##
cleaned_ces2011 <- read_csv("Outputs/Data/CES/cleaned_ces2011.csv")  

# Re-coded issues #
recoded_ces2011 <- read_csv("Outputs/Data/CES/recoding/recoded_ces2011.csv")

## 2015 survey ##
cleaned_ces2015_combined <- read_csv("Outputs/Data/CES/cleaned_ces2015_combined.csv")

# Re-coded issues #
recoded_ces2015 <- read_csv("Outputs/Data/CES/recoding/recoded_ces2015.csv")

## 2019 survey ##
# Other data #
cleaned_ces2019_web <- read_csv("Outputs/Data/CES/cleaned_ces2019_web.csv")

# Main issues #
cleaned_ces2019_issues <- read_csv("Outputs/Data/CES/cleaned_ces2019_issues.csv")

# Summarized issues #
summarized_ces2019_issues <- read_csv("Outputs/Data/CES/summarized_ces2019_issues.csv")

# Re-coded issues #
recoded_ces2019 <- read_csv("Outputs/Data/CES/recoding/recoded_ces2019.csv")

# Re-coded issues & by language
joined_ces2019_issues <- read_csv("Outputs/Data/CES/joined_ces2019.csv")

## 2021 survey ##
# Other data #
cleaned_ces2021_web <- read_csv("Outputs/Data/CES/cleaned_ces2021_web.csv")

# Main issues #
cleaned_ces2021_issues <- read_csv("Outputs/Data/CES/cleaned_ces2021_issues.csv")

# Summarized issues #
summarized_ces2021_issues <- read_csv("Outputs/Data/CES/summarized_ces2021_issues.csv")

# Re-coded issues #
recoded_ces2021 <- read_csv("Outputs/Data/CES/recoding/recoded_ces2021.csv")

# Re-coded issues & by language
joined_ces2021_issues <- read_csv("Outputs/Data/CES/joined_ces2021.csv")

#### Test 2008 survey ####
## Cleaned survey data ##
# Check for type of variables #
rules <- validator(
  is.numeric(ID),
  is.character(important_issues),
  is.character(language),
  language %vin% c("English", "French") # Check to make sure the only options for 'language' is English or French. 
)

out <-
  confront(cleaned_ces2008, rules)

summary(out)

# Check number of rows is correct #
nrow(cleaned_ces2008) == 2783

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(cleaned_ces2008, rules)
summary(out)

## Re-coded survey data ##
# Check class #
rules <- validator(
  is.numeric(ID),
  is.character(important_issues),
  important_issues %vin% c("Crime and public safety", "Social Welfare", "Economy", "Environment", "Other", "Public Finances", "Health", "Education", "Ethics")
)
  
out <-
  confront(recoded_ces2008, rules)

summary(out)

# Check number of rows is correct #
nrow(recoded_ces2008) == 2783

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(recoded_ces2008, rules)
summary(out)

#### Test 2011 survey ####
## Cleaned survey data ##
# Check class #
rules <- validator(
  is.numeric(ID),
  is.character(important_issues),
  is.character(language),
  language %vin% c("English", "French") # Check to make sure the only options for 'language' is English or French. 
)

out <-
  confront(cleaned_ces2011, rules)

summary(out)

# Check number of rows is correct #
nrow(cleaned_ces2011) == 4308

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(cleaned_ces2011, rules)
summary(out)

## Re-coded survey data ##
class(recoded_ces2011$ID) == "numeric"
class(recoded_ces2011$important_issues) == "character"

# Check number of rows is correct #
nrow(recoded_ces2011) == 4308

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(recoded_ces2011, rules)
summary(out)

#### Test 2015 survey ####
## Cleaned survey data ##
# Check class #
rules <- validator(
  is.numeric(ID),
  is.character(important_issues),
  is.character(language),
  language %vin% c("English", "French") # Check to make sure the only options for 'language' is English or French. 
)

out <-
  confront(cleaned_ces2015_combined, rules)

summary(out)

# Check number of rows is correct #
nrow(cleaned_ces2015_combined) == 4202

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(cleaned_ces2015_combined, rules)
summary(out)

## Re-coded survey data ##
class(recoded_ces2015$ID) == "numeric"
class(recoded_ces2015$important_issues) == "character"

# Check number of rows is correct #
nrow(recoded_ces2015) == 4202

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(recoded_ces2015, rules)
summary(out)

#### Test 2019 survey ####
## Other data ##
# Check class #
rules <- validator(
  is.character(ID),
  is.character(watched_EN_debate),
  is.character(watched_FR_debate),
  is.character(language),
  language %vin% c("English", "French") # Check to make sure the only options for 'language' is English or French. 
)

out <-
  confront(cleaned_ces2019_web, rules)

summary(out)

# Check number of rows is correct #
nrow(cleaned_ces2019_web) == 37822

## Main issues data ##
# Check class #
class(cleaned_ces2019_issues$ID) == "character"
class(cleaned_ces2019_issues$Economy) == "numeric"
class(cleaned_ces2019_issues$Environment) == "numeric"
class(cleaned_ces2019_issues$Immigration) == "numeric"
class(cleaned_ces2019_issues$Healthcare) == "numeric"
class(cleaned_ces2019_issues$Housing) == "numeric"
class(cleaned_ces2019_issues$Seniors) == "numeric"
class(cleaned_ces2019_issues$Leaders) == "numeric"
class(cleaned_ces2019_issues$Ethics) == "numeric"
class(cleaned_ces2019_issues$Education) == "numeric"
class(cleaned_ces2019_issues$Crime) == "numeric"
class(cleaned_ces2019_issues$Indigenous) == "numeric"
class(cleaned_ces2019_issues$Welfare) == "numeric"
class(cleaned_ces2019_issues$Election) == "numeric"
class(cleaned_ces2019_issues$Women) == "numeric"
class(cleaned_ces2019_issues$Security) == "numeric"
class(cleaned_ces2019_issues$Quebec) == "numeric"
class(cleaned_ces2019_issues$Race) == "numeric"
class(cleaned_ces2019_issues$Immigration_and_race) == "numeric"
class(cleaned_ces2019_issues$Ethics_and_leaders) == "numeric"
class(cleaned_ces2019_issues$Other_welfare) == "numeric"
class(cleaned_ces2019_issues$Number_of_categories) == "numeric"

# Check number of rows is correct #
nrow(cleaned_ces2019_issues) == 26044

# Check for completeness #
rules <- validator(
  is_complete(Economy,
              Environment,
              Immigration,
              Healthcare,
              Housing,
              Seniors,
              Leaders,
              Ethics,
              Education,
              Crime,
              Indigenous,
              Welfare,
              Election,
              Women,
              Security,
              Quebec,
              Race,
              Immigration_and_race,
              Ethics_and_leaders,
              Other_welfare,
              Number_of_categories)
)

out <- confront(cleaned_ces2019_issues, rules)
summary(out)

## Summarized survey data ##
class(summarized_ces2019_issues$ID) == "character"
class(summarized_ces2019_issues$important_issues) == "character"

# Check number of rows is correct #
nrow(summarized_ces2019_issues) == 23521

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(summarized_ces2019_issues, rules)
summary(out)

## Re-coded survey data ##
class(recoded_ces2019$ID) == "character"
class(recoded_ces2019$important_issues) == "character"

# Check number of rows is correct #
nrow(recoded_ces2019) == 23521

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(recoded_ces2019, rules)
summary(out)

## Re-coded issues & by language ##
rules <- validator(
  is.character(ID),
  is.character(language),
  is.character(important_issues),
  language %vin% c("English", "French") # Check to make sure the only options for 'language' is English or French. 
)

out <-
  confront(joined_ces2019_issues, rules)

summary(out)

# Check number of rows is correct #
nrow(joined_ces2019_issues) == 23521

#### Test 2021 survey ####
## Other data ##
# Check class #
class(cleaned_ces2021_web$ID) == "character"
class(cleaned_ces2021_web$watched_EN_debate) == "character"
class(cleaned_ces2021_web$watched_FR_debate) == "character"
class(cleaned_ces2021_web$watched_FR_debate2) == "character"

nrow(cleaned_ces2021_web) == 11895

## Main issues data ##
# Check class #
class(cleaned_ces2021_issues$ID) == "character"
class(cleaned_ces2021_issues$Economy) == "numeric"
class(cleaned_ces2021_issues$Environment) == "numeric"
class(cleaned_ces2021_issues$Immigration) == "numeric"
class(cleaned_ces2021_issues$Healthcare) == "numeric"
class(cleaned_ces2021_issues$Housing) == "numeric"
class(cleaned_ces2021_issues$Seniors) == "numeric"
class(cleaned_ces2021_issues$Leaders) == "numeric"
class(cleaned_ces2021_issues$Ethics) == "numeric"
class(cleaned_ces2021_issues$Education) == "numeric"
class(cleaned_ces2021_issues$Crime) == "numeric"
class(cleaned_ces2021_issues$Indigenous) == "numeric"
class(cleaned_ces2021_issues$Welfare) == "numeric"
class(cleaned_ces2021_issues$Election) == "numeric"
class(cleaned_ces2021_issues$Women) == "numeric"
class(cleaned_ces2021_issues$Security) == "numeric"
class(cleaned_ces2021_issues$Quebec) == "numeric"
class(cleaned_ces2021_issues$Race) == "numeric"
class(cleaned_ces2021_issues$Covid) == "numeric"
class(cleaned_ces2021_issues$Immigration_and_race) == "numeric"
class(cleaned_ces2021_issues$Ethics_and_leaders) == "numeric"
class(cleaned_ces2021_issues$Other_welfare) == "numeric"
class(cleaned_ces2021_issues$Number_of_categories) == "numeric"

# Check number of rows is correct #
nrow(cleaned_ces2021_issues) == 17541

# Check for completeness #
rules <- validator(
  is_complete(Economy,
              Environment,
              Immigration,
              Healthcare,
              Housing,
              Seniors,
              Leaders,
              Ethics,
              Education,
              Crime,
              Indigenous,
              Welfare,
              Election,
              Women,
              Security,
              Quebec,
              Race,
              Covid,
              Immigration_and_race,
              Ethics_and_leaders,
              Other_welfare,
              Number_of_categories)
)

out <- confront(cleaned_ces2021_issues, rules)
summary(out)

## Summarized survey data ##
class(summarized_ces2021_issues$ID) == "character"
class(summarized_ces2021_issues$important_issues) == "character"

# Check number of rows is correct #
nrow(summarized_ces2021_issues) == 15926

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(summarized_ces2021_issues, rules)
summary(out)

## Re-coded survey data ##
class(recoded_ces2021$ID) == "character"
class(recoded_ces2021$important_issues) == "character"

# Check number of rows is correct #
nrow(recoded_ces2021) == 15926

# Check for completeness #
rules <- validator(
  is_complete(important_issues)
)

out <- confront(recoded_ces2021, rules)
summary(out)

## Re-coded issues & by language ##
rules <- validator(
  is.character(ID),
  is.character(language),
  is.character(important_issues),
  language %vin% c("English", "French") # Check to make sure the only options for 'language' is English or French. 
)

out <-
  confront(joined_ces2021_issues, rules)

summary(out)

# Check number of rows is correct #
nrow(joined_ces2021_issues) == 15926