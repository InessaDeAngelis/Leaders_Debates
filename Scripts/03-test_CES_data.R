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
## 2008 survey
cleaned_ces2008 <- read_csv("Outputs/Data/CES/cleaned_ces2008.csv")

## 2011 survey
cleaned_ces2011 <- read_csv("Outputs/Data/CES/cleaned_ces2011.csv")  

## 2015 survey
cleaned_ces2015_combined <- read_csv("Outputs/Data/CES/cleaned_ces2015_combined.csv")

## 2019 survey
# Other data
cleaned_ces2019_web <- read_csv("Outputs/Data/CES/cleaned_ces2019_web.csv")

# Main issues
cleaned_ces2019_issues <- read_csv("Outputs/Data/CES/cleaned_ces2019_issues.csv")

## 2021 survey
# Other data
cleaned_ces2021_web <- read_csv("Outputs/Data/CES/cleaned_ces2021_web.csv")

# Main issues
cleaned_ces2021_issues <- read_csv("Outputs/Data/CES/cleaned_ces2021_issues.csv")

#### Test 2008 survey ####
## Check class ##
class(cleaned_ces2008$ID) == "numeric"
class(cleaned_ces2008$important_issues) == "character"

## Check number of rows is correct ##
nrow(cleaned_ces2008) == 2783

## Check for completeness ##
rules <- validator(
  is_complete(important_issues)
)

out <- confront(cleaned_ces2008, rules)
summary(out)

#### Test 2011 survey ####
## Check class ##
class(cleaned_ces2011$ID) == "numeric"
class(cleaned_ces2011$important_issues) == "character"

## Check number of rows is correct ##
nrow(cleaned_ces2011) == 4308

## Check for completeness ##
rules <- validator(
  is_complete(important_issues)
)

out <- confront(cleaned_ces2011, rules)
summary(out)

#### Test 2015 survey ####
## Check class ##
class(cleaned_ces2015_combined$ID) == "numeric"
class(cleaned_ces2015_combined$important_issues) == "character"

## Check number of rows is correct ##
nrow(cleaned_ces2015_combined) == 4202

## Check for completeness ##
rules <- validator(
  is_complete(important_issues)
)

out <- confront(cleaned_ces2015_combined, rules)
summary(out)

#### Test 2019 survey ####
## Other data ##
# Check class #
class(cleaned_ces2019_web$ID) == "character"
class(cleaned_ces2019_web$watched_EN_debate) == "character"
class(cleaned_ces2019_web$watched_FR_debate) == "character"

# Check number of rows is correct #
nrow(cleaned_ces2019_web) == 12993

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

## Check for completeness ##
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

## Check for completeness ##
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