#### Preamble ####
# Purpose: Download CES data
# Author: Inessa De Angelis
# Date: 28 February 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None
# Note: cesR package doesn't consistently work and doesn't cover all CES data
  # All of 2021, 2019 re-coded issues, and 2015 combined (web and phone) are missing - download directly from CES website

#### Workspace setup ####
library(tidyverse)
library(labelled)
library(haven)
library(cesR) # Try to use when possible 

#### Download data using cesR package ####
## Call all surveys
get_cescodes()

## Get 2019 web survey
get_ces("ces2019_web")

## Get 2011 survey
get_ces("ces2011")

## Get 2008 survey
get_ces("ces2008")

#### Convert values to factor type ####
## 2019 survey
ces2019_web <- to_factor(ces2019_web)
head(ces2019_web)

## 2011 survey
ces2011 <- to_factor(ces2011)
head(ces2011)

## 2008 survey
ces2008 <- to_factor(ces2008)
head(ces2008)

#### Select columns of interest from each survey ####
## 2019 survey
raw_ces2019_web =
  ces2019_web |>
  select(
    cps19_ResponseId,
    cps19_debate_en,
    cps19_debate_fr,
    cps19_imp_iss,
    cps19_province,
    cps19_Q_Language
  ) 
raw_ces2019_web

## 2011 survey
raw_ces2011 =
  ces2011 |>
  select(
    CES11_IDNUM,
    CPS11_1,
    REGION11,
    PROVINCE11,
    CPS_INTLANG11,
    CPS11_76,
    CPS11_77
  )
raw_ces2011

## 2008 survey
raw_ces2008 =
  ces2008 |>
  select(
    ces08_IDNUM,
    ces08_CPS_A2,
    ces08_CPS_INTLANG,
    ces08_PROVINCE,
    ces08_CPS_R1,
    ces08_CPS_R2A,
    ces08_CPS_R2B,       
    ces08_CPS_R3A,
    ces08_CPS_R3B,
    ces08_CPS_R4,
    ces08_CPS_R5A,
    ces08_CPS_R5B,    
    ces08_CPS_R6A,
    ces08_CPS_R6B
  )
raw_ces2008

#### Save datasets ####
## 2019 survey
write_csv(x = raw_ces2019_web, file = "Inputs/Data/CES/raw_ces2019_web.csv")

## 2011 survey
write_csv(x = raw_ces2011, file = "Inputs/Data/CES/raw_ces2011.csv")

## 2008 survey
write_csv(x = raw_ces2008, file = "Inputs/Data/CES/raw_ces2008.csv")

#### Download data directly from CES website ####
## 2015 survey (combined web and telephone)
# Download and unzip combined data
zip_file <- "Inputs/Data/large_files/CES2015_Combined_CSV.zip"

download.file("https://ces-eec.sites.olt.ubc.ca/files/2017/04/CES2015_Combined_CSV.zip", zip_file)
unzip(zip_file, exdir = "Inputs/Data/large_files")

# Read in CSV file
raw_ces2015_combined <- read_csv("Inputs/Data/large_files/CES2015_Combined_CSV.csv")

## 2019 survey
# Read in re-coded 2019 issues data
raw_ces2019_issues <- read_dta(here::here("Inputs/Data/CES/CES19_dictionarycoding.dta"))
head(raw_ces2019_issues)

## 2021 survey
# Download data from: https://dataverse.harvard.edu/file.xhtml?fileId=7517836&version=3.0 & read it in
# Read in general 2021 data 
raw_ces2021_web <- read_dta(here::here("Inputs/Data/CES/2021_CES_v2.dta"))
head(raw_ces2021_web)

# Download data from: https://dataverse.harvard.edu/file.xhtml?fileId=7079561&version=3.0 
# Read in re-coded 2021 issues data
raw_ces2021_issues <- read_dta(here::here("Inputs/Data/CES/CES21_dictionarycoding.dta"))
head(raw_ces2021_issues)

#### Convert values to factor type ####
## 2019 survey
raw_ces2019_issues <- to_factor(raw_ces2019_issues)
head(raw_ces2019_issues)

## 2021 survey
# Other data
raw_ces2021_web <- to_factor(raw_ces2021_web)

# Main issues
raw_ces2021_issues <- to_factor(raw_ces2021_issues)

#### Select columns of interest from each survey ####
## 2015 survey
raw_ces2015_combined =
  raw_ces2015_combined |>
  select(
    ID,
   main_issue,
   province,
   language
  ) 
raw_ces2015_combined

## 2019 survey
raw_ces2019_issues =
  raw_ces2019_issues |>
  select(
      cps19_ResponseId,
      economydum,
      envirodum,
      immigrationdum,
      healthcaredum,
      housingdum,
      seniorsdum,
      leadersdum,
      ethicsdum,
      educationdum,
      crimedum,
      indigenousdum,
      welfaredum,
      electiondum,
      womendum,
      securitydum,
      quebecdum,
      racedum,
      immindracedum,
      ethleadelecdum,
      otherwelfaredum,
      complete_cat_count,
      number_of_cats,
      multcat,
    )
raw_ces2019_issues

## 2021 survey
# Main issues
raw_ces2021_issues =
  raw_ces2021_issues |>
  select(
    cps21_ResponseId,
    economydum,
    envirodum,
    immigrationdum,
    healthcaredum,
    housingdum,
    seniorsdum,
    leadersdum,
    ethicsdum,
    educationdum,
    crimedum,
    indigenousdum,
    welfaredum,
    electiondum,
    womendum,
    securitydum,
    quebecdum,
    racedum,
    coviddum,
    immindracedum,
    ethleadelecdum,
    otherwelfaredum,
    complete_cat_count,
    number_of_cats,
    multcat,
  )
raw_ces2021_issues

# Other data
raw_ces2021_web =
  raw_ces2021_web |>
  select(
    cps21_ResponseId,
    cps21_debate_en,
    cps21_debate_fr,
    cps21_debate_fr2,
    province,
    provcode,
    Q_Language,
    UserLanguage
  )
raw_ces2021_web

#### Save datasets ####
## 2015 survey
write_csv(x = raw_ces2015_combined, file = "Inputs/Data/CES/raw_ces2015_combined.csv")

## 2019 survey
write_csv(x = raw_ces2019_issues, file = "Inputs/Data/CES/raw_ces2019_issues.csv")

## 2021 survey
# Main issues
write_csv(x = raw_ces2021_issues, file = "Inputs/Data/CES/raw_ces2021_issues.csv")

# Other data
write_csv(x = raw_ces2021_web, file = "Inputs/Data/CES/raw_ces2021_web.csv")