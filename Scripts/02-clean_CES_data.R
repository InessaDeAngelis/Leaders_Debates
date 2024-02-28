#### Preamble ####
# Purpose: Cleans CES data
# Author: Inessa De Angelis
# Date: 28 February 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Read in raw datasets ####
## 2008 survey
raw_ces2008 <- read_csv("Inputs/Data/CES/raw_ces2008.csv")

## 2011 survey
raw_ces2011 <- read_csv("Inputs/Data/CES/raw_ces2011.csv")

## 2015 survey
raw_ces2015_combined <- read_csv("Inputs/Data/CES/raw_ces2015_combined.csv")

## 2019 survey
# Main issues
raw_ces2019_issues <- read_csv("Inputs/Data/CES/raw_ces2019_issues.csv")

# Other data
raw_ces2019_web <- read_csv("Inputs/Data/CES/raw_ces2019_web.csv")

## 2021 survey
# Main issues
raw_ces2021_issues <- read_csv("Inputs/Data/CES/raw_ces2021_issues.csv")

# Other data
raw_ces2021_web <- read_csv("Inputs/Data/CES/raw_ces2021_web.csv")

#### Clean datasets - get rid of NAs, rename columns, etc ####
## 2008 survey 
cleaned_ces2008 =
  raw_ces2008 |>
  drop_na("ces08_CPS_A2") |>
  rename(
    ID = ces08_IDNUM,
    important_issues = ces08_CPS_A2
  )
cleaned_ces2008

## 2011 survey
cleaned_ces2011 =
  raw_ces2011 |>
  drop_na("CPS11_1") |>
  rename(
    ID = CES11_IDNUM,
    important_issues = CPS11_1
  )
cleaned_ces2011

## 2015 survey
# Rename column 
cleaned_ces2015_combined =
raw_ces2015_combined |>
  rename(
    important_issues = main_issue
  )
cleaned_ces2015_combined

# Case match (referencing: https://ces-eec.sites.olt.ubc.ca/files/2017/04/CES2015_Combined_Data_Codebook.pdf)
cleaned_ces2015_combined =
  cleaned_ces2015_combined |>
  mutate("important_issues" = case_when(
    important_issues == 1 ~ "Other & multiple responses [not coded elsewhere]",
    important_issues == 2 ~ "Negative politics, adds, lies, etc.",
    important_issues == 4 ~ "Bill C51",
    important_issues == 5 ~ "Harper/Conservative’s contempt of parliament, others",
    important_issues == 6 ~ "Party platform / what the parties stand for (not coded elsewhere)",
    important_issues == 7 ~ "Democracy",
    important_issues == 8 ~ "Change",
    important_issues == 9 ~ "Niqab",
    important_issues == 10 ~ "Create jobs / employment",
    important_issues == 11 ~ "Senate",
    important_issues == 12 ~ "Harper/Cons negative behaviour towards parliament, civil serv, scient, etc.",
    important_issues == 13 ~ "Energy",
    important_issues == 14 ~ "Income splitting",
    important_issues == 15 ~ "Infrastructure",
    important_issues == 16 ~ "Middle class",
    important_issues == 18 ~ "Legalizing marijuana",
    important_issues == 20 ~ "General mention: debt, finances, deficit",
    important_issues == 25 ~ "Government spending, government waste",
    important_issues == 26 ~ "Balance the budget / budget",
    important_issues == 29 ~ "Cost of living, living expenses / wages",
    important_issues == 30 ~ "Economy",
    important_issues == 31 ~ "Economy & health / social programs",
    important_issues == 32 ~ "Economy & environment",
    important_issues == 33 ~ "Health care & environment",
    important_issues == 34 ~ "Immigration and refugees",
    important_issues == 35 ~ "Agriculture",
    important_issues == 36 ~ "Housing",
    important_issues == 39 ~ "Oil & gas (fuel) prices",
    important_issues == 46 ~ "Cost of election",
    important_issues == 48 ~ "Military / military spending / spending on vets",
    important_issues == 49 ~ "Arts & Culture",
    important_issues == 50 ~ "Taxation issues (includes HST)",
    important_issues == 55 ~ "Aboriginal rights / First Nations issues",
    important_issues == 56 ~ "Health and pensions for seniors",
    important_issues == 57 ~ "Health care issues",
    important_issues == 58 ~ "Health & taxes combined",
    important_issues == 59 ~ "Health & jobs combined",
    important_issues == 60 ~ "Social programs, benefits, services, welfare / health & programs",
    important_issues == 61 ~ "Seniors: pensions / retirement issues & health",
    important_issues == 62 ~ "Family benefits, childcare funding & programs, families",
    important_issues == 64 ~ "Health care & education combined",
    important_issues == 65 ~ "Educational issues, programs & funding",
    important_issues == 66 ~ "Education and employment / jobs",
    important_issues == 71 ~ "Crime / violence, gun crime, justice system",
    important_issues == 72 ~ "Poverty, low incomes, wealth distribution",
    important_issues == 73 ~ "Abortion (pro or con)",
    important_issues == 74 ~ "Rights / social justice issues: aboriginal, women, immigrants, etc.",
    important_issues == 75 ~ "Environmental / ecological issues / climate change",
    important_issues == 76 ~ "Moral issues, family values (regardless of direction)",
    important_issues == 77 ~ "Gun control / registry, Bill C68",
    important_issues == 79 ~ "Foreign affairs / national secuity / US relations",
    important_issues == 80 ~ "Quebec sovereignty / interests",
    important_issues == 82 ~ "Federal / Provincial relations, “fiscal inequality”",
    important_issues == 83 ~ "Electoral reform & procedural reform issues",
    important_issues == 84 ~ "Canada’s future, stability",
    important_issues == 90 ~ "Corruption, dishonesty / honesty",
    important_issues == 91 ~ "Ethics & effectiveness: accountability / transparency / leadership",
    important_issues == 92 ~ "Majority government (includes: to get a majority or minority)",
    important_issues == 93 ~ "Minority government",
    important_issues == 94 ~ "Defeat Conservatives / elect Liberals (NDP)",
    important_issues == 95 ~ "Defeat Liberals/ elect Conservatives (NDP)",
    important_issues == 97 ~ "None, no issue important / too many to single out",
    important_issues == 98 ~ "Don’t know / not sure / not paying attention",
    important_issues == 99 ~ "Refused",
  )) |>
  select(ID, important_issues)
  cleaned_ces2015_combined
  
## 2019 survey
# Other data: did you watch the debate question (EN/FR)
cleaned_ces2019_web = 
raw_ces2019_web |>
  select(
    cps19_ResponseId,
    cps19_debate_en,
    cps19_debate_fr,
  ) |>
  drop_na("cps19_debate_en") |>
  drop_na("cps19_debate_fr") |>
  rename(
    ID = cps19_ResponseId,
    watched_EN_debate = cps19_debate_en,
    watched_FR_debate = cps19_debate_fr,
  )
cleaned_ces2019_web

# Main issues
cleaned_ces2019_issues =
raw_ces2019_issues |>
  filter(!number_of_cats == "0") |>
  filter(!number_of_cats == "3") |>
  filter(!number_of_cats == "4") |>
  filter(!number_of_cats == "5") |>
  filter(!number_of_cats == "6") |>
  rename(
    ID = cps19_ResponseId,
    Economy = economydum,
    Environment = envirodum,
    Immigration = immigrationdum,
    Healthcare = healthcaredum,
    Housing = housingdum,
    Seniors = seniorsdum,
    Leaders = leadersdum,
    Ethics = ethicsdum,
    Education = educationdum,
    Crime = crimedum,
    Indigenous = indigenousdum,
    Welfare = welfaredum,
    Election = electiondum,
    Women = womendum,
    Security = securitydum,
    Quebec = quebecdum,
    Race = racedum,
    Immigration_and_race = immindracedum,
    Ethics_and_leaders = ethleadelecdum,
    Other_welfare = otherwelfaredum,
    Number_of_categories = number_of_cats,
  ) |>
  select(
    ID,
    Economy,
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
    Number_of_categories
  )
cleaned_ces2019_issues

## 2021 survey
# Other data: did you watch the debate question (EN/FR)
cleaned_ces2021_web = 
  raw_ces2021_web |>
  select(
    cps21_ResponseId,
    cps21_debate_en,
    cps21_debate_fr,
    cps21_debate_fr2,
  ) |>
  drop_na("cps21_debate_en") |>
  drop_na("cps21_debate_fr") |>
  drop_na("cps21_debate_fr2") |>
  rename(
    ID = cps21_ResponseId,
    watched_EN_debate = cps21_debate_en,
    watched_FR_debate = cps21_debate_fr,
    watched_FR_debate2 = cps21_debate_fr2,
  )
cleaned_ces2021_web

# Main issues
cleaned_ces2021_issues =
  raw_ces2021_issues |>
  filter(!number_of_cats == "0") |>
  filter(!number_of_cats == "3") |>
  filter(!number_of_cats == "4") |>
  filter(!number_of_cats == "5") |>
  filter(!number_of_cats == "6") |>
  rename(
    ID = cps21_ResponseId,
    Economy = economydum,
    Environment = envirodum,
    Immigration = immigrationdum,
    Healthcare = healthcaredum,
    Housing = housingdum,
    Seniors = seniorsdum,
    Leaders = leadersdum,
    Ethics = ethicsdum,
    Education = educationdum,
    Crime = crimedum,
    Indigenous = indigenousdum,
    Welfare = welfaredum,
    Election = electiondum,
    Women = womendum,
    Security = securitydum,
    Quebec = quebecdum,
    Race = racedum,
    Immigration_and_race = immindracedum,
    Ethics_and_leaders = ethleadelecdum,
    Other_welfare = otherwelfaredum,
    Covid = coviddum,
    Number_of_categories = number_of_cats,
  ) |>
  select(
    ID,
    Economy,
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
    Covid,
    Number_of_categories
  )
cleaned_ces2021_issues

#### Save cleaned datasets ####
## 2008 survey
write_csv(x = cleaned_ces2008, file = "Outputs/Data/CES/cleaned_ces2008.csv")

## 2011 survey
write_csv(x = cleaned_ces2011, file = "Outputs/Data/CES/cleaned_ces2011.csv")  

## 2015 survey
write_csv(x = cleaned_ces2015_combined, file = "Outputs/Data/CES/cleaned_ces2015_combined.csv")

## 2019 survey
# Other data
write_csv(x = cleaned_ces2019_web, file = "Outputs/Data/CES/cleaned_ces2019_web.csv")

# Main issues
write_csv(x = cleaned_ces2019_issues, file = "Outputs/Data/CES/cleaned_ces2019_issues.csv")

## 2021 survey
# Other data
write_csv(x = cleaned_ces2021_web, file = "Outputs/Data/CES/cleaned_ces2021_web.csv")

# Main issues
write_csv(x = cleaned_ces2021_issues, file = "Outputs/Data/CES/cleaned_ces2021_issues.csv")
