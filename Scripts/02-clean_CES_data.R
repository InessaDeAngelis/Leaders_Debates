#### Preamble ####
# Purpose: Cleans CES data
# Author: Inessa De Angelis
# Date: 28 February 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites:
  # 01-download_CES_data.R

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
    important_issues = CPS11_1,
    language = CPS_INTLANG11,
    watched_EN_debate = CPS11_76,
    watched_FR_debate = CPS11_77,
  ) |>
  select(ID, important_issues, language, watched_EN_debate, watched_FR_debate)
cleaned_ces2011

## 2015 survey
# Rename column 
cleaned_ces2015_combined =
raw_ces2015_combined |>
  drop_na("main_issue") |>
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

## Re-organize main issues ##
# Economy #
economy_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Economy) |>
  mutate("Economy" = case_when( 
    Economy == 1 ~ "Economy"
  )) |>
  rename(important_issues = Economy) |>
drop_na("important_issues") 
economy_ces2019_issues

# Environment #
environment_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Environment) |>
  mutate("Environment" = case_when( 
    Environment == 1 ~ "Environment"
  )) |>
  rename(important_issues = Environment) |>
  drop_na("important_issues") 
environment_ces2019_issues

# Immigration #
immigration_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Immigration) |>
  mutate("Immigration" = case_when( 
    Immigration == 1 ~ "Immigration"
  )) |>
  rename(important_issues = Immigration) |>
  drop_na("important_issues") 
immigration_ces2019_issues

# Healthcare #
healthcare_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Healthcare) |>
  mutate("Healthcare" = case_when( 
    Healthcare == 1 ~ "Healthcare"
  )) |>
  rename(important_issues = Healthcare) |>
  drop_na("important_issues") 
healthcare_ces2019_issues

# Housing #
housing_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Housing) |>
  mutate("Housing" = case_when( 
    Housing == 1 ~ "Housing"
  )) |>
  rename(important_issues = Housing) |>
  drop_na("important_issues") 
housing_ces2019_issues

# Seniors #
seniors_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Seniors) |>
  mutate("Seniors" = case_when( 
    Seniors == 1 ~ "Seniors"
  )) |>
  rename(important_issues = Seniors) |>
  drop_na("important_issues") 
seniors_ces2019_issues

# Leaders #
leaders_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Leaders) |>
  mutate("Leaders" = case_when( 
    Leaders == 1 ~ "Leaders"
  )) |>
  rename(important_issues = Leaders) |>
  drop_na("important_issues") 
leaders_ces2019_issues

# Ethics #
ethics_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Ethics) |>
  mutate("Ethics" = case_when( 
    Ethics == 1 ~ "Ethics"
  )) |>
  rename(important_issues = Ethics) |>
  drop_na("important_issues") 
ethics_ces2019_issues

# Education #
education_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Education) |>
  mutate("Education" = case_when( 
    Education == 1 ~ "Education"
  )) |>
  rename(important_issues = Education) |>
  drop_na("important_issues") 
education_ces2019_issues

# Crime #
crime_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Crime) |>
  mutate("Crime" = case_when( 
    Crime == 1 ~ "Crime"
  )) |>
  rename(important_issues = Crime) |>
  drop_na("important_issues") 
crime_ces2019_issues

# Indigenous #
indigenous_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Indigenous) |>
  mutate("Indigenous" = case_when( 
    Indigenous == 1 ~ "Indigenous"
  )) |>
  rename(important_issues = Indigenous) |>
  drop_na("important_issues") 
indigenous_ces2019_issues

# Welfare #
welfare_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Welfare) |>
  mutate("Welfare" = case_when( 
    Welfare == 1 ~ "Welfare"
  )) |>
  rename(important_issues = Welfare) |>
  drop_na("important_issues") 
welfare_ces2019_issues

# Election #
election_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Election) |>
  mutate("Election" = case_when( 
    Election == 1 ~ "Election"
  )) |>
  rename(important_issues = Election) |>
  drop_na("important_issues") 
election_ces2019_issues

# Women #
women_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Women) |>
  mutate("Women" = case_when( 
    Women == 1 ~ "Women"
  )) |>
  rename(important_issues = Women) |>
  drop_na("important_issues") 
women_ces2019_issues

# Security #
security_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Security) |>
  mutate("Security" = case_when( 
    Security == 1 ~ "Security"
  )) |>
  rename(important_issues = Security) |>
  drop_na("important_issues") 
security_ces2019_issues

# Quebec #
quebec_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Quebec) |>
  mutate("Quebec" = case_when( 
    Quebec == 1 ~ "Quebec"
  )) |>
  rename(important_issues = Quebec) |>
  drop_na("important_issues") 
quebec_ces2019_issues

# Race #
race_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Race) |>
  mutate("Race" = case_when( 
    Race == 1 ~ "Race"
  )) |>
  rename(important_issues = Race) |>
  drop_na("important_issues") 
race_ces2019_issues

# Combine all separate issues datasets #
summarized_ces2019_issues <-
  rbind(
    economy_ces2019_issues,
    environment_ces2019_issues,
    immigration_ces2019_issues,
    healthcare_ces2019_issues,
    housing_ces2019_issues,
    seniors_ces2019_issues,
    leaders_ces2019_issues,
    ethics_ces2019_issues,
    education_ces2019_issues,
    crime_ces2019_issues,
    indigenous_ces2019_issues,
    welfare_ces2019_issues,
    election_ces2019_issues,
    women_ces2019_issues,
    security_ces2019_issues,
    quebec_ces2019_issues,
    race_ces2019_issues
  )
summarized_ces2019_issues

## 2021 survey ##
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

## Trying something ##
# Economy #
economy_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Economy) |>
  mutate("Economy" = case_when( 
    Economy == 1 ~ "Economy"
  )) |>
  rename(important_issues = Economy) |>
  drop_na("important_issues") 
economy_ces2021_issues

# Environment #
environment_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Environment) |>
  mutate("Environment" = case_when( 
    Environment == 1 ~ "Environment"
  )) |>
  rename(important_issues = Environment) |>
  drop_na("important_issues") 
environment_ces2021_issues

# Immigration #
immigration_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Immigration) |>
  mutate("Immigration" = case_when( 
    Immigration == 1 ~ "Immigration"
  )) |>
  rename(important_issues = Immigration) |>
  drop_na("important_issues") 
immigration_ces2021_issues

# Healthcare #
healthcare_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Healthcare) |>
  mutate("Healthcare" = case_when( 
    Healthcare == 1 ~ "Healthcare"
  )) |>
  rename(important_issues = Healthcare) |>
  drop_na("important_issues") 
healthcare_ces2021_issues

# Housing #
housing_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Housing) |>
  mutate("Housing" = case_when( 
    Housing == 1 ~ "Housing"
  )) |>
  rename(important_issues = Housing) |>
  drop_na("important_issues") 
housing_ces2021_issues

# Seniors #
seniors_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Seniors) |>
  mutate("Seniors" = case_when( 
    Seniors == 1 ~ "Seniors"
  )) |>
  rename(important_issues = Seniors) |>
  drop_na("important_issues") 
seniors_ces2021_issues

# Leaders #
leaders_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Leaders) |>
  mutate("Leaders" = case_when( 
    Leaders == 1 ~ "Leaders"
  )) |>
  rename(important_issues = Leaders) |>
  drop_na("important_issues") 
leaders_ces2021_issues

# Ethics #
ethics_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Ethics) |>
  mutate("Ethics" = case_when( 
    Ethics == 1 ~ "Ethics"
  )) |>
  rename(important_issues = Ethics) |>
  drop_na("important_issues") 
ethics_ces2021_issues

# Education #
education_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Education) |>
  mutate("Education" = case_when( 
    Education == 1 ~ "Education"
  )) |>
  rename(important_issues = Education) |>
  drop_na("important_issues") 
education_ces2021_issues

# Crime #
crime_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Crime) |>
  mutate("Crime" = case_when( 
    Crime == 1 ~ "Crime"
  )) |>
  rename(important_issues = Crime) |>
  drop_na("important_issues") 
crime_ces2021_issues

# Indigenous #
indigenous_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Indigenous) |>
  mutate("Indigenous" = case_when( 
    Indigenous == 1 ~ "Indigenous"
  )) |>
  rename(important_issues = Indigenous) |>
  drop_na("important_issues") 
indigenous_ces2021_issues

# Welfare #
welfare_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Welfare) |>
  mutate("Welfare" = case_when( 
    Welfare == 1 ~ "Welfare"
  )) |>
  rename(important_issues = Welfare) |>
  drop_na("important_issues") 
welfare_ces2021_issues

# Election #
election_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Election) |>
  mutate("Election" = case_when( 
    Election == 1 ~ "Election"
  )) |>
  rename(important_issues = Election) |>
  drop_na("important_issues") 
election_ces2021_issues

# Women #
women_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Women) |>
  mutate("Women" = case_when( 
    Women == 1 ~ "Women"
  )) |>
  rename(important_issues = Women) |>
  drop_na("important_issues") 
women_ces2021_issues

# Security #
security_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Security) |>
  mutate("Security" = case_when( 
    Security == 1 ~ "Security"
  )) |>
  rename(important_issues = Security) |>
  drop_na("important_issues") 
security_ces2021_issues

# Quebec #
quebec_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Quebec) |>
  mutate("Quebec" = case_when( 
    Quebec == 1 ~ "Quebec"
  )) |>
  rename(important_issues = Quebec) |>
  drop_na("important_issues") 
quebec_ces2021_issues

# Race #
race_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Race) |>
  mutate("Race" = case_when( 
    Race == 1 ~ "Race"
  )) |>
  rename(important_issues = Race) |>
  drop_na("important_issues") 
race_ces2021_issues

# Covid #
covid_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "2") |>
  select(ID,
         Covid) |>
  mutate("Covid" = case_when( 
    Covid == 1 ~ "Covid"
  )) |>
  rename(important_issues = Covid) |>
  drop_na("important_issues") 
covid_ces2021_issues

# Combine all separate issues datasets #
summarized_ces2021_issues <-
  rbind(
    economy_ces2021_issues,
    environment_ces2021_issues,
    immigration_ces2021_issues,
    healthcare_ces2021_issues,
    housing_ces2021_issues,
    seniors_ces2021_issues,
    leaders_ces2021_issues,
    ethics_ces2021_issues,
    education_ces2021_issues,
    crime_ces2021_issues,
    indigenous_ces2021_issues,
    welfare_ces2021_issues,
    election_ces2021_issues,
    women_ces2021_issues,
    security_ces2021_issues,
    quebec_ces2021_issues,
    race_ces2021_issues,
    covid_ces2021_issues
  )
summarized_ces2021_issues

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

# Re-coded important issue categories
write_csv(x = summarized_ces2019_issues, file = "Outputs/Data/CES/summarized_ces2019_issues.csv")

## 2021 survey
# Other data
write_csv(x = cleaned_ces2021_web, file = "Outputs/Data/CES/cleaned_ces2021_web.csv")

# Main issues
write_csv(x = cleaned_ces2021_issues, file = "Outputs/Data/CES/cleaned_ces2021_issues.csv")

# Re-coded important issue categories
write_csv(x = summarized_ces2021_issues, file = "Outputs/Data/CES/summarized_ces2021_issues.csv")

#### OLD code to created combined most important issue categories for the 2019 & 2021 CES ####
## Create dataset of combined most important issue categories (2019) ##
# Economy & Environment
econ_enviro_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Environment == "1") |>
  select(ID, Economy, Environment)
econ_enviro_ces2019_issues

# ifelse code referenced from: https://stackoverflow.com/questions/39405628/how-do-i-create-a-new-column-based-on-multiple-conditions-from-multiple-columns
econ_enviro_ces2019_issues$important_issues <- ifelse(
  ( 
    (econ_enviro_ces2019_issues$Environment %in% c("1")) &
      (econ_enviro_ces2019_issues$Economy == "1")
  ),
  "Economy & environment",  # if condition is met, put E&E 
  0   # else put 0
) 
econ_enviro_ces2019_issues 

econ_enviro_ces2019_issues2 =
  econ_enviro_ces2019_issues |>
  select(ID, important_issues)
econ_enviro_ces2019_issues2

# Economy & Healthcare
econ_health_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Healthcare == "1") |>
  select(ID, Economy, Healthcare)
econ_health_ces2019_issues

econ_health_ces2019_issues$important_issues <- ifelse(
  ( 
    (econ_health_ces2019_issues$Economy %in% c("1")) &
      (econ_health_ces2019_issues$Healthcare == "1")
  ),
  "Economy & healthcare",  # if condition is met, put E&H 
  0   # else put 0
) 
econ_health_ces2019_issues 

econ_health_ces2019_issues2 =
  econ_health_ces2019_issues  |>
  select(ID, important_issues)
econ_health_ces2019_issues2

# Economy & Housing
econ_housing_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Housing == "1") |>
  select(ID, Economy, Housing)
econ_housing_ces2019_issues

econ_housing_ces2019_issues$important_issues <- ifelse(
  ( 
    (econ_housing_ces2019_issues$Economy %in% c("1")) &
      (econ_housing_ces2019_issues$Housing == "1")
  ),
  "Economy & housing",  # if condition is met, put E&H 
  0   # else put 0
) 
econ_housing_ces2019_issues

econ_housing_ces2019_issues2 =
  econ_housing_ces2019_issues  |>
  select(ID, important_issues)
econ_housing_ces2019_issues2

# Economy & welfare
econ_welfare_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Welfare == "1") |>
  select(ID, Economy, Welfare)
econ_welfare_ces2019_issues

econ_welfare_ces2019_issues$important_issues <- ifelse(
  ( 
    (econ_welfare_ces2019_issues$Economy %in% c("1")) &
      (econ_welfare_ces2019_issues$Welfare == "1")
  ),
  "Economy & welfare",  # if condition is met, put E&W
  0   # else put 0
) 
econ_welfare_ces2019_issues

econ_welfare_ces2019_issues2 =
  econ_welfare_ces2019_issues |>
  select(ID, important_issues)
econ_welfare_ces2019_issues2

# Ethics & leaders
ethics_leaders_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Ethics == "1") |>
  filter(Leaders == "1") |>
  select(ID, Ethics, Leaders)
ethics_leaders_ces2019_issues

ethics_leaders_ces2019_issues$important_issues <- ifelse(
  ( 
    (ethics_leaders_ces2019_issues$Ethics %in% c("1")) &
      (ethics_leaders_ces2019_issues$Leaders == "1")
  ),
  "Ethics & leaders",  # if condition is met, put E&L
  0   # else put 0
) 
ethics_leaders_ces2019_issues

ethics_leaders_ces2019_issues2 =
  ethics_leaders_ces2019_issues |>
  select(ID, important_issues)
ethics_leaders_ces2019_issues2

# Healthcare & education
health_edu_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Healthcare == "1") |>
  filter(Education == "1") |>
  select(ID, Healthcare, Education)
health_edu_ces2019_issues

health_edu_ces2019_issues$important_issues <- ifelse(
  ( 
    (health_edu_ces2019_issues$Healthcare %in% c("1")) &
      (health_edu_ces2019_issues$Education == "1")
  ),
  "Healthcare & education",  # if condition is met, put H&E
  0   # else put 0
) 
health_edu_ces2019_issues

health_edu_ces2019_issues2 =
  health_edu_ces2019_issues |>
  select(ID, important_issues)
health_edu_ces2019_issues2

# Healthcare & seniors
health_seniors_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Healthcare == "1") |>
  filter(Seniors == "1") |>
  select(ID, Healthcare, Seniors)
health_seniors_ces2019_issues

health_seniors_ces2019_issues$important_issues <- ifelse(
  ( 
    (health_seniors_ces2019_issues$Healthcare %in% c("1")) &
      (health_seniors_ces2019_issues$Seniors == "1")
  ),
  "Healthcare & seniors",  # if condition is met, put H&S
  0   # else put 0
) 
health_seniors_ces2019_issues

health_seniors_ces2019_issues2 =
  health_seniors_ces2019_issues |>
  select(ID, important_issues)
health_seniors_ces2019_issues2

# Healthcare & environment
health_enviro_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Healthcare == "1") |>
  filter(Environment == "1") |>
  select(ID, Healthcare, Environment)
health_enviro_ces2019_issues

health_enviro_ces2019_issues$important_issues <- ifelse(
  ( 
    (health_enviro_ces2019_issues$Healthcare %in% c("1")) &
      (health_enviro_ces2019_issues$Environment == "1")
  ),
  "Healthcare & environment",  # if condition is met, put H&E
  0   # else put 0
) 
health_enviro_ces2019_issues

health_enviro_ces2019_issues2 =
  health_enviro_ces2019_issues |>
  select(ID, important_issues)
health_enviro_ces2019_issues2

# Welfare & seniors
welfare_seniors_ces2019_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Welfare == "1") |>
  filter(Seniors == "1") |>
  select(ID, Welfare, Seniors)
welfare_seniors_ces2019_issues

welfare_seniors_ces2019_issues$important_issues <- ifelse(
  ( 
    (welfare_seniors_ces2019_issues$Welfare %in% c("1")) &
      (welfare_seniors_ces2019_issues$Seniors == "1")
  ),
  "Welfare & seniors",  # if condition is met, put W&S
  0   # else put 0
) 
welfare_seniors_ces2019_issues

welfare_seniors_ces2019_issues2 =
  welfare_seniors_ces2019_issues |>
  select(ID, important_issues)
welfare_seniors_ces2019_issues2

# Combine all separate issues datasets #
summarized_ces2019_issues2 <-
  rbind(
    economy_ces2019_issues,
    environment_ces2019_issues,
    immigration_ces2019_issues,
    healthcare_ces2019_issues,
    housing_ces2019_issues,
    seniors_ces2019_issues,
    leaders_ces2019_issues,
    ethics_ces2019_issues,
    education_ces2019_issues,
    crime_ces2019_issues,
    indigenous_ces2019_issues,
    welfare_ces2019_issues,
    election_ces2019_issues,
    women_ces2019_issues,
    security_ces2019_issues,
    quebec_ces2019_issues,
    race_ces2019_issues,
    econ_enviro_ces2019_issues2,
    econ_health_ces2019_issues2,
    econ_housing_ces2019_issues2,
    econ_welfare_ces2019_issues2,
    ethics_leaders_ces2019_issues2,
    health_edu_ces2019_issues2,
    health_seniors_ces2019_issues2,
    health_enviro_ces2019_issues2,
    welfare_seniors_ces2019_issues2
  )
summarized_ces2019_issues2

## Create dataset of combined most important issue categories (2021) ##
# Economy & Environment
econ_enviro_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Environment == "1") |>
  select(ID, Economy, Environment)
econ_enviro_ces2021_issues

econ_enviro_ces2021_issues$important_issues <- ifelse(
  ( 
    (econ_enviro_ces2021_issues$Environment %in% c("1")) &
      (econ_enviro_ces2021_issues$Economy == "1")
  ),
  "Economy & environment",  # if condition is met, put E&E 
  0   # else put 0
) 
econ_enviro_ces2021_issues 

econ_enviro_ces2021_issues2 =
  econ_enviro_ces2021_issues |>
  select(ID, important_issues)
econ_enviro_ces2021_issues2

# Economy & Healthcare
econ_health_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Healthcare == "1") |>
  select(ID, Economy, Healthcare)
econ_health_ces2021_issues

econ_health_ces2021_issues$important_issues <- ifelse(
  ( 
    (econ_health_ces2021_issues$Economy %in% c("1")) &
      (econ_health_ces2021_issues$Healthcare == "1")
  ),
  "Economy & healthcare",  # if condition is met, put E&H 
  0   # else put 0
) 
econ_health_ces2021_issues 

econ_health_ces2021_issues2 =
  econ_health_ces2021_issues  |>
  select(ID, important_issues)
econ_health_ces2021_issues2

# Economy & Housing
econ_housing_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Housing == "1") |>
  select(ID, Economy, Housing)
econ_housing_ces2021_issues

econ_housing_ces2021_issues$important_issues <- ifelse(
  ( 
    (econ_housing_ces2021_issues$Economy %in% c("1")) &
      (econ_housing_ces2021_issues$Housing == "1")
  ),
  "Economy & housing",  # if condition is met, put E&H 
  0   # else put 0
) 
econ_housing_ces2021_issues

econ_housing_ces2021_issues2 =
  econ_housing_ces2021_issues  |>
  select(ID, important_issues)
econ_housing_ces2021_issues2

# Economy & welfare
econ_welfare_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Economy == "1") |>
  filter(Welfare == "1") |>
  select(ID, Economy, Welfare)
econ_welfare_ces2021_issues

econ_welfare_ces2021_issues$important_issues <- ifelse(
  ( 
    (econ_welfare_ces2021_issues$Economy %in% c("1")) &
      (econ_welfare_ces2021_issues$Welfare == "1")
  ),
  "Economy & welfare",  # if condition is met, put E&W
  0   # else put 0
) 
econ_welfare_ces2021_issues

econ_welfare_ces2021_issues2 =
  econ_welfare_ces2021_issues |>
  select(ID, important_issues)
econ_welfare_ces2021_issues2

# Ethics & leaders
ethics_leaders_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Ethics == "1") |>
  filter(Leaders == "1") |>
  select(ID, Ethics, Leaders)
ethics_leaders_ces2021_issues

# ifelse code referenced from: https://stackoverflow.com/questions/39405628/how-do-i-create-a-new-column-based-on-multiple-conditions-from-multiple-columns
ethics_leaders_ces2021_issues$important_issues <- ifelse(
  ( 
    (ethics_leaders_ces2021_issues$Ethics %in% c("1")) &
      (ethics_leaders_ces2021_issues$Leaders == "1")
  ),
  "Ethics & leaders",  # if condition is met, put E&L
  0   # else put 0
) 
ethics_leaders_ces2021_issues

ethics_leaders_ces2021_issues2 =
  ethics_leaders_ces2021_issues |>
  select(ID, important_issues)
ethics_leaders_ces2021_issues2

# Healthcare & education
health_edu_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Healthcare == "1") |>
  filter(Education == "1") |>
  select(ID, Healthcare, Education)
health_edu_ces2021_issues

health_edu_ces2021_issues$important_issues <- ifelse(
  ( 
    (health_edu_ces2021_issues$Healthcare %in% c("1")) &
      (health_edu_ces2021_issues$Education == "1")
  ),
  "Healthcare & education",  # if condition is met, put H&E
  0   # else put 0
) 
health_edu_ces2021_issues

health_edu_ces2021_issues2 =
  health_edu_ces2021_issues |>
  select(ID, important_issues)
health_edu_ces2021_issues2

# Healthcare & seniors
health_seniors_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Healthcare == "1") |>
  filter(Seniors == "1") |>
  select(ID, Healthcare, Seniors)
health_seniors_ces2021_issues

health_seniors_ces2021_issues$important_issues <- ifelse(
  ( 
    (health_seniors_ces2021_issues$Healthcare %in% c("1")) &
      (health_seniors_ces2021_issues$Seniors == "1")
  ),
  "Healthcare & seniors",  # if condition is met, put H&S
  0   # else put 0
) 
health_seniors_ces2021_issues

health_seniors_ces2021_issues2 =
  health_seniors_ces2021_issues |>
  select(ID, important_issues)
health_seniors_ces2021_issues2

# Healthcare & environment
health_enviro_ces2021_issues =
  cleaned_ces2021_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Healthcare == "1") |>
  filter(Environment == "1") |>
  select(ID, Healthcare, Environment)
health_enviro_ces2021_issues

health_enviro_ces2021_issues$important_issues <- ifelse(
  ( 
    (health_enviro_ces2021_issues$Healthcare %in% c("1")) &
      (health_enviro_ces2021_issues$Environment == "1")
  ),
  "Healthcare & environment",  # if condition is met, put H&E
  0   # else put 0
) 
health_enviro_ces2021_issues

health_enviro_ces2021_issues2 =
  health_enviro_ces2021_issues |>
  select(ID, important_issues)
health_enviro_ces2021_issues2

# Welfare & seniors
welfare_seniors_ces2021_issues =
  cleaned_ces2019_issues |>
  filter(!Number_of_categories == "1") |>
  filter(Welfare == "1") |>
  filter(Seniors == "1") |>
  select(ID, Welfare, Seniors)
welfare_seniors_ces2021_issues

welfare_seniors_ces2021_issues$important_issues <- ifelse(
  ( 
    (welfare_seniors_ces2021_issues$Welfare %in% c("1")) &
      (welfare_seniors_ces2021_issues$Seniors == "1")
  ),
  "Welfare & seniors",  # if condition is met, put W&S
  0   # else put 0
) 
welfare_seniors_ces2021_issues

welfare_seniors_ces2021_issues2 =
  welfare_seniors_ces2021_issues |>
  select(ID, important_issues)
welfare_seniors_ces2021_issues2

# Combine all separate issues datasets #
summarized_ces2021_issues2 <-
  rbind(
    economy_ces2021_issues,
    environment_ces2021_issues,
    immigration_ces2021_issues,
    healthcare_ces2021_issues,
    housing_ces2021_issues,
    seniors_ces2021_issues,
    leaders_ces2021_issues,
    ethics_ces2021_issues,
    education_ces2021_issues,
    crime_ces2021_issues,
    indigenous_ces2021_issues,
    welfare_ces2021_issues,
    election_ces2021_issues,
    women_ces2021_issues,
    security_ces2021_issues,
    quebec_ces2021_issues,
    race_ces2021_issues,
    covid_ces2021_issues,
    econ_enviro_ces2021_issues2,
    econ_health_ces2021_issues2,
    econ_housing_ces2021_issues2,
    econ_welfare_ces2021_issues2,
    ethics_leaders_ces2021_issues2,
    health_edu_ces2021_issues2,
    health_seniors_ces2021_issues2,
    health_enviro_ces2021_issues2,
    welfare_seniors_ces2021_issues2
  )
summarized_ces2021_issues2