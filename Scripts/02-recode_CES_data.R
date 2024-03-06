#### Preamble ####
# Purpose: Re-codes the re-coded CES data to match our Bastien categories
# Author: Inessa De Angelis
# Date: 5 March 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites:
  # 01-download_CES_data.R
  # 02-clean_CES_data.R

#### Workspace setup ####
library(tidyverse)

#### Read in raw datasets ####
## 2008 survey ##
cleaned_ces2008 <- read_csv("Outputs/Data/CES/cleaned_ces2008.csv")

## 2011 survey ##
cleaned_ces2011 <- read_csv("Outputs/Data/CES/cleaned_ces2011.csv")  

## 2015 survey ##
cleaned_ces2015_combined <- read_csv("Outputs/Data/CES/cleaned_ces2015_combined.csv")

## 2019 survey ##
# Re-coded important issue categories #
summarized_ces2019_issues <- read_csv("Outputs/Data/CES/summarized_ces2019_issues.csv")

## 2021 survey ##
# Re-coded important issue categories #
summarized_ces2021_issues <- read_csv("Outputs/Data/CES/summarized_ces2021_issues.csv")

#### Re-coding - 2008 ####
## Re-coding ## 
recoded_ces2008 = 
  cleaned_ces2008 |>
  select(ID, important_issues) |>
  mutate("important_issues" = case_when( 
    important_issues == "Create jobs" ~ "Economy",
    important_issues == "Economy" ~ "Economy",
    important_issues == "Economy & Environment" ~ "Economy",
    important_issues == "Economy & Health" ~ "Economy",
    important_issues == "Health care issues" ~ "Health",
    important_issues == "Health care & education combined" ~ "Health",
    important_issues == "Health care & Environment'" ~ "Health",
    important_issues == "Health & taxes combined" ~ "Health",
    important_issues == "Health & jobs combined" ~ "Health",
    important_issues == "Environmental/ecological issues" ~ "Environment",
    important_issues == "Seniors: pensions/retirement issues & health" ~ "Social Welfare",
    important_issues == "Family benefits, childcare funding & programs" ~ "Social Welfare",
    important_issues == "Social programs, services, welfare/ Health & programs" ~ "Social Welfare",
    important_issues == "Poverty as agenda issue" ~ "Social Welfare",
    important_issues == "Taxation issues" ~ "Public Finances",
    important_issues == "General mention: debt, finances" ~ "Public Finances",
    important_issues == "Balance the budget / Budget" ~ "Public Finances",
    important_issues == "Government spending, government waste" ~ "Public Finances",
    important_issues == "Educational issues, programs & funding" ~ "Education",
    important_issues == "Ethics&effectiveness: accountability/transparency/leadership" ~ "Ethics",
    important_issues == "Sponsorship issue, corruption, dishonesty /honesty" ~ "Ethics",
    important_issues == "Crime/violence, gun crime, justice system" ~ "Crime and public safety",
    important_issues == "Gun control/registry, Bill C68" ~ "Crime and public safety",
    important_issues == "Other & multiple responses [not coded elsewhere]" ~ "Other", # this was their original "other" category
    important_issues == "None, no issue important/too many to single out" ~ "Other",
    important_issues == "Defeat Conservatives / elect Liberals" ~ "Other", # this is our own "other" category 
    important_issues == "Federal/Provincial relations, 'fiscal inequality'" ~ "Other", 
    important_issues == "Foreign affairs/US relations, security issues" ~ "Other",
    important_issues == "Oil & gas (fuel) prices" ~ "Other",
    important_issues == "Agriculture" ~ "Other",
    important_issues == "Rights/Justice issues: aboriginal, women, immigrants, etc." ~ "Other",
    important_issues == "Afghan war" ~ "Other",
    important_issues == "Arts & Culture" ~ "Other",
    important_issues == "Immigration as an issue" ~ "Other",
    important_issues == "Canada Wheat Board" ~ "Other",
    important_issues == "Desire for majority government" ~ "Other",
    important_issues == "Abortion (pro or con)" ~ "Other",
    important_issues == "Quebec sovereignty / interests" ~ "Other",
    important_issues == "Election timing" ~ "Other",
    important_issues == "National unity" ~ "Other",
    important_issues == "Moral issues, family values (regardless of direction)" ~ "Other",
    important_issues == "Party platform/ what the parties stand for" ~ "Other",
    important_issues == "Desire for minority government" ~ "Other",
    important_issues == "Canada's future, stability" ~ "Other",
    important_issues == "Defeat Liberals/ elect Conservatives" ~ "Other",
    important_issues == "Electoral reform & procedural reform issues" ~ "Other",
    important_issues == "Military / military spending / defence" ~ "Other"
  )) 
recoded_ces2008

## Save dataset ##
write_csv(x = recoded_ces2008, file = "Outputs/Data/CES/recoding/recoded_ces2008.csv")

#### Re-coding - 2011 ####
## Re-coding ## 
recoded_ces2011 = 
  cleaned_ces2011 |>
  select(ID, important_issues) |>
  mutate("important_issues" = case_when( 
    important_issues == "Create jobs/employment" ~ "Economy",
    important_issues == "Economy" ~ "Economy",
    important_issues == "Economy & Environment" ~ "Economy",
    important_issues == "Economy & Health/Social programs" ~ "Economy",
    important_issues == "Cost of living, living expenses" ~ "Economy",
    important_issues == "Health care issues" ~ "Health",
    important_issues == "Health care & education combined" ~ "Health",
    important_issues == "Health care & Environment" ~ "Health",
    important_issues == "Health & taxes combined" ~ "Health",
    important_issues == "Health & jobs combined" ~ "Health",
    important_issues == "Environmental/ecological issues" ~ "Environment",
    important_issues == "Seniors: pensions/retirement issues & health" ~ "Social Welfare",
    important_issues == "Family benefits, childcare funding & programs, families" ~ "Social Welfare",
    important_issues == "Social programs, benefits, services, welfare/ Health & programs" ~ "Social Welfare",
    important_issues == "Health, pensions or seniors" ~ "Social Welfare",
    important_issues == "Poverty, low incomes" ~ "Social Welfare",
    important_issues == "Taxation issues (includes HST)" ~ "Public Finances",
    important_issues == "General mention: debt, finances, deficit" ~ "Public Finances",
    important_issues == "Balance the budget / Budget" ~ "Public Finances",
    important_issues == "Government spending, government waste" ~ "Public Finances",
    important_issues == "Educational issues, programs & funding" ~ "Education",
    important_issues == "Ethics&effectiveness: accountability/transparency/leadership" ~ "Ethics",
    important_issues == "Sponsorship issue, corruption, dishonesty /honesty" ~ "Ethics",
    important_issues == "Democracy" ~ "Ethics",
    important_issues == "Contempt of parliament" ~ "Ethics",
    important_issues == "Crime/violence, gun crime, justice system" ~ "Crime and public safety",
    important_issues == "Gun control/registry, Bill C68" ~ "Crime and public safety",
    important_issues == "Other & multiple responses [not coded elsewhere]" ~ "Other", # this was their original "other" category
    important_issues == "None, no issue important/too many to single out" ~ "Other",
    important_issues == "don't know/not sure/not paying attention" ~ "Other",
    important_issues == "Federal/Provincial relations, 'fiscal inequality'" ~ "Other", 
    important_issues == "Foreign affairs/US relations, security issues" ~ "Other",
    important_issues == "Oil & gas (fuel) prices" ~ "Other",
    important_issues == "Agriculture" ~ "Other",
    important_issues == "Rights/Justice issues: aboriginal, women, immigrants, etc." ~ "Other",
    important_issues == "Afghan war" ~ "Other",
    important_issues == "Arts & Culture" ~ "Other",
    important_issues == "Immigration" ~ "Other",
    important_issues == "Coalition" ~ "Other",
    important_issues == "Canada Wheat Board" ~ "Other",
    important_issues == "Abortion (pro or con)" ~ "Other",
    important_issues == "Quebec sovereignty / interests" ~ "Other",
    important_issues == "Election timing, cost, not needed" ~ "Other",
    important_issues == "National unity" ~ "Other",
    important_issues == "Moral issues, family values (regardless of direction)" ~ "Other",
    important_issues == "Party platform/ what the parties stand for" ~ "Other",
    important_issues == "Canada's future, stability" ~ "Other",
    important_issues == "Defeat Liberals/ elect Conservatives (NDP)" ~ "Other",
    important_issues == "Defeat Conservatives / elect Liberals (NDP)" ~ "Other", 
    important_issues == "Electoral reform & procedural reform issues" ~ "Other",
    important_issues == "Military / military spending / defence / jets" ~ "Other",
    important_issues == "Majority government (includes: to get a majority or minority)" ~ "Other",
    important_issues == "refused" ~ "Other",
    important_issues == "Minority government"  ~ "Other",
    important_issues == "Negative plitics, adds, lies, etc." ~ "Other"
  )) 
recoded_ces2011

## Save dataset ##
write_csv(x = recoded_ces2011, file = "Outputs/Data/CES/recoding/recoded_ces2011.csv")

#### Re-coding - 2015 ####
## Re-coding ## 
recoded_ces2015 = 
  cleaned_ces2015_combined |>
  select(ID, important_issues) |>
  mutate("important_issues" = case_when( 
    important_issues == "Create jobs / employment" ~ "Economy",
    important_issues == "Economy" ~ "Economy",
    important_issues == "Economy & environment" ~ "Economy",
    important_issues == "Economy & health / social programs" ~ "Economy",
    important_issues == "Cost of living, living expenses / wages" ~ "Economy",
    important_issues == "Health care issues" ~ "Health",
    important_issues == "Health care & education combined" ~ "Health",
    important_issues == "Health care & environment" ~ "Health",
    important_issues == "Health & taxes combined" ~ "Health",
    important_issues == "Health & jobs combined" ~ "Health",
    important_issues == "Environmental / ecological issues / climate change" ~ "Environment",
    important_issues == "Seniors: pensions / retirement issues & health" ~ "Social Welfare",
    important_issues == "Family benefits, childcare funding & programs, families" ~ "Social Welfare",
    important_issues == "Social programs, benefits, services, welfare / health & programs" ~ "Social Welfare",
    important_issues == "Health and pensions for seniors" ~ "Social Welfare",
    important_issues == "Poverty, low incomes, wealth distribution" ~ "Social Welfare",
    important_issues == "Middle class" ~ "Social Welfare",
    important_issues == "Taxation issues (includes HST)" ~ "Public Finances",
    important_issues == "General mention: debt, finances, deficit" ~ "Public Finances",
    important_issues == "Balance the budget / Budget" ~ "Public Finances",
    important_issues == "Government spending, government waste" ~ "Public Finances",
    important_issues == "Income splitting" ~ "Public Finances",
    important_issues == "Balance the budget / budget" ~ "Public Finances",
    important_issues == "Educational issues, programs & funding" ~ "Education",
    important_issues == "Education and employment / jobs"  ~ "Education",
    important_issues == "Ethics & effectiveness: accountability / transparency / leadership" ~ "Ethics",
    important_issues == "Democracy" ~ "Ethics",
    important_issues == "Harper/Conservative’s contempt of parliament, others" ~ "Ethics",
    important_issues == "Corruption, dishonesty / honesty"  ~ "Ethics",
    important_issues == "Crime / violence, gun crime, justice system" ~ "Crime and public safety",
    important_issues == "Gun control / registry, Bill C68" ~ "Crime and public safety",
    important_issues == "Bill C51" ~ "Crime and public safety",
    important_issues == "Immigration and refugees" ~ "Immigration",
    important_issues == "Rights / social justice issues: aboriginal, women, immigrants, etc." ~ "Justice",
    important_issues == "Abortion (pro or con)" ~ "Justice",
    important_issues == "Legalizing marijuana" ~ "Justice",
    important_issues == "Other & multiple responses [not coded elsewhere]" ~ "Other", # this was their original "other" category
    important_issues == "None, no issue important / too many to single out" ~ "Other",
    important_issues == "Don’t know / not sure / not paying attention" ~ "Other",
    important_issues == "Federal / Provincial relations, 'fiscal inequality'" ~ "Other", 
    important_issues == "Foreign affairs / national secuity / US relations" ~ "Other",
    important_issues == "Oil & gas (fuel) prices" ~ "Other",
    important_issues == "Agriculture" ~ "Other",
    important_issues == "Infrastructure" ~ "Other",
    important_issues == "Arts & Culture" ~ "Other",
    important_issues == "Aboriginal rights / First Nations issues" ~ "Other",
    important_issues == "Canada Wheat Board" ~ "Other",
    important_issues == "Quebec sovereignty / interests" ~ "Other",
    important_issues == "Election timing, cost, not needed" ~ "Other",
    important_issues == "National unity" ~ "Other",
    important_issues == "Moral issues, family values (regardless of direction)" ~ "Other",
    important_issues == "Party platform / what the parties stand for (not coded elsewhere)" ~ "Other",
    important_issues == "Canada’s future, stability" ~ "Other",
    important_issues == "Defeat Liberals/ elect Conservatives (NDP)" ~ "Other",
    important_issues == "Defeat Conservatives / elect Liberals (NDP)" ~ "Other", 
    important_issues == "Electoral reform & procedural reform issues" ~ "Other",
    important_issues == "Military / military spending / defence / jets" ~ "Other",
    important_issues == "Majority government (includes: to get a majority or minority)" ~ "Other",
    important_issues == "Refused" ~ "Other",
    important_issues == "Energy" ~ "Other",
    important_issues == "Niqab" ~ "Other",
    important_issues == "Senate" ~ "Other",
    important_issues == "Military / military spending / spending on vets" ~ "Other",
    important_issues == "Cost of election" ~ "Other",
    important_issues == "Minority government"  ~ "Other",
    important_issues == "Electoral reform & procedural reform issues" ~ "Other",
    important_issues == "Change" ~ "Other",
    important_issues == "Housing" ~ "Other",
    important_issues == "Negative politics, adds, lies, etc." ~ "Other"
  )) 
recoded_ces2015

## Save dataset ##
write_csv(x = recoded_ces2015, file = "Outputs/Data/CES/recoding/recoded_ces2015.csv")

#### Re-coding - 2019 ####
## Re-coding ## 
recoded_ces2019 = 
  summarized_ces2019_issues |>
  select(ID, important_issues) |>
  mutate("important_issues" = case_when( 
    important_issues == "Economy" ~ "Economy",
    important_issues == "Environment" ~ "Environment",
    important_issues == "Healthcare" ~ "Health",
    important_issues == "Welfare" ~ "Social Welfare",
    important_issues == "Seniors" ~ "Social Welfare",
    important_issues == "Education" ~ "Education",
    important_issues == "Ethics" ~ "Ethics",
    important_issues == "Housing" ~ "Housing",
    important_issues == "Immigration" ~ "Immigration",
    important_issues == "Crime"  ~ "Crime and public safety",
    important_issues == "Women" ~ "Justice",
    important_issues == "Race" ~ "Other",
    important_issues == "Quebec" ~ "Other",
    important_issues == "Indigenous" ~ "Other",
    important_issues == "Security" ~ "Other",
    important_issues == "Election" ~ "Other",
    important_issues == "Leaders" ~ "Other"
  ))
recoded_ces2019

## Save dataset ##
write_csv(x = recoded_ces2019, file = "Outputs/Data/CES/recoding/recoded_ces2019.csv")

#### Re-coding - 2021 ####
## Re-coding ## 
recoded_ces2021 = 
  summarized_ces2021_issues |>
  select(ID, important_issues) |>
  mutate("important_issues" = case_when( 
    important_issues == "Economy" ~ "Economy",
    important_issues == "Environment" ~ "Environment",
    important_issues == "Healthcare" ~ "Health",
    important_issues == "Welfare" ~ "Social Welfare",
    important_issues == "Seniors" ~ "Social Welfare",
    important_issues == "Education" ~ "Education",
    important_issues == "Ethics" ~ "Ethics",
    important_issues == "Housing" ~ "Housing",
    important_issues == "Covid" ~ "Covid",
    important_issues == "Immigration" ~ "Immigration",
    important_issues == "Crime"  ~ "Crime and public safety",
    important_issues == "Women" ~ "Justice",
    important_issues == "Race" ~  "Other",
    important_issues == "Quebec" ~  "Other",
    important_issues == "Indigenous" ~ "Other",
    important_issues == "Security" ~ "Other",
    important_issues == "Election" ~ "Other",
    important_issues == "Leaders" ~ "Other"
  ))
recoded_ces2021

## Save dataset ##
write_csv(x = recoded_ces2021, file = "Outputs/Data/CES/recoding/recoded_ces2021.csv")
