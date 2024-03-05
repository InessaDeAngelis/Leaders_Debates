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
library(janitor)

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
