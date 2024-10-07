#### Preamble ####
# Purpose: Re-code public finance + economy debate questions data for Chapter 3
# Author: Inessa De Angelis
# Date: 12 August 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in dataset ####
debate_questions_final <- read_csv("Outputs/Data/debate_questions_final.csv")

#### Re-coding #### 
debate_questions_final_recoded = 
  debate_questions_final|>
  mutate("Primary_issue" = case_when( 
    Primary_issue == "Public finance" ~ "Economy", # THIS IS IT
    Primary_issue == "Indigenous" ~ "Indigenous",
    Primary_issue == "Women" ~ "Women",
    Primary_issue == "Crime and public safety" ~ "Crime and public safety",
    Primary_issue == "Culture and heritage" ~ "Culture and heritage", 
    Primary_issue == "Economy" ~ "Economy",
    Primary_issue == "Environment" ~ "Environment",
    Primary_issue == "Other" ~ "Other",
    Primary_issue == "Immigration" ~ "Immigration",
    Primary_issue == "Government formation" ~ "Government formation",
    Primary_issue == "Health care" ~ "Health care",
    Primary_issue == "Defence" ~ "Defence",
    Primary_issue == "Foreign affairs" ~ "Foreign affairs",
    Primary_issue == "Constitution" ~ "Constitution", 
    Primary_issue == "Energy" ~ "Energy",
    Primary_issue == "Ethics" ~ "Ethics", 
    Primary_issue == "Housing" ~ "Housing",
    Primary_issue == "Transport" ~ "Transport",
    Primary_issue == "Justice" ~ "Justice",
    Primary_issue == "Agriculture" ~ "Agriculture", 
    Primary_issue == "Official Languages" ~ "Official Languages",
    Primary_issue == "Federalism" ~ "Federalism",
    Primary_issue == "Covid" ~ "Covid",
    Primary_issue == "Social welfare" ~ "Social welfare")) |>
mutate("Secondary_issue" = case_when( 
  Secondary_issue == "Public finance" ~ "Economy", # THIS IS IT
  Secondary_issue == "Indigenous" ~ "Indigenous",
  Secondary_issue == "Women" ~ "Women",
  Secondary_issue == "Crime and public safety" ~ "Crime and public safety",
  Secondary_issue == "Culture and heritage" ~ "Culture and heritage", 
  Secondary_issue == "Economy" ~ "Economy",
  Secondary_issue == "Environment" ~ "Environment",
  Secondary_issue == "Other" ~ "Other",
  Secondary_issue == "Immigration" ~ "Immigration",
  Secondary_issue == "Government formation" ~ "Government formation",
  Secondary_issue == "Health care" ~ "Health care",
  Secondary_issue == "Defence" ~ "Defence",
  Secondary_issue == "Foreign affairs" ~ "Foreign affairs",
  Secondary_issue == "Constitution" ~ "Constitution", 
  Secondary_issue == "Energy" ~ "Energy",
  Secondary_issue == "Ethics" ~ "Ethics", 
  Secondary_issue == "Housing" ~ "Housing",
  Secondary_issue == "Transport" ~ "Transport",
  Secondary_issue == "Justice" ~ "Justice",
  Secondary_issue == "Agriculture" ~ "Agriculture", 
  Secondary_issue == "Official Languages" ~ "Official Languages",
  Secondary_issue == "Federalism" ~ "Federalism",
  Secondary_issue == "Covid" ~ "Covid",
  Secondary_issue == "Social welfare"~ "Social welfare"))
debate_questions_final_recoded

#### Save re-coded dataset ####
write_csv(debate_questions_final_recoded, file = "Outputs/Data/debate_questions_final_recoded.csv")