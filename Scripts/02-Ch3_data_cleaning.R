#### Preamble ####
# Purpose: Further cleaning for Chapter 3
# Author: Inessa De Angelis
# Date: 24 July 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in dataset ####
debate_questions_cleaned <- read_csv("Outputs/Data/debate_questions_cleaned.csv")

#### Re-name ID column ####
debate_questions_final <- debate_questions_cleaned |>
  rename(
    ID = unique_id,
    Year = year,
    Organizer = organizer,
    Debate_number = debate_number,
    Language_of_question = language_of_question,
    Theme = theme,
    Question_text = question_text,
    Question_source = question_source,
    Questioner_id = questioner_id,
    Lead_followup = lead_followup,
    Posed_to_number = posed_to_number,
    Posed_to_who = posed_to_who,
    Content_of_question = content_of_question,
    Primary_issue = primary_issue,
    Secondary_issue = secondary_issue,
    Other_issue_text = other_issue_text,
    Territory_1 = territory1,
    Territory_2 = territory2,
    Coder = coder,
    Incumbent = incumbent,
    Debate_lang = debate_lang)
debate_questions_final

#### Drop old "filter" column ####
debate_questions_final <- select(debate_questions_final, -"filter") 

#### Re-code "debate_number" for consistency across datasets ####
debate_questions_final <- debate_questions_final |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008_Consortium_en" ~ "2008EnConsortium",
    Debate_number == "2008_Consortium_Fr" ~ "2008FrConsortium",
    Debate_number == "2011_Consortium_en" ~ "2011EnConsortium",
    Debate_number == "2011_Consortium_Fr" ~ "2011FrConsortium",
    Debate_number == "2015_Macleans" ~ "2015Macleans",    
    Debate_number == "2015_Globe&Mail" ~ "2015Globe&Mail",
    Debate_number == "2015_Radio-Canada" ~ "2015FrConsortium",
    Debate_number == "2015_Munk" ~ "2015Munk",
    Debate_number == "2015_TVA" ~ "2015TVA",
    Debate_number == "2019_Macleans" ~ "2019Macleans",
    Debate_number == "2019_TVA" ~ "2019TVA",
    Debate_number == "2019_LDC_en" ~ "2019EnLDC",
    Debate_number == "2019_LDC_fr" ~ "2019FrLDC",
    Debate_number == "2021_TVA" ~ "2021TVA",
    Debate_number == "2021_LDC_fr" ~ "2021FrLDC",
    Debate_number == "2021_LDC_en" ~ "2021EnLDC")) 
debate_questions_final

#### Update "debate_organizer" for 2015 Consortium debate ####
debate_questions_final <- debate_questions_final |>
  mutate("Organizer" = case_when(
  Organizer == "Radio_Canada" ~ "Consortium", 
  TRUE ~ Organizer))
debate_questions_final

#### Fix spelling of names in "Questioner_id" column ####
debate_questions_final <- debate_questions_final |>
  mutate("Questioner_id" = case_when(
    Questioner_id == "Soloman" ~ "Solomon", 
    TRUE ~ Questioner_id))
debate_questions_final   

#### Save dataset ####
write_csv(debate_questions_final, "Outputs/Data/debate_questions_final.csv")

#### Re-code issues: public finance -> economy #### 
debate_questions_final_recoded = 
  debate_questions_final|>
  mutate("Primary_issue" = case_when( 
    Primary_issue == "Public finance" ~ "Economy", 
    TRUE ~ Primary_issue)) |>
  mutate("Secondary_issue" = case_when( 
    Secondary_issue == "Public finance" ~ "Economy", 
    TRUE ~ Secondary_issue))
debate_questions_final_recoded

#### Save re-coded dataset ####
write_csv(debate_questions_final_recoded, "Outputs/Data/debate_questions_final_recoded.csv")
