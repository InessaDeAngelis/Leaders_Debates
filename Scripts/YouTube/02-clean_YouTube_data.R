#### Preamble ####
# Purpose: Cleans YouTube data for analysis as part of Chapter 5
# Author: Inessa De Angelis
# Date: 29 September 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in data ####
list_of_files <- list.files(path = "Inputs/Data/YouTube/",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

debate_comments <- readr::read_csv(list_of_files, id = "Debate_number")

## Remove file path info debate number column ##
debate_comments$Debate_number <- str_remove(debate_comments$Debate_number, "Inputs/Data/YouTube//")
debate_comments$Debate_number <- str_remove(debate_comments$Debate_number, ".csv")

## Fix debate names ##
debate_comments =
  debate_comments |>
  mutate("Debate_number" = case_when(
    Debate_number == "EN_debate_2019_commments_CTV" ~ "2019EnLDC",
    Debate_number == "EN_debate_2019_commments_CBC" ~ "2019EnLDC",
    Debate_number == "FR_debate_2019_commments_GN" ~ "2019FrLDC",
    Debate_number == "FR_debate_2019_commments_CBC" ~ "2019FrLDC",
    Debate_number == "FR_debate_2021_commments_GN" ~ "2021FrLDC",
    Debate_number == "FR_debate_2021_commments" ~ "2021FrLDC",
    Debate_number == "EN_debate_2021_commments_CTV" ~ "2021EnLDC",
    Debate_number == "EN_debate_2021_commments" ~ "2021EnLDC")) 
debate_comments

## Save dataset ##
write_csv(debate_comments, "Outputs/Data/YouTube/debate_comments.csv")
