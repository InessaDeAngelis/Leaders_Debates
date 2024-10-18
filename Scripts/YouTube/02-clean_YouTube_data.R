#### Preamble ####
# Purpose: Cleans YouTube data for analysis as part of Chapter 5
# Author: Inessa De Angelis
# Date: 29 September 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(cld2)

#### Read in data ####
list_of_files <- list.files(path = "Inputs/Data/YouTube/",
                            recursive = TRUE,
                            pattern = "\\.csv$",
                            full.names = TRUE)

debate_comments <- readr::read_csv(list_of_files, id = "Debate_number")

## Remove file path info debate number column ##
debate_comments$Debate_number <- str_remove(debate_comments$Debate_number, "Inputs/Data/YouTube//")
debate_comments$Debate_number <- str_remove(debate_comments$Debate_number, ".csv")

## Detect comment language ##
# Rename column #
debate_comments <- debate_comments |>
  rename(text = Comment)

# Detect languages #
language_results <- detect_language(debate_comments$text)

# Put language categorizations into df #
df2 <- data.frame(language_results)

# Combine language categorizations with the rest of the data #
debate_comments2 <- cbind(debate_comments, df2)

## ENGLISH: Drop comments in other languages, fix debate names, drop extra columns ##
debate_comments_en =
  debate_comments2 |>
  filter(grepl('en',language_results)) |>
  mutate("Outlet" = case_when(
    Debate_number == "debate_2015_commments_GM" ~ "Globe & Mail",
    Debate_number == "debate_2015_commments_Mac" ~ "Maclean's",
    Debate_number == "EN_debate_2019_commments_CTV" ~ "CTV",
    Debate_number == "EN_debate_2019_commments_CBC" ~ "CBC",
    Debate_number == "FR_debate_2019_commments_GN" ~ "Global News",
    Debate_number == "FR_debate_2019_commments_CBC" ~ "CBC",
    Debate_number == "FR_debate_2021_commments_GN" ~ "Global News",
    Debate_number == "EN_debate_2021_commments_CTV" ~ "CTV",
    Debate_number == "EN_debate_2021_commments" ~ "Global News",
    Debate_number == "FR_debate_2021_commments" ~ "Global News"), .before = text) |>
  mutate("Debate_number" = case_when(
    Debate_number == "debate_2015_commments_GM" ~ "2015Globe&Mail",
    Debate_number == "debate_2015_commments_Mac" ~ "2015Macleans",
    Debate_number == "EN_debate_2019_commments_CTV" ~ "2019EnLDC",
    Debate_number == "EN_debate_2019_commments_CBC" ~ "2019EnLDC",
    Debate_number == "FR_debate_2019_commments_GN" ~ "2019FrLDC",
    Debate_number == "FR_debate_2019_commments_CBC" ~ "2019FrLDC",
    Debate_number == "FR_debate_2021_commments_GN" ~ "2021FrLDC",
    Debate_number == "FR_debate_2021_commments" ~ "2021FrLDC",
    Debate_number == "EN_debate_2021_commments_CTV" ~ "2021EnLDC",
    Debate_number == "EN_debate_2021_commments" ~ "2021EnLDC")) |>
  rename(Comment = text) |>
  select(-c(language_results, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt, ReplyCount, LikeCount))
debate_comments_en

## Save dataset ##
write_csv(debate_comments_en, "Outputs/Data/YouTube/debate_comments.csv")

## French: Drop comments in other languages, fix debate names, drop extra columns ##
debate_comments_fr =
  debate_comments2 |>
  filter(grepl('fr',language_results)) |>
  mutate("Outlet" = case_when(
    Debate_number == "debate_2015_commments_GM" ~ "Globe & Mail",
    Debate_number == "debate_2015_commments_Mac" ~ "Maclean's",
    Debate_number == "EN_debate_2019_commments_CTV" ~ "CTV",
    Debate_number == "EN_debate_2019_commments_CBC" ~ "CBC",
    Debate_number == "FR_debate_2019_commments_GN" ~ "Global News",
    Debate_number == "FR_debate_2019_commments_CBC" ~ "CBC",
    Debate_number == "FR_debate_2021_commments_GN" ~ "Global News",
    Debate_number == "EN_debate_2021_commments_CTV" ~ "CTV",
    Debate_number == "EN_debate_2021_commments" ~ "Global News",
    Debate_number == "FR_debate_2021_commments" ~ "Global News"), .before = text) |>
  mutate("Debate_number" = case_when(
    Debate_number == "debate_2015_commments_GM" ~ "2015Globe&Mail",
    Debate_number == "debate_2015_commments_Mac" ~ "2015Macleans",
    Debate_number == "EN_debate_2019_commments_CTV" ~ "2019EnLDC",
    Debate_number == "EN_debate_2019_commments_CBC" ~ "2019EnLDC",
    Debate_number == "FR_debate_2019_commments_GN" ~ "2019FrLDC",
    Debate_number == "FR_debate_2019_commments_CBC" ~ "2019FrLDC",
    Debate_number == "FR_debate_2021_commments_GN" ~ "2021FrLDC",
    Debate_number == "FR_debate_2021_commments" ~ "2021FrLDC",
    Debate_number == "EN_debate_2021_commments_CTV" ~ "2021EnLDC",
    Debate_number == "EN_debate_2021_commments" ~ "2021EnLDC")) |>
  rename(Comment = text) |>
  select(-c(language_results, AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt, ReplyCount, LikeCount))
debate_comments_fr

## Save dataset ##
write_csv(debate_comments_fr, "Outputs/Data/YouTube/debate_comments_fr.csv")