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

#### Read in datasets ####
debate_2015_commments_GM <- read_csv("Inputs/Data/YouTube/debate_2015_comments_GM.csv")
debate_2015_commments_Mac <- read_csv("Inputs/Data/YouTube/debate_2015_comments_Mac.csv")
EN_debate_2019_comments_CBC <- read_csv("Inputs/Data/YouTube/EN_debate_2019_comments_CBC.csv")
EN_debate_2019_comments_CTV <- read_csv("Inputs/Data/YouTube/EN_debate_2019_comments_CTV.csv")
FR_debate_2019_comments_CBC <- read_csv("Inputs/Data/YouTube/FR_debate_2019_comments_CBC.csv")
FR_debate_2019_comments_GN <- read_csv("Inputs/Data/YouTube/FR_debate_2019_comments_GN.csv")
EN_debate_2021_comments_CTV <- read_csv("Inputs/Data/YouTube/EN_debate_2021_comments_CTV.csv")
EN_debate_2021_comments_GN <- read_csv("Inputs/Data/YouTube/EN_debate_2021_comments_GN.csv")
FR_debate_2021_comments_GN <- read_csv("Inputs/Data/YouTube/FR_debate_2021_comments_GN.csv")
FR_debate_2021_comments_GN2 <- read_csv("Inputs/Data/YouTube/FR_debate_2021_comments_GN2.csv")

#### Clean datasets ####
## Create function to filter comments between the day of the debate and the end of the writ period & add debate number ##
process_comments <- function(df, start_date, end_date, Debate_number) {df |>
    mutate(PublishedAt = as.Date(as.POSIXct(PublishedAt)),
      Debate_number = Debate_number, .before = Comment) |>
    filter(between(PublishedAt, as.Date(start_date), as.Date(end_date)))}

## Run function ##
debate_2015_commments_GM <- process_comments(debate_2015_commments_GM, '2015-09-18', '2015-10-19', '2015Globe&Mail')
debate_2015_commments_Mac <- process_comments(debate_2015_commments_Mac, '2015-08-07', '2015-10-19', '2015Macleans')
EN_debate_2019_comments_CBC <- process_comments(EN_debate_2019_comments_CBC, '2019-10-08', '2019-10-21', '2019EnLDC')
EN_debate_2019_comments_CTV <- process_comments(EN_debate_2019_comments_CTV, '2019-10-08', '2019-10-21', '2019EnLDC')
FR_debate_2019_comments_CBC <- process_comments(FR_debate_2019_comments_CBC, '2019-10-11', '2019-10-21', '2019FrLDC')
FR_debate_2019_comments_GN <- process_comments(FR_debate_2019_comments_GN, '2019-10-11', '2019-10-21', '2019FrLDC')
EN_debate_2021_comments_CTV <- process_comments(EN_debate_2021_comments_CTV, '2021-09-10', '2021-09-20', '2021EnLDC')
EN_debate_2021_comments_GN <- process_comments(EN_debate_2021_comments_GN, '2021-09-10', '2021-09-20', '2021EnLDC')
FR_debate_2021_comments_GN <- process_comments(FR_debate_2021_comments_GN, '2021-09-09', '2021-09-20', '2021FrLDC')
FR_debate_2021_comments_GN2 <- process_comments(FR_debate_2021_comments_GN2, '2021-09-09', '2021-09-20', '2021FrLDC')

## Combine cleaned datasets into one ##
debate_comments_raw <-
  rbind(debate_2015_commments_GM,
    debate_2015_commments_Mac,
    EN_debate_2019_comments_CBC,
    EN_debate_2019_comments_CTV,
    FR_debate_2019_comments_CBC,
    FR_debate_2019_comments_GN,
    EN_debate_2021_comments_CTV,
    EN_debate_2021_comments_GN,
    FR_debate_2021_comments_GN,
    FR_debate_2021_comments_GN2)

## Detect comment language ##
# Rename column #
debate_comments_raw <- debate_comments_raw |>
  rename(text = Comment)

# Detect languages #
language_results <- detect_language(debate_comments_raw$text)

# Put language categorizations into df #
df2 <- data.frame(language_results)

# Combine language categorizations with the rest of the data #
debate_comments <- cbind(debate_comments_raw, df2)

## Create EN dataset ##
debate_comments_en <- debate_comments |>
  filter(grepl('en',language_results)) |>
  rename(Comment = text) |>
  select(-c(AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt, ReplyCount, LikeCount))
debate_comments_en

## Create FR dataset ##
debate_comments_fr <- debate_comments |>
  filter(grepl('fr',language_results)) |>
  rename(Comment = text) |>
  select(-c(AuthorProfileImageUrl, AuthorChannelUrl, UpdatedAt, ReplyCount, LikeCount))
debate_comments_fr

#### Save datasets ####
write_csv(debate_comments_en, "Outputs/Data/YouTube/debate_comments_en.csv")
write_csv(debate_comments_fr, "Outputs/Data/YouTube/debate_comments_fr.csv")
