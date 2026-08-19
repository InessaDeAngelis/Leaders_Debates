#### Preamble ####
# Purpose: Construct and analyze speaking time dataset for Ch4
# Author: Inessa De Angelis
# Date: 19 August 2026
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Construct dataset (from Appendix B in the manuscript) ####
speaking_time <- data.frame(
  Debate_number = c(
    "2008FrConsortium", "2008EnConsortium",
    "2011EnConsortium", "2011FrConsortium",
    "2015Macleans", "2015Globe&Mail", "2015FrConsortium", "2015Munk", "2015TVA",
    "2019Macleans", "2019TVA", "2019EnLDC", "2019FrLDC",
    "2021TVA", "2021FrLDC", "2021EnLDC"),
  Participants = c(
    "Harper, Dion, Layton, May, Duceppe",
    "Harper, Dion, Layton, May, Duceppe",
    "Harper, Ignatieff, Layton, Duceppe",
    "Harper, Ignatieff, Layton, Duceppe",
    "Trudeau, Harper, Mulcair, May, Duceppe",
    "Trudeau, Harper, Mulcair",
    "Trudeau, Harper, Mulcair, May, Duceppe",
    "Trudeau, Harper, Mulcair",
    "Trudeau, Harper, Mulcair, Duceppe",
    "Scheer, Singh, May",
    "Trudeau, Scheer, Singh, Blanchet",
    "Trudeau, Scheer, Singh, May, Blanchet, Bernier",
    "Trudeau, Scheer, Singh, May, Blanchet, Bernier",
    "Trudeau, O'Toole, Singh, Blanchet",
    "Trudeau, O'Toole, Singh, Paul, Blanchet",
    "Trudeau, O'Toole, Singh, Paul, Blanchet"),
  Non_leader_participation = c(
    16.7, 8.3,
    8.2, 14.6,
    14.3, 14.0, 16.4, 10.8, 13.9,
    17.52, 14.0, 15.4, 20.8,
    18.1, 30.7, 27.7),
  Justin_Trudeau = c(
    NA, NA,
    NA, NA,
    21.9, 28.4, 18.5, 35.0, 20.8,
    NA, 21.7, 15.8, 14.6,
    24.6, 16.8, 16.0),
  Stephen_Harper = c(
    16.1, 23.4,
    33.1, 18.7,
    25.5, 29.2, 16.5, 28.3, 19.2,
    NA, NA, NA, NA,
    NA, NA, NA),
  Thomas_Mulcair = c(
    NA, NA,
    NA, NA,
    21.2, 28.5, 20.9, 26.0, 24.9,
    NA, NA, NA, NA,
    NA, NA, NA),
  Elizabeth_May = c(
    10.8, 17.7,
    NA, NA,
    17.2, NA, 9.5, NA, NA,
    29.8, NA, 15.0, 10.1,
    NA, NA, NA),
  Gilles_Duceppe = c(
    19.3, 11.8,
    14.6, 28.9,
    NA, NA, 18.1, NA, 21.2,
    NA, NA, NA, NA,
    NA, NA, NA),
  Stephane_Dion = c(
    22.8, 19.9,
    NA, NA,
    NA, NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA),
  Michael_Ignatieff = c(
    NA, NA,
    21.4, 20.2,
    NA, NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA),
  Jack_Layton = c(
    12.1, 19.0,
    22.6, 17.6,
    NA, NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, NA, NA),
  Annamie_Paul = c(
    NA, NA,
    NA, NA,
    NA, NA, NA, NA, NA,
    NA, NA, NA, NA,
    NA, 11.5, 14.9),
  Erin_OToole = c(
    NA, NA,
    NA, NA,
    NA, NA, NA, NA, NA,
    NA, NA, NA, NA,
    16.4, 11.5, 14.6),
  Jagmeet_Singh = c(
    NA, NA,
    NA, NA,
    NA, NA, NA, NA, NA,
    24.4, 18.0, 16.2, 13.4,
    17.2, 13.2, 15.6),
  Andrew_Scheer = c(
    NA, NA,
    NA, NA,
    NA, NA, NA, NA, NA,
    28.6, 20.5, 17.5, 12.5,
    NA, NA, NA),
  Yves_Francois_Blanchet = c(
    NA, NA,
    NA, NA,
    NA, NA, NA, NA, NA,
    NA, 25.7, 9.3, 15.3,
    23.8, 16.3, 11.2),
  Maxime_Bernier = c(
    NA, NA,
    NA, NA,
    NA, NA, NA, NA, NA,
    NA, NA, 10.8, 13.2,
    NA, NA, NA))

## Pivot dataset ##
speaking_long <- speaking_time |>
  pivot_longer(
    cols = Justin_Trudeau:Maxime_Bernier,
    names_to = "leader",
    values_to = "speaking_share")

speaking_long$leader <- gsub("_", " ", speaking_long$leader)

## Remove leaders that didn't participate in a given debate ##
speaking_long <- speaking_long |>
  filter(!is.na(speaking_share))

## Add gender and ethnicity variables ##
speaking_long <- speaking_long |>
  mutate(
    Gender = case_when(
      leader == "Non-leader" ~ NA_character_,
      leader %in% c("Elizabeth May", "Annamie Paul") ~ "Woman",
      TRUE ~ "Man"),
    Ethnicity = case_when(
      leader == "Non-leader" ~ NA_character_,
      leader %in% c("Jagmeet Singh", "Annamie Paul") ~ "Racialized",
      TRUE ~ "White"))

## Save CSV ##
write_csv(speaking_long, "Outputs/Data/speaking_time.csv")

## Test: Check that each debate has the correct number of leaders ##
speaking_long |>
  group_by(Debate_number) |>
  summarise(n_leaders = n())

#### Analyzing speaking time ####
## By gender ##
speaking_long |>
  group_by(Gender) |>
  summarise(
    N = n(),
    Mean = mean(speaking_share),
    SD = sd(speaking_share),
    Median = median(speaking_share),
    Min = min(speaking_share),
    Max = max(speaking_share))

## By ethnicity ##
speaking_long |>
  group_by(Ethnicity) |>
  summarise(
    N = n(),
    Mean = mean(speaking_share),
    SD = sd(speaking_share),
    Median = median(speaking_share),
    Min = min(speaking_share),
    Max = max(speaking_share))

## Check by individual leader ##
leader_summary <- speaking_long |>
  group_by(leader, Gender, Ethnicity) |>
  summarise(
    N_debates = n(),
    Mean_speaking_share = mean(speaking_share, na.rm = TRUE),
    SD_speaking_share = sd(speaking_share, na.rm = TRUE),
    Median_speaking_share = median(speaking_share, na.rm = TRUE),
    Min_speaking_share = min(speaking_share, na.rm = TRUE),
    Max_speaking_share = max(speaking_share, na.rm = TRUE),
    .groups = "drop")
