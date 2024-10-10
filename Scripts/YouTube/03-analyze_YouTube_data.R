#### Preamble ####
# Purpose: Analyzes YouTube comments for Chapter 5
# Author: Inessa De Angelis
# Date: 8 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(quanteda)
library(readtext)

#### Read in dataset ####
debate_comments <- read_csv("Outputs/Data/YouTube/debate_comments.csv")

#### Prepare Dictionary ####
debates.lexicon <-
  dictionary(list(
    moderation = c(
      "moderator",
      "moderation",
      "moderators",
      "Althia",
      "Raj",
      "Rosie",
      "Barton",
      "Susan",
      "Delacourt",
      "Donna",
      "Frisen",
      "LaFlamme",
      "Lisa"),
    format = c("format"),
    production = c("stage", "podium", "audience"),
    won = c("won*", "win*")))

