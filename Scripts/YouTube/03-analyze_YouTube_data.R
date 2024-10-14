#### Preamble ####
# Purpose: Analyzes YouTube comments for Chapter 5
# Author: Inessa De Angelis
# Date: 8 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(quanteda)

#### Read in dataset ####
debate_comments <- read_csv("Outputs/Data/YouTube/debate_comments.csv")

#### Prepare Dictionary ####
# Note: add a DQI column?

debates.lexicon <-
  dictionary(list(
    important_issues = c(
      "issue*",
      "immigration",
      "health",
      "housing",
      "economy",
      "platform*",
      "tax*",
      "climate change",
      "Indigenous",
      "welfare",
      "agricultur*",
      "energy",
      "infrastructure"), 
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
    format = c("format", "second*", "design"),
    production = c("stage", "podium", "audience", "requirement", "participat*"),
    won = c("won*", "win*", "best")))

#### Prepare corpus and run dictionary ####
## Prepare ##
debate_comments_corpus <-
  corpus(debate_comments, 
         text_field = "Comment")
debate_comments_corpus

## Run dictionary ##
debates_analyzed <- tokens(debate_comments_corpus) |>
  tokens_lookup(dictionary = debates.lexicon) |>
  dfm()

## Convert to df ##
df_analyzed <- convert(debates_analyzed, to = "data.frame")

## Drop doc_id column ##
df_analyzed <- select(df_analyzed, -doc_id) 

# Add back original columns ##
df_analyzed$Debate_number <- debate_comments$Debate_number
df_analyzed$Comment <- debate_comments$Comment
df_analyzed$AuthorDisplayName <- debate_comments$AuthorDisplayName
df_analyzed$AuthorChannelID <- debate_comments$AuthorChannelID
df_analyzed$PublishedAt <- debate_comments$PublishedAt
df_analyzed$CommentID <- debate_comments$CommentID
df_analyzed$ParentID <- debate_comments$ParentID
df_analyzed$VideoID <- debate_comments$VideoID
