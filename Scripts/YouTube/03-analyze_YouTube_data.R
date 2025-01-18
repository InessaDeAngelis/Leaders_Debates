#### Preamble ####
# Purpose: Analyzes YouTube comments for Chapter 5
# Author: Inessa De Angelis
# Date: 8 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(quanteda)

#### Read in datasets ####
debate_comments_en <- read_csv("Outputs/Data/YouTube/debate_comments_en.csv")
debate_comments_fr <- read_csv("Outputs/Data/YouTube/debate_comments_fr.csv")

#### Prepare Dictionary (EN) ####
debates.lexicon <-
  dictionary(list(
    important_issues = c("issue*", "immigration", "trade","welfare","health","housing",
      "economy","justice", "platform", "platforms","crime","coalition","tax*","climate change",
      "indigenous","welfare","agricultur*","energy", "environment", "infrastructure","covid",
      "snc", "snc lavalin"),
    moderation = c("moderator", "moderation", "moderators", "faciliator", "facil*",
      "host", "Althia", "Raj", "Rosie", "Barton", "Susan", "Delacourt", "Donna","Frisen",
      "LaFlamme", "Lisa", "Paul","Wells","Paul Wells", "poorly run","poorly done"),
    format = c("format", "second*", "design"),
    production = c("stage", "podium", "audience", "requirement", "participat*"),
    won = c("won", "win", "best", "winner", "winning"),
    lost = c("worst", "least", "lost", "awful", "terrible", "nasty")))

#### Prepare corpus and run dictionary ####
## Prepare ##
debate_comments_corpus <- corpus(debate_comments_en, text_field = "Comment")

## Run dictionary ##
debates_analyzed <- tokens(debate_comments_corpus) |>
  tokens_lookup(dictionary = debates.lexicon) |>
  dfm()

## Convert to df ##
df_analyzed <- convert(debates_analyzed, to = "data.frame")

## Drop doc_id column ##
df_analyzed <- select(df_analyzed, -doc_id) 

## Add back original columns ##
df_analyzed$Debate_number <- debate_comments_en$Debate_number
df_analyzed$Comment <- debate_comments_en$Comment
df_analyzed$AuthorDisplayName <- debate_comments_en$AuthorDisplayName
df_analyzed$AuthorChannelID <- debate_comments_en$AuthorChannelID
df_analyzed$PublishedAt <- debate_comments_en$PublishedAt
df_analyzed$CommentID <- debate_comments_en$CommentID
df_analyzed$ParentID <- debate_comments_en$ParentID
df_analyzed$VideoID <- debate_comments_en$VideoID
df_analyzed$language_results <- debate_comments_en$language_results

#### Save analyzed dataset ####
write_csv(df_analyzed, "Outputs/Data/YouTube/debate_comments_en_analyzed.csv")

#### Prepare Dictionary (FR) ####
debates_fr.lexicon <-
  dictionary(list(
    important_issues = c(
      "conomie",
      "economie",
      "économie",
      "sante",
      "emploi*",
      "climat*",
      "enviro*",
      "immigrat*",
      "constitution",
      "énergie",
      "budget",
      "vacci*",
      "covid",
      "pandémie",
      "armée",
      "l’armée",
      "snc",
      "snc lavalin",
      "justice",
      "racis*",
      "démocrat*",
      "democratie",
      "fédéralisme"), 
    moderation = c(
      "modération",
      "modérat",
      "modérat*",
      "civilisé",
      "clown show",
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
    format = c("forma*"),
    production = c("participat*"),
    won = c("gagne", "gagn*", "meilleur"),
    lost = c("worst", "least", "pas gagner")))

#### Prepare corpus and run dictionary ####
## Prepare ##
debate_comments_fr_corpus <- corpus(debate_comments_fr, text_field = "Comment")

## Run dictionary ##
debates_analyzed_fr <- tokens(debate_comments_fr_corpus) |>
  tokens_lookup(dictionary = debates_fr.lexicon) |>
  dfm()

## Convert to df ##
df_analyzed_fr <- convert(debates_analyzed_fr, to = "data.frame")

## Drop doc_id column ##
df_analyzed_fr <- select(df_analyzed_fr, -doc_id) 

# Add back original columns ##
df_analyzed_fr$Debate_number <- debate_comments_fr$Debate_number
df_analyzed_fr$Comment <- debate_comments_fr$Comment
df_analyzed_fr$AuthorDisplayName <- debate_comments_fr$AuthorDisplayName
df_analyzed_fr$AuthorChannelID <- debate_comments_fr$AuthorChannelID
df_analyzed_fr$PublishedAt <- debate_comments_fr$PublishedAt
df_analyzed_fr$CommentID <- debate_comments_fr$CommentID
df_analyzed_fr$ParentID <- debate_comments_fr$ParentID
df_analyzed_fr$VideoID <- debate_comments_fr$VideoID
df_analyzed_fr$language_results <- debate_comments_fr$language_results

#### Save analyzed dataset ####
write_csv(df_analyzed_fr, "Outputs/Data/YouTube/debate_comments_fr_analyzed.csv")

#### Combine EN & FR datasets ####
all_debate_comments <- rbind(df_analyzed, df_analyzed_fr)

all_debate_comments <-
  all_debate_comments |>
  rename(comment_language = language_results)

## Save combined dataset ##
write_csv(all_debate_comments, "Outputs/Data/YouTube/all_debate_comments.csv")
