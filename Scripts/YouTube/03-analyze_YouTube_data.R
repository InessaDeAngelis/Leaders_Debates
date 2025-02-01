#### Preamble ####
# Purpose: Analyzes YouTube comments for Chapter 5
# Author: Inessa De Angelis
# Date: 8 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(quanteda)
library(tidytext)
library(stm)

#### Read in datasets ####
debate_comments_en <- read_csv("Outputs/Data/YouTube/debate_comments_en.csv")
debate_comments_fr <- read_csv("Outputs/Data/YouTube/debate_comments_fr.csv")

#### Prepare Dictionary (EN) ####
debates.lexicon <-
  dictionary(list(
    important_issues = c("issue*", "immigration", "trade","welfare","health","housing", "Lbtq", "EI", "CPP", "mining",
      "economy","justice", "platform", "platforms","crime","coalition","tax*","climate change", "Pipelines",
      "indigenous","welfare","agricultur*","energy", "environment", "infrastructure","covid", "ECONOMY", "First Nations",
      "snc", "snc lavalin", "job*", "carbon", "science", "foreign*", "quebec*", "racism", "lgtbq", "Global warming",
      "francophon*", "farm*", "deficit*", "corrupt*", "NAFTA", "middle class", "Keystone XL", "climate", "ingenious",
      "manufacturing", "border*", "platform*", "money", "refugee*", "military", "foreign", "international", "Greengouse", "forest",
      "unity", "defund*", "multicultural*", "Cryptocurrency", "currency", "inflation", "drugs", "Afghan*", "China", "inflation",
      "pharmacare", "universal", "policy plan", "minimum wage", "university", "surplus", "mortgage", "nuclear", "corruption", "marijuana",
      "weed", "Premier*", "rail", "airport", "childcare", "daycare", "trump", "fiscal", "huawei", "sociét*", "réchauffement"),
    moderation = c("moderator", "moderation", "moderators", "faciliator", "facil*",
      "host", "Althia", "Raj", "Rosie", "Barton", "Susan", "Delacourt", "Donna","Frisen",
      "LaFlamme", "Lisa", "Paul","Wells","Paul Wells", "poorly run","poorly done", "journalist*",
      "anchor", "Patrice", "bruno", "Dussault", "Kurl", "Shachi", "impartial", "Walmsley", "paikin",
      "Mercedes", "Solomon", "bad debate", "host's"),
    format = c("format", "second*", "design", "open-debating", "set up"),
    production = c("stage", "podium", "audience", "requirement", "participat*"),
    won = c("won", "win", "best", "winner", "winning", "worthy"),
    lost = c("worst", "least", "lost", "awful", "terrible", "nasty"),
    leaders_party = c("bernier", "green*", "singh", "jagmeet", "trudeau", "justin", "Trudeau*", "Scheer", "NDP", "PC", "LIBS", "Libs",
      "elizabeth", "prime minister", "mulcair", "harper", "andrew", "scheer", "O’Toole", "Trudumb", "erin", "Harper", "Harpo",
      "conservative", "liberal", "ppc", "vote", "bloc", "blanchet", "trudy", "turd", "max*", "People’s Party", "PPC", "Mulcair",
      "Blanchette", "erin", "Barnier", "May", "MsMay", "PM", "Truturd", "Sheer", "Blanchè", "peoples party", "mr.Singh", "jt", "CONS ervatives",
      "#PPC2019", "Trudope", "truedue", "CPC", "Conservatives", "Blanchett", "sheer", "Bernie", "Prime Minister", "Liz", "liz", "otoole",
      "Gilles Duceppe", "Duceppe", "Gilles", "Mulclair's", "May's", "max*", "Mulcair's", "Harper's", "BLOC QUÉBÉCOIS", "sheer", "party")))

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
    important_issues = c("conomie", "economie", "économie", "sante", "emploi*", "climat*", "enviro*", "immigrat*",
      "constitution", "énergie", "budget", "vacci*", "covid", "pandémie", "armée", "l’armée", "snc", "snc lavalin", 
      "justice", "racis*", "démocrat*", "democratie", "fédéralisme", "foreign*", "quebec*", "racism", "francophon*",
      "farm*", "deficit*", "corrupt*", "immigrants", "immigration", "SNC-Lavalin", "pipeline", "Anti-science", "bilingue", "Russie",
      "argent", "racistes", "institutions", "autochtone*", "province", "trump", "santé", "US", "États", "Énergie", "energie", 
      "gouvernement", "malade", "provinces", "émissions", "budget", "Québec", "fiscale", "huawei", "enfants", "Etats Unis",
      "scientifique", "social", "financière", "produit", "famille*"), 
    moderation = c("modération", "modérat","modérat*", "civilisé", "clown show", "Althia", "Raj", "Rosie", "Barton",
      "Susan", "Delacourt", "Donna", "Frisen", "LaFlamme", "Lisa", "journalist*", "roy", "Patrice", "bruno", "Dussault",
      "Kurl", "Shachi", "impartial", "Walmsley", "paikin", "Mercedes", "Solomon", "Anne-Marie", "modérateur", "biaisé"),
    format = c("forma*", "théatre", "comédie", "section", "Section", "agenda*", "formule", "Face à Face", "time", "temp*"),
    production = c("participat*", "participer"),
    won = c("gagne", "gagn*", "meilleur"),
    lost = c("worst", "least", "pas gagner"),
    leaders_party = c("bernier", "green*", "singh", "jagmeet", "trudeau", "justin", "Trudeau*", "Scheer", "NDP", "PC", "LIBS", "Libs",
                      "elizabeth", "prime minister", "mulcair", "harper", "andrew", "scheer", "O’Toole", "Trudumb", "erin", "Harper", "Harpo",
                      "conservative", "liberal", "ppc", "vote", "bloc", "blanchet", "trudy", "turd", "max*", "People’s Party", "PPC", "Mulcair",
                      "Blanchette", "erin", "Barnier", "May", "MsMay", "PM", "Truturd", "Sheer", "Blanchè", "peoples party", "mr.Singh", "jt", "CONS ervatives",
                      "#PPC2019", "Trudope", "truedue", "CPC", "Conservatives", "Blanchett", "sheer", "Bernie", "Prime Minister", "Liz", "liz",
                      "Gilles Duceppe", "Duceppe", "Gilles", "Mulclair's", "May's", "max*", "Mulcair's", "Harper's", "BLOC QUÉBÉCOIS", "Parti Québécois",
                      "parti*", "Parti*", "paul", "party", "otoole")))

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

## Re-code the coded comments so everything is on a binary ##
all_debate_comments <- all_debate_comments |>
  mutate(
    important_issues = ifelse(important_issues >= 1, 1, 0),
    moderation = ifelse(moderation >= 1, 1, 0),
    format = ifelse(format>= 1, 1, 0),
    production = ifelse(production >= 1, 1, 0),
    won = ifelse(won >= 1, 1, 0),
    lost = ifelse(lost >= 1, 1, 0),
    leaders_party = ifelse(leaders_party >= 1, 1, 0))

## Save combined dataset ##
write_csv(all_debate_comments, "Outputs/Data/YouTube/all_debate_comments.csv")

#### Create topic models to validate coding/themes ####
all_debate_comments <- read_csv("Outputs/Data/YouTube/all_debate_comments.csv")

comment_corpus <-
  corpus(all_debate_comments, 
         text_field = "Comment")
comment_corpus

toks <- tokens(comment_corpus)

# Create custom list of stop words #
comment_dfm <-
  comment_corpus |>
  tokens(
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE
  ) |>
  dfm() |>
  dfm_trim(min_termfreq = 2, min_docfreq = 2, ) |>
  dfm_remove(stopwords(source = "stopwords-iso")) 

comment_dfm

#### Make model ####
comment_topics <- stm(documents = comment_dfm, K = 20)

labelTopics(comment_topics)

write_rds(comment_topics, file = "comment_topics.rda")
