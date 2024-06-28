#### Preamble ####
# Purpose: Cleaning and EDA for Chapter 5
# Author: Inessa De Angelis
# Date: 1 May 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Read in dataset ####
## Newspaper dataset ##
newspaper_data_cleaned <- read_csv(file = "Outputs/Data/newspaper_data_cleaned.csv")

#### Further cleaning ####
## Rename columns ##
newspaper_data_final =
  newspaper_data_cleaned |>
  rename(
    Article_title = ArticleTitle,
    Election_year = ElectionYear,
    Debate_number = DebateNum,
    Both_languages = Bothlanguages,
    EN_FR_Mult_Gen = EnFrMultGen,
    Participation_Crit = ParticipationCrit,
    Num_debates = NumDebates,
    Article_language = ArticleLanguage,
    Consortium_sentiment = ConsortiumSentiment,
    Commission_sentiment = CommissionSentiment,
    Other_org_sentiment = OtherOrganizerSentiment,
    Less_moderation = Less_Moderation,
    Other_moderation_criticism = OtherModeration_Criticism,
    Moderation_neutral = Moderation_Neutral,
    Moderation_no_mention = Moderation_Nomention,
    Total_moderator_criticism = TotalModeratorCriticism,
  )
newspaper_data_final

## Drop unneeded columns ##
# Old format/performance/effect/substance column #
newspaper_data_final <- select(newspaper_data_final, -Format_Performance_Effect_Substance) 

# Not sure what this column does #
newspaper_data_final <- select(newspaper_data_final, -"filter_$") 

# Remove old unique ID column before adding my own #
newspaper_data_final <- select(newspaper_data_final, -UniqueID) 

## Add in a new ID column ##
newspaper_data_final =
  newspaper_data_final |>
  mutate(ID = c(1:903),
         .before = Article_title) |>
  select(
    ID,
    Article_title,
    Authors,
    Publication,
    Link,
    Date,
    Year,
    Election_year,
    Debate_number,
    Fr08,
    En08,
    Fr11,
    En11,
    Macleans15,
    Globe15,
    RC15,
    Munk15,
    TVA15,
    Macleans19,
    TVA19,
    LDCen19,
    LDCfr19,
    TVA21,
    LDCfr21,
    LDCen21,
    English,
    French,
    Both_languages,
    En_Fr_Both,
    EN_FR_Mult_Gen,
    Format,
    Performance,
    Effect,
    Substance,
    Organizers,
    Participation_Crit,
    Num_debates,
    Moderation,
    Accessibility,
    Citations,
    Article_language,
    Consortium_sentiment,
    Commission_sentiment,
    Other_org_sentiment,
    More_moderation,
    Less_moderation,
    Moderation_good,
    Other_moderation_criticism,
    Moderation_neutral,
    Moderation_no_mention,
    Performance_effects,
    Total_moderator_criticism
    )
newspaper_data_final

#### Save dataset ####
write_csv(x = newspaper_data_final, file = "Outputs/Data/newspaper_data_final.csv")
