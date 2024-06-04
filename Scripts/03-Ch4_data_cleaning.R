#### Preamble ####
# Purpose: Cleaning and EDA for Chapter 4
# Author: Inessa De Angelis
# Date: 1 May 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Read in datasets ####
## By debates dataset ##
by_debates_cleaned <- read_csv(file = "Outputs/Data/by_debates_cleaned.csv")

## DQI by speech dataset ##
dqi_by_speech_cleaned <- read_csv(file = "Outputs/Data/dqi_by_speech_cleaned.csv")

#### Further clean DQI by speech dataset ####
## Remove old unique ID column before adding my own ##
dqi_by_speech_cleaned <- select(dqi_by_speech_cleaned, -UniqueID) 

## Rename columns ##
dqi_by_speech_final =
  dqi_by_speech_cleaned |>
  rename(
    Election_year = ElectionYear,
    Debate_organizer = DebateOrganizer,
    Debate_number = DebateNum,
    Speech_act_number = SpeechActNumber,
    Segment_classification = SegmentClassification,
    Number_of_Debaters_in_Segment = NumberofDebatersinSegment,
    Notes_on_segment = NotesonSegment,
    Segment_number_in_debate = SegmentNumberinDebate,
    Presence_of_demands = PresenceOfDemands,
    Respect_for_demands = RespectForDemands,
    Group_mention = GroupMention,
    Respect_for_groups = RespectforGroups,
    Speaker_incumbent_PM = IsSpeakerIncumbentPM
)
dqi_by_speech_final

## Add Unique ID column back in, starting from 1 ##
dqi_by_speech_final =
  dqi_by_speech_final |>
  mutate(ID = c(1:6995),
         .before = Language) |>
  select(
    ID,
    Language,
    Election_year,
    Debate_organizer,
    Debate_number,
    Speech_act_number,
    Speaker,
    Segment_classification,
    Number_of_Debaters_in_Segment,
    Notes_on_segment,
    Segment_number_in_debate,
    Presence_of_demands,
    Positional,
    Interruption,
    Justification,
    Respect_for_demands,
    Group_mention,
    Respect_for_groups,
    Speaker_incumbent_PM
  )
dqi_by_speech_final

#### Save dataset ####
write_csv(x = dqi_by_speech_final, file = "Inputs/Data/dqi_by_speech_final.csv")
