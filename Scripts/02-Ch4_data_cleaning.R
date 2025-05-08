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

#### Further clean by debates dataset ####
## Re-name columns ##
by_debates_cleaned <- by_debates_cleaned |>
  rename(
    debate_number = Debate_number,
    election_year = Election_year)
by_debates_cleaned

#### Further clean DQI by speech dataset ####
## Remove old unique ID column before adding my own ##
dqi_by_speech_cleaned <- select(dqi_by_speech_cleaned, -UniqueID) 

## Rename columns ##
dqi_by_speech_final <- dqi_by_speech_cleaned |>
  rename(
    Election_year = ElectionYear,
    Organizer = DebateOrganizer,
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
    Speaker_incumbent_PM = IsSpeakerIncumbentPM)
dqi_by_speech_final

#### Re-code "debate_number" for consistency across datasets ####
dqi_by_speech_final <- dqi_by_speech_final |>
  mutate("Debate_number" = case_when(
    Debate_number == 	"2008_FrConsortium" ~ "2008FrConsortium",
    Debate_number == "2008_EnConsortium" ~ "2008EnConsortium",
    Debate_number == "2011_EnConsortium" ~ "2011EnConsortium",
    Debate_number == "2011_FrConsortium" ~ "2011FrConsortium",
    Debate_number == "2015_Macleans" ~ "2015Macleans",    
    Debate_number == "2015_Globe" ~ "2015Globe&Mail",
    Debate_number == "2015_Radio-Canada" ~ "2015FrConsortium",
    Debate_number == "2015_Munk" ~ "2015Munk",
    Debate_number == "2015_TVA" ~ "2015TVA",
    Debate_number == "2019_Macleans" ~ "2019Macleans",
    Debate_number == "2019_TVA" ~ "2019TVA",
    Debate_number == "2019_EnCommission" ~ "2019EnLDC",
    Debate_number == "2019_FrCommission" ~ "2019FrLDC",
    Debate_number == "2021_TVA" ~ "2021TVA",
    Debate_number == "2021_FrCommission" ~ "2021FrLDC",
    Debate_number == "2021_En_Commission" ~ "2021EnLDC")) 
dqi_by_speech_final

#### Update "debate_organizer" for 2015 Consortium debate ####
dqi_by_speech_final <- dqi_by_speech_final |>
  mutate("Organizer" = case_when(
    Organizer == "Radio_Canada" ~ "Consortium", 
    TRUE ~ Organizer))
dqi_by_speech_final

## Add Unique ID column back in, starting from 1 ##
dqi_by_speech_final <- dqi_by_speech_final |>
  mutate(ID = c(1:6995), .before = Language) |>
  select(
    ID,
    Language,
    Election_year,
    Organizer,
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
    Speaker_incumbent_PM)
dqi_by_speech_final

#### Save datasets ####
## By debates ##
write_csv(by_debates_cleaned, "Outputs/Data/by_debates_cleaned.csv")

## DQI by speech ##
write_csv(dqi_by_speech_final, "Outputs/Data/dqi_by_speech_final.csv")
