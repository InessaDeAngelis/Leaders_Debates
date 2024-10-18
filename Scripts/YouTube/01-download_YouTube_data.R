#### Preamble ####
# Purpose: Download YouTube comments for Chapter 5
# Author: Inessa De Angelis
# Date: 15 April 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Code referenced from: https://forum.posit.co/t/collect-comments-youtube/63762/2

#### Workspace setup ####
library(vosonSML)
library(tidyverse)

#### API Key ####
myAPIKey = "ADD KEY HERE" 

#### Get 2021 EN debate comments ####
## Global News ##
# URL: https://www.youtube.com/watch?v=Tr_CwDsQzg8&ab_channel=GlobalNews
videoIDs <- "Tr_CwDsQzg8"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2021_commments <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 4000)

write_csv(EN_debate_2021_commments, "Inputs/Data/YouTube/EN_debate_2021_commments.csv")

## CTV News ##
# URL: https://www.youtube.com/watch?v=063tg9EyQyY&ab_channel=CTVNews
videoIDs <- "063tg9EyQyY"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2021_commments_CTV <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(x = EN_debate_2021_commments_CTV, file = "Inputs/Data/YouTube/EN_debate_2021_commments_CTV.csv")

#### Get 2021 FR debate comments ####
## Global News ##
# URL: https://www.youtube.com/watch?v=Eqjmz0HlsLQ&ab_channel=GlobalNews
videoIDs <- "Eqjmz0HlsLQ"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2021_commments <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(FR_debate_2021_commments, "Inputs/Data/YouTube/FR_debate_2021_commments.csv")

## Global News ##
# URL: https://www.youtube.com/watch?v=FLhrX9hBcy0&ab_channel=GlobalNews
videoIDs <- "FLhrX9hBcy0"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2021_commments_GN <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(FR_debate_2021_commments_GN, "Inputs/Data/YouTube/FR_debate_2021_commments_GN.csv")

#### Get 2019 EN debate comments ####
## CTV News ##
# URL: https://www.youtube.com/watch?v=MG6in0Ix4SY&ab_channel=CTVNews
videoIDs <- "MG6in0Ix4SY"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2019_commments_CTV <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 2000)

write_csv(EN_debate_2019_commments_CTV, "Inputs/Data/YouTube/EN_debate_2019_commments_CTV.csv")

## CBC News ##
# URL: https://www.youtube.com/watch?v=1VRliFlrvfA&ab_channel=CBCNews
videoIDs <- "1VRliFlrvfA"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2019_commments_CBC <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 5000)

write_csv(EN_debate_2019_commments_CBC, "Inputs/Data/YouTube/EN_debate_2019_commments_CBC.csv")

#### Get 2019 FR debate comments ####
## Global News ##
# URL: https://www.youtube.com/watch?v=zU6AudYUCuo&ab_channel=GlobalNews
videoIDs <- "zU6AudYUCuo"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2019_commments_GN <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(FR_debate_2019_commments_GN, "Inputs/Data/YouTube/FR_debate_2019_commments_GN.csv")

## CBC News ##
# URL: https://www.youtube.com/watch?v=68hBSrw_qvw&ab_channel=Radio-CanadaInfo
videoIDs <- "68hBSrw_qvw"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2019_commments_CBC <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(FR_debate_2019_commments_CBC, "Inputs/Data/YouTube/FR_debate_2019_commments_CBC.csv")

#### Get 2015 EN & FR debate comments ####
## Globe & Mail ##
# URL: https://www.youtube.com/watch?v=XbnMz7tsXjo&ab_channel=TheGlobeandMail
videoIDs <- "XbnMz7tsXjo"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
debate_2015_commments_GM <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(debate_2015_commments_GM, "Inputs/Data/YouTube/debate_2015_commments_GM.csv")

## Macleans ##
# URL: https://www.youtube.com/watch?v=hSf2__qpeGA&ab_channel=Maclean%27s
videoIDs <- "hSf2__qpeGA"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
debate_2015_commments_Mac <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1500)

write_csv(debate_2015_commments_Mac, "Inputs/Data/YouTube/debate_2015_commments_Mac.csv")