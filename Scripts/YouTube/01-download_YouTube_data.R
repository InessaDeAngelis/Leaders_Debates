#### Preamble ####
# Purpose: Download YouTube comments
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
videoIDs <- "Tr_CwDsQzg8"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2021_commments <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 4000)

write_csv(x = EN_debate_2021_commments, file = "Inputs/Data/YouTube/EN_debate_2021_commments.csv")

## CTV News ##
videoIDs <- "063tg9EyQyY"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2021_commments_CTV <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(x = EN_debate_2021_commments_CTV, file = "Inputs/Data/YouTube/EN_debate_2021_commments_CTV.csv")

#### Get 2021 FR debate comments ####
## Global News ##
videoIDs <- "Eqjmz0HlsLQ"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2021_commments <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(x = FR_debate_2021_commments, file = "Inputs/Data/YouTube/FR_debate_2021_commments.csv")

## Global News - again ##
videoIDs <- "FLhrX9hBcy0"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2021_commments_GN <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(x = FR_debate_2021_commments_GN, file = "Inputs/Data/YouTube/FR_debate_2021_commments_GN.csv")

#### Get 2019 EN debate comments ####
## CTV News ##
videoIDs <- "MG6in0Ix4SY"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2019_commments_CTV <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 2000)

write_csv(x = EN_debate_2019_commments_CTV, file = "Inputs/Data/YouTube/EN_debate_2019_commments_CTV.csv")

## CBC News ##
videoIDs <- "1VRliFlrvfA"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
EN_debate_2019_commments_CBC <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 5000)

write_csv(x = EN_debate_2019_commments_CBC, file = "Inputs/Data/YouTube/EN_debate_2019_commments_CBC.csv")

#### Get 2019 FR debate comments ####
## Global News ##
videoIDs <- "zU6AudYUCuo"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2019_commments_GN <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(x = FR_debate_2019_commments_GN, file = "Inputs/Data/YouTube/FR_debate_2019_commments_GN.csv")

## CBC News ##
videoIDs <- "68hBSrw_qvw"
youtubeAuth <- Authenticate("youtube", apiKey = myAPIKey)
FR_debate_2019_commments_CBC <- youtubeAuth |> 
  Collect(videoIDs = videoIDs, writeToFile = FALSE, verbose = FALSE, maxComments = 1000)

write_csv(x = FR_debate_2019_commments_CBC, file = "Inputs/Data/YouTube/FR_debate_2019_commments_CBC.csv")
