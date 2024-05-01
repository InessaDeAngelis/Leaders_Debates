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
