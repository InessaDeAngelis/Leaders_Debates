#### Preamble ####
# Purpose: Analyzes YouTube comments for Chapter 5
# Author: Inessa De Angelis
# Date: 8 October 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT

#### Workspace setup ####
library(tidyverse)

#### Read in dataset ####
debate_comments <- read_csv("Outputs/Data/YouTube/debate_comments.csv")

#### Prepare dataset ####
