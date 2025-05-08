#### Preamble ####
# Purpose: Calculate Green Party debate participation / vote share
# Author: Inessa De Angelis
# Date: 7 May 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Construct dataset ####
# 2008-2021 vote share: https://www.sfu.ca/~aheard/elections/1867-present.html
# 2025 (preliminary) vote share: https://enr.elections.ca/National.aspx?lang=e

gpc_data <- data.frame(
    Election_year = c("2008", "2011", "2015", "2019", "2021", "2025"),
    Debate_participation = c("1", "0", "1", "1", "1", "0"),
    Vote_share = c("6.8", "3.9", "3.4", "6.5", "2.3", "1.2"))

#### Analyze ####
## Make variables numeric ##
gpc_data$Debate_participation <- as.numeric(gpc_data$Debate_participation)
gpc_data$Vote_share <- as.numeric(gpc_data$Vote_share)

## Run Pearson correlation test ##
correlation <- cor(gpc_data$Debate_participation, gpc_data$Vote_share)
print(correlation)
