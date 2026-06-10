#### Preamble ####
# Purpose: Visualize who is asking for Ch3
# Author: Inessa De Angelis
# Date: 4 December 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
## Read in packages ##
library(tidyverse)
library(hrbrthemes)
library(patchwork)

## Read in dataset ##
all_qs <- read_csv("Outputs/Data/all_qs.csv")

## Fix up dataset for visualization ##
all_qs_viz <- all_qs |>
  mutate("Debate_number" = case_when(
    Debate_number == "2008FrConsortium" ~ "2008 Consortium (FR)",
    Debate_number == "2008EnConsortium" ~ "2008 Consortium (EN)",
    Debate_number == "2011EnConsortium" ~ "2011 Consortium (EN)",
    Debate_number == "2011FrConsortium" ~ "2011 Consortium (FR)",
    Debate_number == "2015Macleans" ~ "2015 Maclean's",
    Debate_number == "2015Globe&Mail" ~ "2015 Globe & Mail",
    Debate_number == "2015Radio-Canada" ~ "2015 Consortium (FR)",
    Debate_number == "2015Munk" ~ "2015 Munk",
    Debate_number == "2015TVA" ~ "2015 TVA",
    Debate_number == "2019Macleans" ~ "2019 Maclean's",
    Debate_number == "2019TVA" ~ "2019 TVA",
    Debate_number == "2019EnLDC" ~ "2019 LDC (EN)",
    Debate_number == "2019FrLDC" ~ "2019 LDC (FR)",
    Debate_number == "2021TVA" ~ "2021 TVA",
    Debate_number == "2021FrLDC" ~ "2021 LDC (FR)",
    Debate_number == "2021EnLDC" ~ "2021 LDC (EN)"),
    "Issue" = case_when(
      Issue == "Health care" ~ "Healthcare",
      TRUE ~ Issue))

#### Visualize data ####
## V1: Facet wrap ##
## Helpful suggestions: https://www.datacamp.com/tutorial/facets-ggplot-r

jpeg("Ch3_whos_asking.jpeg", units="in", width=8, height=7, res=300) 
ggplot(all_qs_viz, aes(Issue, Percentage/1000)) +
  geom_bar(stat = "identity", fill = "#123A7A") +
  facet_wrap(~Whos_asking) +
  labs(
    x = "Issue",
    y = "Percentage of questions asked") +
  theme_ipsum() +
  scale_y_continuous(labels=scales::percent) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(axis.text.x =  element_text(size = 9, angle = 70, hjust = 1)) +
  theme(axis.text.y = element_text(size = 9)) + 
  theme(axis.title.x = element_text(size = 16, face = "bold")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) 
dev.off()

## V2: Four separate graphs brought together using patchwork ##
# Citizens #
citizen <- ggplot(subset(all_qs_viz, Whos_asking == "Leader"), aes(Issue, Percentage/100)) +
  geom_col(fill = "#123A7A") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Leader", y = "Percentage of questions asked") +
  theme_ipsum() +
  theme(axis.text.x =  element_text(size = 9, angle = 70, hjust = 1)) +
  theme(axis.text.y = element_text(size = 9)) + 
  theme(axis.title.x = element_text(size = 16, face = "bold")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) 

# Journalists #
journalist <- ggplot(subset(all_qs_viz, Whos_asking == "Journalist"), aes(Issue, Percentage/100)) +
  geom_col(fill = "#123A7A") +
  scale_y_continuous(labels = scales::percent) + # to be fix
  labs(title = "Journalist", y = "Percentage of questions asked") +
  theme_ipsum() +
  theme(axis.text.x =  element_text(size = 9, angle = 70, hjust = 1)) +
  theme(axis.text.y = element_text(size = 9)) + 
  theme(axis.title.x = element_text(size = 16, face = "bold")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) 

# Leaders #
leader <- ggplot(subset(all_qs_viz, Whos_asking == "Leader"), aes(Issue, Percentage/100)) +
  geom_col(fill = "#123A7A") +
  scale_y_continuous(labels = scales::percent) + # to be fix
  labs(title = "Leader", y = "Percentage of questions asked") +
  theme_ipsum() +
  theme(axis.text.x =  element_text(size = 9, angle = 70, hjust = 1)) +
  theme(axis.text.y = element_text(size = 9)) + 
  theme(axis.title.x = element_text(size = 16, face = "bold")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) 

# Moderator #
moderator <- ggplot(subset(all_qs_viz, Whos_asking == "Moderator"), aes(Issue, Percentage/100)) +
  geom_col(fill = "#123A7A") +
  scale_y_continuous(labels = scales::percent) + # to be fix
  labs(title = "Moderator", y = "Percentage of questions asked") +
  theme_ipsum() +
  theme(axis.text.x =  element_text(size = 9, angle = 70, hjust = 1)) +
  theme(axis.text.y = element_text(size = 9)) + 
  theme(axis.title.x = element_text(size = 16, face = "bold")) +
  theme(axis.title.y = element_text(size = 16, face = "bold")) 

# Bring all together #
jpeg("Ch3_whos_asking_v2.jpeg", units="in", width=8, height=7, res=300) 
citizen + journalist + leader + moderator
dev.off()
