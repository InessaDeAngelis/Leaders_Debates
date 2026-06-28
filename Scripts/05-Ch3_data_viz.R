#### Preamble ####
# Purpose: Visualize who is asking debate questions for Ch3
# Author: Inessa De Angelis
# Date: 4 December 2024
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
## Read in packages ##
library(tidyverse)
library(hrbrthemes)

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
# Helpful suggestions: https://www.datacamp.com/tutorial/facets-ggplot-r
jpeg("Ch3_whos_asking.jpeg", units="in", width=9, height=6, res=300)
ggplot(all_qs_viz, aes(Issue, Percentage/1000)) +
  geom_bar(stat = "identity", fill = "#123A7A") +
  facet_wrap(~Whos_asking) +
  labs(x = "Issue", y = "Percentage of questions asked") +
  scale_y_continuous(labels = scales::percent) +
  theme_ipsum() +
  theme(
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.major.x = element_line(color = "grey94", linewidth = 0.25),
    axis.line = element_blank(),
    strip.text.x = element_text(size = 14),
    axis.text.x = element_text(size = 8, angle = 60, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"))
dev.off()
