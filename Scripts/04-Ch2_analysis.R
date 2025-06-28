#### Preamble ####
# Purpose: Make political engagement x debate watching models for Chapter 2
# Author: Inessa De Angelis
# Date: 9 May 2025
# Contact: inessa.deangelis@mail.utoronto.ca 
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)
library(janitor)
library(tinytable)
library(marginaleffects)
library(kableExtra)

ces2019_EN_debate <- read_csv("ces2019_EN_debate.csv")
ces2021_EN_debate <- read_csv("ces2021_EN_debate.csv")
ces2019_FR_debate <- read_csv("ces2019_FR_debate.csv")
ces2021_FR_debate <- read_csv("ces2021_FR_debate.csv")

#### Make 2019 models ####
## EN = 2019 ##
ces2019_EN_mod <- ces2019_EN_debate |>
  filter(!grepl("Don't know/ Prefer not to answer", pes19_follow_pol)) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps19_govt_confusing)) |>
  mutate(cps19_debate_en = case_when(
    cps19_debate_en == "No" ~ 0,
    cps19_debate_en == "Yes" ~ 1),
    pes19_follow_pol = case_when(
      pes19_follow_pol == "Not at all" ~ 0,
      pes19_follow_pol == "Not very closely" ~ 1,
      pes19_follow_pol == "Fairly closely" ~ 2,
      pes19_follow_pol == "Very closely" ~ 3),
    cps19_govt_confusing = case_when(
      cps19_govt_confusing == "Strongly agree" ~ 0,
      cps19_govt_confusing == "Somewhat agree" ~ 1,
      cps19_govt_confusing == "Somewhat disagree" ~ 2,
      cps19_govt_confusing == "Strongly disagree" ~ 3),
    cps19_gender = case_when(
      cps19_gender == "A man" ~ 0,
      cps19_gender == "A woman" ~ 1,
      cps19_gender == "Other (e.g. Trans, non-binary, two-spirit, gender-queer)" ~ 2),
   cps19_age = case_when(
     cps19_age >= 18 & cps19_age <= 24 ~ "18-24",
     cps19_age >= 25 & cps19_age <= 34 ~ "25-34",
     cps19_age >= 35 & cps19_age <= 44 ~ "35-44",
     cps19_age >= 45 & cps19_age <= 54 ~ "45-54",
     cps19_age >= 55 & cps19_age <= 64 ~ "55-64",
     cps19_age >= 65 & cps19_age <= 74 ~ "65-74",
     cps19_age >= 75 & cps19_age <= 84 ~ "75-84",
     cps19_age >= 85 & cps19_age <= 99 ~ "85-99"),
  cps19_age = factor(cps19_age, levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85-99")))

## Factor predictor variables ##
ces2019_EN_mod$pes19_follow_pol <- factor(ces2019_EN_mod$pes19_follow_pol)
ces2019_EN_mod$cps19_govt_confusing <- factor(ces2019_EN_mod$cps19_govt_confusing)
ces2019_EN_mod$pes19_interest_1 <- factor(ces2019_EN_mod$pes19_interest_1)
ces2019_EN_mod$cps19_interest_gen_1 <- factor(ces2019_EN_mod$cps19_interest_gen_1)
ces2019_EN_mod$cps19_interest_elxn_1 <- factor(ces2019_EN_mod$cps19_interest_elxn_1)
ces2019_EN_mod$cps19_gender <- factor(ces2019_EN_mod$cps19_gender)
  
## Make logistic regression model ##
set.seed(16)
mod2 <- glm(cps19_debate_en ~ pes19_follow_pol + pes19_interest_1 + cps19_interest_gen_1 + 
            cps19_interest_elxn_1 + cps19_govt_confusing + cps19_age + cps19_gender, data = ces2019_EN_mod,
            family = binomial(link = "logit"))
summary(mod2)

write_rds(x = mod2, "mod2.rds")

mod2_en <- slopes(mod2, newdata = "median") |>
  select(term, contrast, estimate) |>
  mutate(estimate = round(estimate, 3)) 

tt(mod2_en)

## FR - 2019 ##
ces2019_FR_mod <- ces2019_FR_debate |>
  filter(!grepl("Don't know/ Prefer not to answer", pes19_follow_pol)) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps19_govt_confusing)) |>
  mutate(cps19_debate_fr = case_when(
    cps19_debate_fr == "No" ~ 0,
    cps19_debate_fr == "Yes" ~ 1),
    pes19_follow_pol = case_when(
      pes19_follow_pol == "Not at all" ~ 0,
      pes19_follow_pol == "Not very closely" ~ 1,
      pes19_follow_pol == "Fairly closely" ~ 2,
      pes19_follow_pol == "Very closely" ~ 3),
    cps19_govt_confusing = case_when(
      cps19_govt_confusing == "Strongly agree" ~ 0,
      cps19_govt_confusing == "Somewhat agree" ~ 1,
      cps19_govt_confusing == "Somewhat disagree" ~ 2,
      cps19_govt_confusing == "Strongly disagree" ~ 3),
    cps19_gender = case_when(
      cps19_gender == "A man" ~ 0,
      cps19_gender == "A woman" ~ 1,
      cps19_gender == "Other (e.g. Trans, non-binary, two-spirit, gender-queer)" ~ 2),
    cps19_age = case_when(
      cps19_age >= 18 & cps19_age <= 24 ~ "18-24",
      cps19_age >= 25 & cps19_age <= 34 ~ "25-34",
      cps19_age >= 35 & cps19_age <= 44 ~ "35-44",
      cps19_age >= 45 & cps19_age <= 54 ~ "45-54",
      cps19_age >= 55 & cps19_age <= 64 ~ "55-64",
      cps19_age >= 65 & cps19_age <= 74 ~ "65-74",
      cps19_age >= 75 & cps19_age <= 84 ~ "75-84",
      cps19_age >= 85 & cps19_age <= 99 ~ "85-99"),
    cps19_age = factor(cps19_age, levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85-99")))

## Factor variables ##
ces2019_FR_mod$pes19_follow_pol <- factor(ces2019_FR_mod$pes19_follow_pol)
ces2019_FR_mod$cps19_govt_confusing <- factor(ces2019_FR_mod$cps19_govt_confusing)
ces2019_FR_mod$pes19_interest_1 <- factor(ces2019_FR_mod$pes19_interest_1)
ces2019_FR_mod$cps19_interest_gen_1 <- factor(ces2019_FR_mod$cps19_interest_gen_1)
ces2019_FR_mod$cps19_interest_elxn_1 <- factor(ces2019_FR_mod$cps19_interest_elxn_1)
ces2019_FR_mod$cps19_gender <- factor(ces2019_FR_mod$cps19_gender)

## Make model ##
set.seed(16)
mod3 <- glm(cps19_debate_fr ~ pes19_follow_pol + pes19_interest_1 + cps19_govt_confusing +
            cps19_interest_gen_1 + cps19_interest_elxn_1 + cps19_gender + cps19_age, data = ces2019_FR_mod,
            family = binomial(link = "logit"))
summary(mod3)

mod3_fr <- slopes(mod3, newdata = "median") |>
  select(term, contrast, estimate) |>
  mutate(estimate = round(estimate, 3)) 

tt(mod2_en)

#### Make 2021 models ####
## EN - 2021 ##
ces2021_EN_mod <- ces2021_EN_debate |>
  filter(!grepl("Don't know/ Prefer not to answer", pes21_follow_pol)) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps21_govt_confusing)) |>
  mutate(cps21_debate_en = case_when(
    cps21_debate_en == "No" ~ 0,
    cps21_debate_en == "Yes" ~ 1),
    pes21_follow_pol = case_when(
      pes21_follow_pol == "Not at all" ~ 0,
      pes21_follow_pol == "Not very closely" ~ 1,
      pes21_follow_pol == "Fairly closely" ~ 2,
      pes21_follow_pol == "Very closely" ~ 3),
    cps21_govt_confusing = case_when(
      cps21_govt_confusing == "Strongly agree" ~ 0,
      cps21_govt_confusing == "Somewhat agree" ~ 1,
      cps21_govt_confusing == "Somewhat disagree" ~ 2,
      cps21_govt_confusing == "Strongly disagree" ~ 3),
    cps21_genderid = case_when(
      cps21_genderid == "A man" ~ 0,
      cps21_genderid == "A woman" ~ 1,
      cps21_genderid == "Non-binary" ~ 2,
      cps21_genderid == "Another gender, please specify:" ~ 3),
    cps21_age = case_when(
      cps21_age  >= 18 & cps21_age  <= 24 ~ "18-24",
      cps21_age  >= 25 & cps21_age  <= 34 ~ "25-34",
      cps21_age  >= 35 & cps21_age  <= 44 ~ "35-44",
      cps21_age  >= 45 & cps21_age  <= 54 ~ "45-54",
      cps21_age  >= 55 & cps21_age  <= 64 ~ "55-64",
      cps21_age  >= 65 & cps21_age  <= 74 ~ "65-74",
      cps21_age  >= 75 & cps21_age  <= 84 ~ "75-84",
      cps21_age  >= 85 & cps21_age  <= 99 ~ "85-99"),
    cps21_age  = factor(cps21_age , levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85-99")))

## Factor predictor variables ##
ces2021_EN_mod$pes21_follow_pol <- factor(ces2021_EN_mod$pes21_follow_pol)
ces2021_EN_mod$cps21_govt_confusing <- factor(ces2021_EN_mod$cps21_govt_confusing)
ces2021_EN_mod$cps21_interest_gen_1 <- factor(ces2021_EN_mod$cps21_interest_gen_1)
ces2021_EN_mod$cps21_interest_elxn_1 <- factor(ces2021_EN_mod$cps21_interest_elxn_1)
ces2021_EN_mod$cps21_genderid <- factor(ces2021_EN_mod$cps21_genderid)

## Make model ##
set.seed(16)
mod4 <- glm(cps21_debate_en ~ pes21_follow_pol + cps21_govt_confusing +
              cps21_interest_gen_1 + cps21_interest_elxn_1 + cps21_genderid + cps21_age, data = ces2021_EN_mod,
            family = binomial(link = "logit"))
summary(mod4)

## FR - 2021 ##
ces2021_FR_mod <- ces2021_FR_debate |>
  filter(!grepl("Don't know/ Prefer not to answer", pes21_follow_pol)) |>
  filter(!grepl("Don't know/ Prefer not to answer", cps21_govt_confusing)) |>
  mutate(cps21_debate_fr = case_when(
    cps21_debate_fr == "No" ~ 0,
    cps21_debate_fr == "Yes" ~ 1),
    pes21_follow_pol = case_when(
      pes21_follow_pol == "Not at all" ~ 0,
      pes21_follow_pol == "Not very closely" ~ 1,
      pes21_follow_pol == "Fairly closely" ~ 2,
      pes21_follow_pol == "Very closely" ~ 3),
    cps21_govt_confusing = case_when(
      cps21_govt_confusing == "Strongly agree" ~ 0,
      cps21_govt_confusing == "Somewhat agree" ~ 1,
      cps21_govt_confusing == "Somewhat disagree" ~ 2,
      cps21_govt_confusing == "Strongly disagree" ~ 3),
    cps21_genderid = case_when(
      cps21_genderid == "A man" ~ 0,
      cps21_genderid == "A woman" ~ 1,
      cps21_genderid == "Non-binary" ~ 2,
      cps21_genderid == "Another gender, please specify:" ~ 3),
    cps21_age = case_when(
      cps21_age  >= 18 & cps21_age  <= 24 ~ "18-24",
      cps21_age  >= 25 & cps21_age  <= 34 ~ "25-34",
      cps21_age  >= 35 & cps21_age  <= 44 ~ "35-44",
      cps21_age  >= 45 & cps21_age  <= 54 ~ "45-54",
      cps21_age  >= 55 & cps21_age  <= 64 ~ "55-64",
      cps21_age  >= 65 & cps21_age  <= 74 ~ "65-74",
      cps21_age  >= 75 & cps21_age  <= 84 ~ "75-84",
      cps21_age  >= 85 & cps21_age  <= 99 ~ "85-99"),
    cps21_age  = factor(cps21_age , levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85-99")))

## Factor predictor variables ##
ces2021_FR_mod$pes21_follow_pol <- factor(ces2021_FR_mod$pes21_follow_pol)
ces2021_FR_mod$cps21_govt_confusing <- factor(ces2021_FR_mod$cps21_govt_confusing)
ces2021_FR_mod$cps21_interest_gen_1 <- factor(ces2021_FR_mod$cps21_interest_gen_1)
ces2021_FR_mod$cps21_interest_elxn_1 <- factor(ces2021_FR_mod$cps21_interest_elxn_1)
ces2021_FR_mod$cps21_genderid <- factor(ces2021_FR_mod$cps21_genderid)

## Make model ##
set.seed(16)
mod5 <- glm(cps21_debate_fr ~ pes21_follow_pol + cps21_govt_confusing +
              cps21_interest_gen_1 + cps21_interest_elxn_1 + cps21_genderid + cps21_age, data = ces2021_FR_mod,
            family = binomial(link = "logit"))
summary(mod5)

#### Correlation Tests ####
## 2019 EN ##
ces2019_EN_debate_cor <- ces2019_EN_debate |>
  mutate(cps19_debate_en = case_when(
    cps19_debate_en == "No" ~ 0,
    cps19_debate_en == "Yes" ~ 1),
    pes19_follow_pol = case_when(
      pes19_follow_pol == "Not at all" ~ 0,
      pes19_follow_pol == "Not very closely" ~ 1,
      pes19_follow_pol == "Fairly closely" ~ 2,
      pes19_follow_pol == "Very closely" ~ 3))

## Make variables numeric ##
ces2019_EN_debate_cor$cps19_debate_en <- as.numeric(ces2019_EN_debate_cor$cps19_debate_en)
ces2019_EN_debate_cor$pes19_follow_pol <- as.numeric(ces2019_EN_debate_cor$pes19_follow_pol)

correlation <- cor(ces2019_EN_debate_cor$cps19_debate_en, ces2019_EN_debate_cor$pes19_follow_pol,
                    use = "complete.obs")
print(correlation)

## 2019 FR ##
ces2019_FR_debate_cor <- ces2019_FR_debate |>
  mutate(cps19_debate_fr = case_when(
    cps19_debate_fr == "No" ~ 0,
    cps19_debate_fr == "Yes" ~ 1),
    pes19_follow_pol = case_when(
      pes19_follow_pol == "Not at all" ~ 0,
      pes19_follow_pol == "Not very closely" ~ 1,
      pes19_follow_pol == "Fairly closely" ~ 2,
      pes19_follow_pol == "Very closely" ~ 3))

## Make variables numeric ##
ces2019_FR_debate_cor$cps19_debate_fr <- as.numeric(ces2019_FR_debate_cor$cps19_debate_fr)
ces2019_FR_debate_cor$pes19_follow_pol <- as.numeric(ces2019_FR_debate_cor$pes19_follow_pol)

cor <- cor(ces2019_FR_debate_cor$cps19_debate_fr, ces2019_FR_debate_cor$pes19_follow_pol,
                   use = "complete.obs")
print(cor)

## 2021 EN ##
ces2021_EN_debate_cor <- ces2021_EN_debate |>
  mutate(cps21_debate_en = case_when(
    cps21_debate_en == "No" ~ 0,
    cps21_debate_en == "Yes" ~ 1),
    pes21_follow_pol = case_when(
      pes21_follow_pol == "Not at all" ~ 0,
      pes21_follow_pol == "Not very closely" ~ 1,
      pes21_follow_pol == "Fairly closely" ~ 2,
      pes21_follow_pol == "Very closely" ~ 3))

## Make variables numeric ##
ces2021_EN_debate_cor$cps21_debate_en <- as.numeric(ces2021_EN_debate_cor$cps21_debate_en)
ces2021_EN_debate_cor$pes21_follow_pol <- as.numeric(ces2021_EN_debate_cor$pes21_follow_pol)

correlation1 <- cor(ces2021_EN_debate_cor$cps21_debate_en, ces2021_EN_debate_cor$pes21_follow_pol,
                   use = "complete.obs")
print(correlation1)

## 2021 FR ##
ces2021_FR_debate_cor <- ces2021_FR_debate |>
  mutate(cps21_debate_fr = case_when(
    cps21_debate_fr == "No" ~ 0,
    cps21_debate_fr == "Yes" ~ 1),
    pes21_follow_pol = case_when(
      pes21_follow_pol == "Not at all" ~ 0,
      pes21_follow_pol == "Not very closely" ~ 1,
      pes21_follow_pol == "Fairly closely" ~ 2,
      pes21_follow_pol == "Very closely" ~ 3))

## Make variables numeric ##
ces2021_FR_debate_cor$cps21_debate_fr <- as.numeric(ces2021_FR_debate_cor$cps21_debate_fr)
ces2021_FR_debate_cor$pes21_follow_pol <- as.numeric(ces2021_FR_debate_cor$pes21_follow_pol)

cor1 <- cor(ces2021_FR_debate_cor$cps21_debate_fr, ces2021_FR_debate_cor$pes21_follow_pol,
                    use = "complete.obs")
print(cor1)

#### Summary statistics ####
## 2019 - EN ##
df <- ces2019_EN_debate |>
  filter(pes19_follow_pol != "Don't know/ Prefer not to answer") |>
  mutate(pes19_follow_pol = factor(
    pes19_follow_pol,
    levels = c("Not at all", "Not very closely", "Fairly closely", "Very closely"))) |>
  tabyl(pes19_follow_pol, cps19_debate_en, cps19_gender) |>
  adorn_percentages("col") |>
  adorn_totals("row") |>
  adorn_pct_formatting() |>
  as.data.frame() |> 
  rename(
    `Follow politics in the media` = pes19_follow_pol,
    `Watched Debate: No` = No,
    `Watched Debate: Yes` = Yes) 

df <- ces2021_EN_mod |>
  group_by(cps21_debate_en, cps21_genderid) |>
  count() |>
  print(n=36)

cps19_gender = case_when(
  cps19_gender == "A man" ~ 0,
  cps19_gender == "A woman" ~ 1,
  cps19_gender == "Other (e.g. Trans, non-binary, two-spirit, gender-queer)" ~ 2),

tt(df, caption = "People who watched the 2019 EN debate, by political media consumption")

## 2019 - FR ##
df1 <- ces2019_FR_debate |>
  filter(pes19_follow_pol != "Don't know/ Prefer not to answer") |>
  mutate(pes19_follow_pol = factor(
    pes19_follow_pol,
    levels = c("Not at all", "Not very closely", "Fairly closely", "Very closely"))) |>
  tabyl(pes19_follow_pol, cps19_debate_fr) |>
  adorn_percentages("col") |>
  adorn_totals("row") |>
  adorn_pct_formatting() |>
  rename(
    `Follow politics in the media` = pes19_follow_pol,
    `Watched Debate: No` = No,
    `Watched Debate: Yes` = Yes) 

tt(df1, caption = "People who watched the 2019 FR debate, by political media consumption")

## 2021 - EN ##
df2 <- ces2021_EN_debate |>
  filter(pes21_follow_pol != "Don't know/ Prefer not to answer") |>
  mutate(pes21_follow_pol = factor(
    pes21_follow_pol,
    levels = c("Not at all", "Not very closely", "Fairly closely", "Very closely"))) |>
  tabyl(pes21_follow_pol, cps21_debate_en) |>
  adorn_percentages("col") |>
  adorn_totals("row") |>
  adorn_pct_formatting() |>
  rename(
    `Follow politics in the media` = pes21_follow_pol,
    `Watched Debate: No` = No,
    `Watched Debate: Yes` = Yes) 

tt(df2, caption = "People who watched the 2021 EN debate, by political media consumption")

## 2021 - FR ##
df3 <- ces2021_FR_debate |>
  filter(pes21_follow_pol != "Don't know/ Prefer not to answer") |>
  mutate(pes21_follow_pol = factor(
    pes21_follow_pol,
    levels = c("Not at all", "Not very closely", "Fairly closely", "Very closely"))) |>
  tabyl(pes21_follow_pol, cps21_debate_fr) |>
  adorn_percentages("col") |>
  adorn_totals("row") |>
  adorn_pct_formatting() |>
  rename(
    `Follow politics in the media` = pes21_follow_pol,
    `Watched Debate: No` = No,
    `Watched Debate: Yes` = Yes) 

tt(df3, caption = "People who watched the 2021 FR debate, by political media consumption")
