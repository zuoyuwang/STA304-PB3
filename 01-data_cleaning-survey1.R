#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Zuoyu Wang; Zijun YE; Yuhan Zhao
# Data: 22 October 2020
# Contact: zuoyu.wang@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# - Need to have downloaded the data from X and save the folder that you're
# interested in to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/tonystark/desktop/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("./ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <-
  raw_data %>%
  select(
    vote_2020,
    household_income,
    gender,
    age,
    race_ethnicity,
    hispanic,
    foreign_born,
    language,
    education,
    employment
  )

reduced_data <-
  reduced_data %>%
  filter(!is.na(vote_2020)) %>%
  mutate(vote_trump =
           ifelse(vote_2020 == "Donald Trump", 1, 0)) %>% 
mutate(vote_biden =
         ifelse(vote_2020 == "Joe Biden", 1, 0))

sample_d <- reduced_data %>% filter(!is.na(household_income)) %>% 
  filter(!is.na(gender)) %>% 
  filter(!is.na(age)) %>% 
  filter(!is.na(race_ethnicity)) %>% 
  filter(!is.na(hispanic)) %>%
  filter(!is.na(foreign_born)) %>% 
  filter(!is.na(language)) %>% 
  filter(!is.na(education)) %>% 
  filter(!is.na(employment))


sample_format <- sample_d %>% mutate(income = ifelse(household_income == "Less than $14,999", "<15,000", 
                                                     ifelse(household_income %in% c("$15,000 to $19,999", 
                                                                                    "$20,000 to $24,999", 
                                                                                    "$25,000 to $29,999",
                                                                                    "$30,000 to $34,999",
                                                                                    "$35,000 to $39,999",
                                                                                    "$40,000 to $44,999",
                                                                                    "$45,000 to $49,999"), "15,000-49,999",
                                                            ifelse(household_income %in% c("$50,000 to $54,999",
                                                                                           "$55,000 to $59,999",
                                                                                           "$60,000 to $64,999",
                                                                                           "$65,000 to $69,999",
                                                                                           "$70,000 to $74,999",
                                                                                           "$75,000 to $79,999",
                                                                                           "$80,000 to $84,999",
                                                                                           "$85,000 to $89,999",
                                                                                           "$90,000 to $94,999",
                                                                                           "$95,000 to $99,999"), "50,000-99,999",
                                                                   ifelse(household_income %in% c("$100,000 to $124,999",
                                                                                                  "$125,000 to $149,999"), "100,000-149,999", "above 150,000"))))) %>% 
  mutate(degree = ifelse(education %in% c("Associate Degree", "College Degree (such as B.A., B.S.)", "Doctorate degree",
                                          "Masters degree", "Completed some graduate, but no degree"), "Associate and above",
                         "Less than Associate")) %>% 
  mutate(employed = ifelse(employment %in% c("Full-time employed", "Part-time employed", "Self-employed"), "Yes", 
                           ifelse(employment == "Retired", "Retired", "No"))) %>% 
  mutate(race = ifelse(!race_ethnicity %in% c("Black, or African American", "White"), "Other", ifelse(race_ethnicity == "White", "White", "Black"))) %>% 
  mutate(age_group = ifelse(age >= 18 & age < 35, "18-34", ifelse(age >= 35 & age < 60, "35-59", "Above 60"))) %>% 
  mutate(hispanic = ifelse(hispanic == "Not Hispanic", "No", "Yes")) %>% 
  mutate(language = ifelse(language == "No, we speak only English.", "English",
                           ifelse(language == "Yes, we speak Spanish.", "Spanish", "Other")))

# Saving the survey/sample data as a csv file in my working directory
write_csv(sample_format, "outputs/survey_data.csv")

