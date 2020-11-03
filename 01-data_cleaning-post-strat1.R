#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://usa.ipums.org/usa/index.shtml
# Author: Zuoyu Wang; Zijun YE; Yuhan Zhao
# Data: 22 October 2020
# Contact: zijun.ye@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/chuckyip/Desktop/ps3")
raw_data <- read_dta("usa_00004.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% select(hhincome, language, empstat, sex, age, race, hispan, bpl, educd)

reduced_data <- reduced_data %>% 
  filter(hhincome != 9999999) %>%
  filter(language != "n/a or blank")%>%
  filter(empstat != "n/a") 

levels(reduced_data$age) <- c(levels(reduced_data$age), "90")
reduced_data$age[reduced_data$age =="90 (90+ in 1980 and 1990)"] <- "90"
reduced_data$age <- droplevels(reduced_data$age)
reduced_data$age <- as.character(reduced_data$age)
reduced_data$age <- as.numeric(reduced_data$age) + 2
reduced_data$bpl <- droplevels(reduced_data$bpl)

reduced_data <- reduced_data %>%
                    mutate(income = ifelse(hhincome < 15000, "<15,000",
                                           ifelse(hhincome >= 15000 & hhincome < 50000, "15,000-49,999",
                                                  ifelse(hhincome >= 50000 & hhincome < 100000, "50,000-99,999",
                                                         ifelse(hhincome >= 100000 & hhincome < 150000, "100,000-149,999",
                                                                "above 150,000"))))) %>% 
                  mutate(degree = ifelse(educd %in% c("associate's degree, type not specified", "Associate's degree, occupational program",
                                                      "Associate's degree, academic program", "3 years of college", "4 years of college",
                                                      "bachelor's degree", "5+ years of college", "6 years of college (6+ in 1960-1970)",
                                                      "7 years of college", "8+ years of college", "master's degree", 
                                                      "professional degree beyond a bachelor's degree", "doctoral degree"), 
                                         "Associate and above", "Less than Associate")) %>% 
                  mutate(age_group = ifelse(age>=18 & age<35, "18-34",
                                            ifelse(age>=35 & age<60, "35-59", "Above 60"))) %>% 
                  mutate(gender = ifelse(sex=="male", "Male", "Female")) %>% 
                  mutate(race = ifelse(race == "white", "White",
                                       ifelse(race == "black/african american/negro", "Black", "Other"))) %>%
                  mutate(hispanic = ifelse(hispan=="not hispanic", "No", "Yes")) %>% 
                  mutate(foreign_born = ifelse(bpl %in% c("alabama", "alaska", "arizona", "arkansas", "california", "colorado",
                                                          "connecticut", "delaware", "district of columbia", "florida", "georgia",
                                                          "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky",
                                                          "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota",
                                                          "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire",
                                                          "new jersey", "new mexico", "new york", "north carolina", "north dakota",
                                                          "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina",
                                                          "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington",
                                                          "west virginia", "wisconsin", "wyoming", "american samoa", "guam", "puerto rico",
                                                          "u.s. virgin islands"), "The United States", "Another country"))


final_reduced_data <- reduced_data %>% select(income, gender, age_group, race, hispanic, foreign_born, degree)



final_trump_data <- final_reduced_data %>% group_by(income, gender, age_group, race, hispanic, foreign_born, degree) %>% count(gender)
final_biden_data <- final_reduced_data %>% group_by(gender, race, degree) %>% count(gender)

# Saving the census data as a csv file in my
# working directory
write_csv(final_trump_data, "census_data_trump.csv")
write_csv(final_biden_data, "census_data_biden.csv")



         