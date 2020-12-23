setwd("~/Desktop/final_project")

library(tidyverse)
library(dplyr)
library(haven)
library(labelled)

raw_sample_online <- read_dta("2019-Canadian-Election-Study/input/survey/online/2019CES.dta")

raw_online <- labelled::to_factor(raw_sample_online)

reduced_online <- 
  raw_online %>% 
  select(cps19_citizenship, cps19_yob_2001_age,
         cps19_yob, cps19_gender,
         cps19_province, cps19_education,
         cps19_imp_iss_party, cps19_v_likely,
         cps19_votechoice, cps19_vote_unlikely,
         cps19_v_advance, cps19_vote_lean,
         cps19_fed_gov_sat, cps19_turnout_2015, 
         cps19_vote_2015, cps19_bornin_other,
         cps19_imm_year,cps19_employment,
         cps19_income_cat, cps19_marital, 
         cps19_household, cps19_rel_imp, cps19_religion
         )

colname <- colnames(reduced_online)
colname <- str_remove(colname,"cps19_" )
names(reduced_online) <- colname



reduced_online$yob <- as.numeric(as.character(reduced_online$yob))
reduced_online$yob_2001_age <- as.numeric(as.character(reduced_online$yob_2001_age))

# clean vote
reduced_online <- reduced_online %>% 
  filter(citizenship == "Canadian citizen")%>%
  mutate(age = 2019- yob)%>% filter(is.na(yob_2001_age) | yob_2001_age == 18) %>%
  filter(!(v_likely == "I am not eligible to vote"))%>%
  mutate(vote_combine = coalesce(votechoice, 
                                 vote_unlikely,
                                 v_advance, 
                                 vote_lean))

reduced_online$vote_combine <- as.character(reduced_online$vote_combine)

reduced_online <- reduced_online%>%
  mutate( vote_combine = case_when(
    str_detect(vote_combine, "Bloc") ~ "Bloc Quebecois",
    TRUE ~ vote_combine))%>%
  filter(!(vote_combine == "Don't know/ Prefer not to answer"))

reduced_online$vote_2015 <- as.character(reduced_online$vote_2015)
reduced_online <- reduced_online%>%
  mutate( vote_2015 = case_when(
    str_detect(vote_2015, "Bloc") ~ "Bloc Quebecois",
    TRUE ~ vote_combine))

reduced_online$imp_iss_party <- as.character(reduced_online$imp_iss_party)
reduced_online <- reduced_online%>%
  mutate( imp_iss_party = case_when(
    str_detect(imp_iss_party, "Bloc") ~ "Bloc Quebecois",
    TRUE ~ vote_combine))

reduced_online$turnout_2015 <- as.character(reduced_online$turnout_2015)
reduced_online <- reduced_online%>%
  mutate( turnout_2015 = case_when(
    str_detect(turnout_2015, "Bloc") ~ "Bloc Quebecois",
    TRUE ~ vote_combine))


# clean education
reduced_online <- reduced_online %>% 
  rowwise() %>% 
  mutate(education = case_when(
    education == "No schooling" ~ "Less than high school diploma",
    education == "Some elementary school" ~ "Less than high school diploma",
    education == "Completed elementary school" ~ "Less than high school diploma",
    education == "Some secondary/ high school" ~ "Less than high school diploma",
    education == "Completed secondary/ high school" ~ "High school diploma or certificate",
    education == "Some technical, community college, CEGEP, College Classique" ~ "University certificate or diploma below the bachelor's level",
    education == "Completed technical, community college, CEGEP, College Classique" ~"University certificate or diploma below the bachelor's level",
    education == "Some university" ~ "University certificate or diploma below the bachelor's level",
    education == "Bachelor's degree" ~ "Bachelor's degree",
    education == "Master's degree" ~"University certificate, diploma or degree above",
    education == "Professional degree or doctorate" ~ "University certificate, diploma or degree above",
    education == "Don't know/ Prefer not to answer" ~ "Not stated"
    )
  )


# clean religion
reduced_online <- reduced_online%>%
  mutate(religion_new = ifelse(religion == "None/ Don't have one/ Atheist", "None", as.character(rel_imp)))

# clean household size
reduced_online <- reduced_online%>% filter(household < 20)%>%
  mutate(household_size = ifelse(household >= 6, 6, household))

# clean age
reduced_online <- reduced_online %>% filter(age >= 18) %>% mutate(age = round(age))
age_group <- cut(reduced_online$age, c(seq(18, 100, by = 5), Inf), include.lowest = TRUE)
reduced_online <- reduced_online %>% cbind(age_group)


# clean gender
reduced_online <- reduced_online %>% mutate(gender = ifelse(gender == "A man", 1, 2))%>%
  rename(sex = gender)


clean_online <- reduced_online %>% select(province,
                                          sex,
                                          age_group,
                                          education,
                                          religion_new,
                                          household_size,
                                          marital,
                                          vote_combine,
                                          vote_2015,
                                          imp_iss_party,
                                          fed_gov_sat,
                                          turnout_2015)%>%rename(religion = religion_new)

write_csv(clean_online, "2019-Canadian-Election-Study/output/cleaned_online_data.csv")











  



