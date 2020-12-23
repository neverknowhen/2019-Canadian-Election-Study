#### Preamble ####
# Purpose: The purpose of this code is to clean-up the 2017 GSS data obtained 
# from the U of T library. That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed. This code does that.
# The main issue is that the data are released with codes for variables, whereas,
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called gss.csv.
# Authors: Rohan Alexander and Sam Caetano
# Contact: rohan.alexander@utoronto.ca
# Date: 7 October 2020
# License: MIT
# Pre-reqs: You need to have downloaded the data from the library. To do that: 
  ## 1. Go to: http://www.chass.utoronto.ca/
  ## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
  ## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
  ## 4. Continue in English (you're welcome to use the French, but we probably can't
  ## help you too much).
  ## 5. Crtl F GSS, click
  ## 6. Click "Data" on the one you want. We used 2017, but you may want a different 
  ## wave. In particular the General Social Survey on social identity (cycle 27), 
  ## 2013 has some variables on voter participation if you're into that sort of 
  ## thing. You're welcome to pick any year but this code applies to 2017.
  ## 7. Click download
  ## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
  ## 9. Can select all variables by clicking button next to green colored "All". Then continue.
  ## 10. Create the files, download and save
# Check: 
  ## You WILL need to change the raw data name. Search for .csv - line 41
  ## You may need to adjust the filepaths depending on your system. Search for: read_


#### Workspace set-up ####
library(janitor)
library(tidyverse)
library(haven)
library(labelled)


setwd("~/Desktop/final_project")

# Load the data dictionary and the raw data and correct the variable names
raw_data <- read_csv("2019-Canadian-Election-Study/input/census/AAEdPQ3N.csv")

dict <- read_lines("2019-Canadian-Election-Study/input/census/gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("2019-Canadian-Election-Study/input/census/gss_labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
 
# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(CASEID, 
         agedc, 
         sex, 
         brthcan,
         brthmacr,
         brthprvc,
         yrarri,
         prv, 
         region, 
         luc_rst, 
         marstat, 
         amb_01, 
         vismin, 
         alndimmg,
         bpr_16, 
         bpr_19,
         ehg3_01b,
         hsdsizec,
         uhw_16gr,
         rlr_110,
         famincg2,
         noc1610, 
         cu0rnkc,
         ree_02,
         rto_101,
         slm_01,
         wght_per,
         wtbs_001)

# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age = agedc,
         sex = sex,
         place_birth_canada = brthcan,
         place_birth_macro_region = brthmacr,
         place_birth_province = brthprvc,
         year_arrived_canada = yrarri,
         province = prv,
         region = region,
         pop_center = luc_rst,
         marital_status = marstat,
         aboriginal = amb_01,
         vis_minority = vismin,
         age_immigration = alndimmg,
         landed_immigrant = bpr_16,
         citizenship_status = bpr_19,
         education = ehg3_01b,
         household_size = hsdsizec,
         average_hours_worked = uhw_16gr,
         religion_importance = rlr_110,
         income_family = famincg2,
         occupation = noc1610,
         number_of_current_union = cu0rnkc,
         religion_participation = ree_02,
         full_part_time_work = rto_101,
         feeling_life = slm_01,
         weight_personal = wght_per,
         bootstrap_weight_person = wtbs_001)



# clean education
gss <- gss %>% 
  rowwise() %>% 
  mutate(education = case_when(
    education == 1 ~ "Less than high school diploma",
    education == 2 ~ "High school diploma or certificate",
    education == 3 ~ "University certificate or diploma below the bachelor's level",
    education == 4 ~ "University certificate or diploma below the bachelor's level",
    education == 5 ~ "University certificate or diploma below the bachelor's level",
    education == 6 ~ "Bachelor's degree",
    education == 7 ~ "University certificate, diploma or degree above",
    education == 96 ~ "Not stated",
    education == 97 ~ "Not stated",
    education == 98 ~ "Not stated",
    education == 99 ~ "Not stated")
  )

gss <- gss %>% 
  rowwise() %>% 
  mutate(feeling_lifes = case_when(
    feeling_life == 97 |feeling_life == 98 |feeling_life == 99 ~ "Don 't know/ Prefer not to answer",		
    feeling_life == 5 |feeling_life == 4 ~ "Not very satisfied" ,		
    feeling_life == 0 |feeling_life == 1 |feeling_life == 2|feeling_life == 3 ~ "Not at all satisfied",			
    feeling_life == 7 |feeling_life == 6 ~ "Fairly satisfied",
    feeling_life == 10 |feeling_life == 9|feeling_life == 8 ~ "Very satisfied")
  )


# clean religion
gss <- gss %>% mutate(religion = case_when(
  religion_importance == 1 ~ "Very important",
  religion_importance == 2 ~ "Somewhat important",
  religion_importance == 3 ~ "Not very important",
  religion_importance == 4 ~ "Not important at all",
  religion_importance == 6 ~ "None",
  religion_importance == 7 ~ "Don't know/ Prefer not to answer",
  religion_importance == 8 ~ "Don't know/ Prefer not to answer",
  religion_importance == 9 ~ "Don't know/ Prefer not to answer"))

gss$province <- as.numeric(gss$province)

# clean resident province
gss <- gss %>% 
  rowwise()%>%
  mutate(province = case_when(
    province == 10 ~ "Newfoundland and Labrador",
    province == 11 ~ "Prince Edward Island",
    province == 12 ~ "Nova Scotia",
    province == 13 ~ "New Brunswick",
    province == 24 ~ "Quebec",
    province == 35 ~ "Ontario", 
    province == 46 ~ "Manitoba",
    province == 47 ~ "Saskatchewan",
    province == 48 ~ "Alberta",
    province == 59 ~ "British Columbia",
    # province == 96 ~ NA,
    # province == 97 ~ NA, 
    # province == 98 ~ NA,
    # province == 99 ~ NA
    )) %>% filter(!(is.na(province)))


# clean marital status
gss <- gss %>% 
  rowwise()%>%
  mutate(marital_status = case_when(
    marital_status == 1 ~ "Married",
    marital_status == 2 ~ "Living with a partner",
    marital_status == 3 ~ "Widowed",
    marital_status == 4 ~ "Separated",
    marital_status == 5 ~ "Divorced",
    marital_status == 6 ~ "Never Married", 
    marital_status == 96 ~ "Don't know/ Prefer not to answer" ,
    marital_status == 97 ~"Don't know/ Prefer not to answer",
    marital_status == 98 ~ "Don't know/ Prefer not to answer",
    marital_status == 99 ~ "Don't know/ Prefer not to answer"))%>%
  rename(marital = marital_status)


# clean age
gss <- gss %>% filter(age >= 18) %>% mutate(age = round(age))
age_group <- cut(gss$age, c(seq(18, 100, by = 5), Inf), include.lowest = TRUE)
gss <- gss %>% cbind(age_group)


gss<- gss%>%select(province,
                   sex,
                   age_group,
                   education, 
                   religion,
                   household_size, 
                   marital,
                   feeling_lifes,
                   weight_personal,
                   bootstrap_weight_person)


write_csv(gss[, 1:8], "2019-Canadian-Election-Study/output/cleaned_gss_data.csv")

clean_census <- gss %>%
  group_by(province, sex, age_group, education, religion, household_size, marital, feeling_lifes)%>%
  summarise(weight = round(sum(weight_personal)),
            weight_bs = round(sum(bootstrap_weight_person)))
  
write_csv(clean_census, "2019-Canadian-Election-Study/output/census_cell_data.csv")



