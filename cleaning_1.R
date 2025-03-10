
library(haven)
library(tidyverse)
library(readxl)
library(countries)
library(countrycode)
library(janitor)
library(lme4)
library(car)

# Religiosity-----------------------------------------------
religiosity <- read_dta("data/replication_data.dta")

religiosity <- religiosity |> 
  group_by(country_name) |> 
  slice_max(year) |> 
  ungroup()

religiosity <- religiosity |> 
  select(religiosity_percent, country_name)

religiosity <- religiosity |> 
  mutate(country = country_name(country_name, to = "simple", 
                                verbose = TRUE),
         country_iso = countrycode::countrycode(country, origin = "country.name",
                                                destination = "iso2c")) |> 
  select(country_iso, religiosity_percent)

# Economic indicators---------------------------------------
economic_indicators <- read_xlsx("data/country_data.xlsx")

economic_indicators <- economic_indicators |> 
  drop_na(`Series Code`) 

economic_indicators <- economic_indicators %>%
  mutate(across(everything(), ~ str_remove_all(.x, "^\\.\\.$"))) %>%
  mutate(across(everything(), ~ if_else(.x == "", NA, .x)))

economic_indicators <- economic_indicators %>%
  mutate(latest_value = coalesce(`2023 [YR2023]`, 
                                 `2022 [YR2022]`, 
                                 `2021 [YR2021]`, 
                                 `2020 [YR2020]`)) |> 
  mutate(latest_value = as.numeric(latest_value))

economic_indicators <- economic_indicators |> 
  select(`Series Name`, `Country Name`, latest_value)

economic_indicators <- economic_indicators |> 
  pivot_wider(names_from = `Series Name`, values_from = latest_value )

economic_indicators <- economic_indicators |> 
  rename("country" = "Country Name", 
         "gini" = "Gini index",
         "gdp_pc" = "GDP per capita (constant 2015 US$)")

economic_indicators <- economic_indicators |> 
  mutate(country = country_name(country, to = "simple", 
                                verbose = TRUE),
         country_iso = countrycode::countrycode(country, origin = "country.name",
                                                destination = "iso2c")) |> 
  select(country_iso, gini, gdp_pc)
# Trans laws------------------------------------------------

trans <-  read_csv("data/trans.csv")

trans <- trans |> 
  row_to_names(row_number = 1)

trans <- trans |> 
  clean_names() |> 
  select(2, 8) |> 
  drop_na(na_2) |> 
  rename("country" = "na_2")

trans <- trans |> 
  mutate(country = country_name(country, to = "simple", 
                                verbose = TRUE),
         country_iso = countrycode::countrycode(country, origin = "country.name",
                                                destination = "iso2c")) |> 
  select(country_iso, self_determination)

# Rainbow score---------------------------------------------
rainbow <- read_csv("data/rainbow.csv")

rainbow <- rainbow |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  select(2, 3) |> 
  drop_na(na_2) |> 
  rename("country" = "na_2",
         "rain_ind" = "na_3") |> 
  mutate(rain_ind = as.numeric(rain_ind) / 100)

rainbow <- rainbow |> 
  mutate(country = country_name(country, to = "simple", 
                                verbose = TRUE),
         country_iso = countrycode::countrycode(country, origin = "country.name",
                                                destination = "iso2c")) |> 
  select(country_iso, rain_ind)

#Survey data------------------------------------------------

survey <- read_dta("data/ZA7575.dta")

survey <- survey |> 
  select(caseid, isocntry, d10, d11, 
         d1, sd3, d7, sd1_4, sd1_7, sd1_8, d70,
         d63, qc19, d15a)

# Recode dependent variable 
filtered_survey <- survey |> 
  mutate(trans_name = case_when(
    qc19==1 ~ 1,
    qc19==2 ~ 0,
    qc19==3 ~ NA)) 

# Recode gender
filtered_survey <- filtered_survey |> 
  mutate(female = case_when(
    d10 == 1 ~ 0,
    d10 == 2 ~ 1,
    TRUE ~ NA),
    female = factor(female))

# Recode age
filtered_survey <- filtered_survey |> 
  mutate(age = if_else(d11 == 99 , NA, d11))

# Current occupation

filtered_survey <- filtered_survey |> 
  mutate(occupation = case_when(
    d15a %in% c(1, 3) ~ "Unemployed",
    d15a == 2 ~ "Student",
    d15a == 4 ~ "Retired",
    d15a <= 9 ~ "Self employed",
    d15a <=  14 ~ "Employed" ,
    TRUE ~ NA), 
    occupation = factor(occupation))

# Recode religion 
filtered_survey <- filtered_survey |> 
  mutate(religion = case_when(
    sd3 %in% c(1, 3, 4) ~ "Christians", 
    sd3 == 2 ~ "Orthodox Chrsitian", 
    sd3 %in% c(6, 7, 8) ~ "Muslims", 
    sd3 %in% c(5, 10, 11, 14) ~ "Other religions", 
    sd3 %in% c(12, 13) ~ "Not religious", 
    TRUE ~ NA),
    religion = factor(religion))

# Marital status

filtered_survey <- filtered_survey |> 
  mutate(marital_status = case_when(
    d7 <= 4 ~ "Married",
    d7 <= 10 ~ "Single", 
    d7 <= 12 ~ "Divorced",
    d7 <=  14 ~ "Widowed",
    d7 == 15 ~ "Other", 
    TRUE ~ NA), 
    marital_status = factor(marital_status))

# Personal satisfaction

filtered_survey <- filtered_survey |> 
  mutate(personal_satis = if_else(d70 == 5, NA, d70),
         personal_satis = case_when(
           personal_satis <= 2 ~ "Satisfied",
           personal_satis <= 4 ~ "Not satisfied"
         ))

# Ideology

filtered_survey <- filtered_survey |> 
  mutate(ideology = if_else(d1 > 10, NA, d1)) 

# Contact LGBTQ+

filtered_survey <- filtered_survey %>% 
  mutate(contact_lgbti = case_when(
    sd1_4 == 1 | sd1_7 == 1 | sd1_8 == 1 ~ "Contact", # There is contact
    sd1_4 == 2 | sd1_7 == 2 | sd1_8 == 2 ~ "No contact", # There is not contact
    TRUE ~ NA
  ),
  contact_lgbti = factor(contact_lgbti))

# Autoperception of the social class

filtered_survey <- filtered_survey %>% 
  mutate(social_class = case_when(
    d63 %in% c(1, 2) ~ "Working class", 
    d63 == 3 ~ "Middle class", 
    d63 %in% c(4, 5) ~ "High class", 
    TRUE ~ NA
  ))

# Country names: DE-E and DE-W were problems 

filtered_survey <- filtered_survey %>% 
  mutate(country = country_name(isocntry, to = "simple", 
                                verbose = TRUE, poor_matches = TRUE),
         isocntry = countrycode::countrycode(country, origin = "country.name",
                                                destination = "iso2c"))
  
individual_data <- filtered_survey |> 
  select(caseid, isocntry, trans_name, female, age, religion, 
         marital_status, personal_satis, ideology, 
         contact_lgbti, social_class, occupation)

# Merge-----------------------------------------------------

final_data <- individual_data |> 
  left_join(rainbow, by = c("isocntry" = "country_iso")) |> 
  left_join(trans, by = c("isocntry" = "country_iso")) |> 
  left_join(economic_indicators, by = c("isocntry" = "country_iso")) |> 
  left_join(religiosity, by = c("isocntry" = "country_iso"))


# Models----------------------------------------------------

final_data$contact_lgbti <- relevel(final_data$contact_lgbti, ref = "No contact")

model <- glm(trans_name ~ age + 
               female + 
               religion + 
               occupation + 
               marital_status + 
               personal_satis +
               contact_lgbti +
               ideology +
               social_class, 
             family = "binomial", data = final_data)

summary(model)

exp(coef(model))

vif(model) # We do not have multicollinearity between the individual variables 


# We are going to scale the variables because we are having troubles with the
# identifiability of the model 
final_data <- final_data  |> 
  mutate(across(where(is.numeric) & !(starts_with("case") |
                                        starts_with("trans") ), scale))

model2 <- glmer(trans_name ~ 
                  age + 
                  female + 
                  religion +
                  occupation +
                  marital_status + 
                  personal_satis +
                  contact_lgbti +
                  ideology +
                  social_class +
                  (1|isocntry), 
                family = "binomial", 
                data = final_data)
summary(model2)


model3 <- glmer(trans_name ~ 
                  age + 
                  I(age^2) +
                  female + 
                  occupation +
                  religion*religiosity_percent + 
                  marital_status*religiosity_percent + 
                  personal_satis*self_determination +
                  contact_lgbti*rain_ind +
                  contact_lgbti*self_determination +
                  ideology*gdp_pc +
                  social_class*gini +
                  (1|isocntry), 
                family = "binomial", 
                data = final_data)
summary(model3)

# Not significant: Age, self_determination by itself, gdp_pc by itself,
# religiosity by itself, interaction between religiosity and marital status,
# AIC = 21167

model4 <- glmer(trans_name ~ 
                  age + 
                  I(age^2) + 
                  female + 
                  religion +
                  occupation + 
                  religion:religiosity_percent + 
                  marital_status + 
                  personal_satis + 
                  personal_satis:self_determination +
                  contact_lgbti*rain_ind +
                  contact_lgbti:self_determination +
                  ideology +
                  ideology:gdp_pc +
                  social_class*gini +
                  (1|isocntry), 
                family = "binomial", 
                data = final_data)
summary(model4)

# Not significant: self_determination and personal_satis, 
# gini and social class

model5 <- glmer(trans_name ~ 
                  age +
                  I(age ^ 2) + 
                  female + 
                  occupation + 
                  religion +
                  religion:religiosity_percent + 
                  marital_status + 
                  personal_satis +
                  contact_lgbti*rain_ind +
                  contact_lgbti:self_determination +
                  ideology + 
                  ideology:gdp_pc +
                  social_class +
                  gini +
                  (1|isocntry), 
                family = "binomial", 
                data = final_data)
summary(model5)

# Not significant Gini, contact and self determination, social class

model6 <- glmer(trans_name ~ 
                  age +
                  I(age ^ 2) + 
                  female + 
                  occupation +
                  religion +
                  religion:religiosity_percent + 
                  marital_status + 
                  personal_satis +
                  contact_lgbti*rain_ind +
                  ideology + 
                  ideology:gdp_pc +
                  (1|isocntry), 
                family = "binomial", 
                data = final_data)
summary(model6)

# Not significant:Marital status

model7 <- glmer(trans_name ~ 
                  age +
                  I(age ^ 2) + 
                  female + 
                  occupation +
                  religion +
                  religion:religiosity_percent + 
                  personal_satis +
                  contact_lgbti*rain_ind +
                  ideology + 
                  ideology:gdp_pc +
                  (1|isocntry), 
                family = "binomial", 
                data = final_data)
summary(model7)

# Now we are going to try random slopes

model8 <- glmer(trans_name ~ 
                  age +
                  I(age ^ 2) + 
                  female + 
                  occupation +
                  religion +
                  religion:religiosity_percent + 
                  personal_satis +
                  contact_lgbti*rain_ind +
                  ideology + 
                  ideology:gdp_pc +
                  (1 + age + female + ideology|isocntry), 
                family = "binomial", 
                data = final_data)
summary(model8)

lme4::ranef(model8) %>% # This is for extracting random intercepts and slopes
  str # ¿Como sabemos cual es cual?
