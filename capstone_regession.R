library(tidyverse)
library(skimr)
library(readxl)

model_data_raw <- read_xlsx("data/model_data_raw.xlsx")

model_data_raw <- model_data_raw %>%
  mutate(across(where(is.character), ~na_if(.x, "NA"))) %>% 
  filter(!is.na(overdose_death))

model_data_raw_sum <- model_data_raw %>% 
  skim() 

model_data <- model_data_raw %>% 
  select(-model_data_raw_sum$skim_variable)


data.frame(
  variable = names(model_data_raw),
  missing = colSums(is.na(model_data_raw))
)
#model

model_data <- model_data_raw[, c(
  "overdose_death",
  "arrests_total",
  "povertyE",
  "unempE",
  "giniE"
)]

model_data <- na.omit(model_data)

model <- lm(overdose_death ~ arrests_total + povertyE + unempE + giniE, data = model_data)
summary(model)

####drop

model_data <- model_data_raw[, c(
  "overdose_death",
  "arrests_total",
  "povertyE",
  "unempE",
  "giniE"
)]

# number check
str(model_data_raw)

#char to number 
library(dplyr)

model_data_2 <- model_data_raw %>%
  mutate(
    popE = as.numeric(popE),
    median_hhincE = as.numeric(median_hhincE),
    povertyE = as.numeric(povertyE),
    giniE = as.numeric(giniE),
    unempE = as.numeric(unempE),
    overdose_death = as.numeric(overdose_death)
  )
model_data_2 <- model_data_raw %>%
  select(
    overdose_death,
    arrests_total,
    arrests_felony,
    median_hhincE,
    povertyE,
    unempE,
    giniE,
    popE,
    arrest_year
  )
#check again
str(model_data_2)

####
library(dplyr)

model_data_2 <- model_data_2 %>%
  mutate(
    overdose_death = as.numeric(overdose_death),
    median_hhincE = as.numeric(median_hhincE),
    povertyE = as.numeric(povertyE),
    unempE = as.numeric(unempE),
    giniE = as.numeric(giniE),
    popE = as.numeric(popE)
  )

str(model_data_2)

#Rate Variables
model_data_2 <- model_data_2 %>%
  mutate(
    overdose_rate = overdose_death / popE,
    arrest_rate = arrests_total / popE
  )

head(model_data_2)

##Na drop

model_clean <- model_data_2 %>%
  select(overdose_rate, arrest_rate, povertyE, median_hhincE, giniE, unempE, arrest_year) %>%
  drop_na()

dim(model_clean)

# reg 1
model <- lm(
  overdose_rate ~ arrest_rate + povertyE + median_hhincE + giniE + unempE,
  data = model_clean
)

summary(model)

# reg 2 fixed effects 

model_fe <- lm(
  overdose_rate ~ arrest_rate + povertyE + median_hhincE + giniE + unempE +
    factor(arrest_year),
  data = model_clean
)

summary(model_fe)

# clean with boro

model_clean_fe <- model_data_raw %>%
  mutate(
    overdose_death = as.numeric(overdose_death),
    median_hhincE = as.numeric(median_hhincE),
    povertyE = as.numeric(povertyE),
    unempE = as.numeric(unempE),
    giniE = as.numeric(giniE),
    popE = as.numeric(popE)
  ) %>%
  mutate(
    overdose_rate = overdose_death / popE,
    arrest_rate = arrests_total / popE
  ) %>%
  select(overdose_rate, arrest_rate, povertyE, median_hhincE, giniE, unempE, arrest_year, boro_cd) %>%
  drop_na()

# reg 3

model_full <- lm(
  overdose_rate ~ arrest_rate + povertyE + median_hhincE + giniE + unempE +
    factor(arrest_year) + factor(boro_cd),
  data = model_clean_fe
)

summary(model_full)

#The coefficient is negative, but the p-value is very large (0.873), meaning it is not statistically significant.
#After controlling for both time and neighborhood fixed effects, there is no statistically significant relationship between arrest rates and overdose rates.


#Lag model 

model_lag <- model_clean_fe %>%
  arrange(boro_cd, arrest_year) %>%
  group_by(boro_cd) %>%
  mutate(
    arrest_rate_lag1 = lag(arrest_rate, 1)
  ) %>%
  ungroup() %>%
  drop_na()

model_lag_fe <- lm(
  overdose_rate ~ arrest_rate_lag1 + povertyE + median_hhincE + giniE + unempE +
    factor(arrest_year) + factor(boro_cd),
  data = model_lag
)

summary(model_lag_fe)

library(car)

vif(model_full)

library(lmtest)

bptest(model_full)





# New file for prof Choe

model_prof <- read.csv("~/Downloads/model_data_rate_cd_2013_2021.csv")

model_data_prof <- model_prof

view(model_data_prof)

model_prof <- lm(
  overdose_deaths_midpoint_per_100k ~ arrests_total_per_100k_lag1 +
    median_hhinc_10k + poverty_rate + unemp_rate +
    pct_black + pct_hispanic + pct_foreign_born +
    factor(boro_cd) + factor(year),
  data = model_data_prof
)

install.packages("kableExtra")

summary(model_prof)

# negative highly statistically significant

vif_check <- lm(
  overdose_deaths_midpoint_per_100k ~ arrests_total_per_100k_lag1 +
    median_hhinc_10k + poverty_rate + unemp_rate +
    pct_black + pct_hispanic + pct_foreign_born,
  data = model_data_prof
)

vif(vif_check)

#Heterskdaticy present 
bptest(model_prof)

#Robust standard errors
coeftest(model_prof, vcov = vcovHC(model_prof, type = "HC1"))

#Bronx reg

bronx_data <- subset(model_data_prof, borough == "Bronx")

bronx_model <- lm(
  overdose_deaths_midpoint_per_100k ~ arrests_total_per_100k_lag1 +
    median_hhinc_10k + poverty_rate + unemp_rate +
    pct_black + pct_hispanic + pct_foreign_born +
    factor(year),
  data = bronx_data
)

summary(bronx_model)

#lagged arrest rates do not appear to significantly predict overdose death 
#rates after controlling for socioeconomic factors and year effects.

# Statan Island

si_data <- subset(model_data_prof, borough == "Staten Island")

si_model <- lm(
  overdose_deaths_midpoint_per_100k ~ arrests_total_per_100k_lag1 +
    median_hhinc_10k + poverty_rate + unemp_rate +
    pct_black + pct_hispanic + pct_foreign_born +
    factor(year),
  data = si_data
)

summary(si_model)

#Manhattan

manhattan_data <- subset(model_data_prof, borough == "Manhattan")

manhattan_model <- lm(
  overdose_deaths_midpoint_per_100k ~ arrests_total_per_100k_lag1 +
    median_hhinc_10k + poverty_rate + unemp_rate +
    pct_black + pct_hispanic + pct_foreign_born +
    factor(year),
  data = manhattan_data
)

summary(manhattan_model)


#Booklyn 

brooklyn_data <- subset(model_data_prof, borough == "Brooklyn")

brooklyn_model <- lm(
  overdose_deaths_midpoint_per_100k ~ arrests_total_per_100k_lag1 +
    median_hhinc_10k + poverty_rate + unemp_rate +
    pct_black + pct_hispanic + pct_foreign_born +
    factor(year),
  data = brooklyn_data
)

summary(brooklyn_model)

#Not significant, but directionally opposite

#Queens 

queens_data <- subset(model_data_prof, borough == "Queens")

queens_model <- lm(
  overdose_deaths_midpoint_per_100k ~ arrests_total_per_100k_lag1 +
    median_hhinc_10k + poverty_rate + unemp_rate +
    pct_black + pct_hispanic + pct_foreign_born +
    factor(year),
  data = queens_data
)

summary(queens_model)

#Higher lagged arrest rates are associated with lower future overdose rates in Queens

