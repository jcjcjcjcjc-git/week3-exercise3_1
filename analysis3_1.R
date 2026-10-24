renv::init()

library(tidyverse)

clinic_raw = read_csv("clinic_raw.csv", skip = 2)
str(clinic_raw)
summary(clinic_raw)

# Splitting BP into two fields
library(stringr)
clinic_clean = 
  clinic_raw %>%
  mutate(
    bp_upper = as.numeric(str_extract(blood_pressure, "^[0-9]+")),
    bp_lower = as.numeric(str_extract(blood_pressure, "(?<=/)[0-9]+"))
  )

# Converting Site into lowercase + strip whitespace
clinic_clean = 
  clinic_clean %>%
  mutate(
    site = tolower(trimws(site)),
    site = gsub("_", " ", site)
  )

# Converting Weight into true NA and weight as numeric
clinic_clean =
  clinic_clean %>%
  mutate(
    weight_kg = na_if(weight_kg, "-"),
    weight_kg = as.numeric(weight_kg)
  )

# Summarising mean age by site
clinic_clean %>% 
  group_by(site) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    n = n()
  )

renv::snapshot()

# Renaming patient_id to id
clinic_clean =
  clinic_clean %>%
  rename (id = patient_id)

# Capitalise gender
clinic_clean =
  clinic_clean %>%
  mutate(
    sex = ifelse(
      toupper(sex) == "M", "Male"
      , ifelse(toupper(sex) == "F", "Female", NA)
    )
  )
