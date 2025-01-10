# Import Data ---------------------------
# TODO: check wrong file on import
linelist_raw <- import("linelist_raw.xlsx", setclass = "tibble")

# agecount <- linelist_raw %>%
#  filter(age > 50) %>%
#  count(gender)

# Clean Data ----------------------------
linelist <- linelist_raw %>%
  
  janitor::clean_names() %>%
  
  rename(
    date_infection = infection_date,
    date_hospitalisation = hosp_date,
    date_outcome = date_of_outcome
  ) %>%
  
  select(-c(row_num,x28, merged_header)) %>%
  
  distinct() %>%
  # Add calculated columns
  mutate(
    bmi = wt_kg / (ht_cm/100)^2,
    gender = case_match(
      gender, 
      "f" ~ "female", 
      "m" ~ "male"
    ),
    text = stringr::str_glue("Admission of {age} {age_unit}-old {gender} in {hospital} on ({date_hospitalisation})")
  ) %>%
  
  filter(between(bmi, 10, 200)
  )%>%
  
  # Change Data Types
  mutate(
    across(contains("date"), as.Date),
    generation = as.numeric(generation),
    age        = as.numeric(age) 
  ) %>%
  
  mutate(outcome = recode(outcome,
                          .missing = "Missing"
  )) %>%
  
  mutate(hospital = recode(hospital,
                           .missing = "Missing",
                           # OLD = NEW
                           "Mitylira Hopital"  = "Military Hospital",
                           "Mitylira Hospital" = "Military Hospital",
                           "Military Hopital"  = "Military Hospital",
                           "Port Hopital"      = "Port Hospital",
                           "Central Hopital"   = "Central Hospital",
                           "other"             = "Other",
                           "St. Marks Maternity Hopital (SMMH)" = "St. Mark's Maternity Hospital (SMMH)"
                           
  )) %>%
  
  # mutate(
  #    time_admission = hm(time_admission)
  #  ) %>%
  
  mutate(
    combined_string = str_c(gender, age, age_unit, sep=", ")
  ) %>%
  
  mutate(
    days_onset_hosp = date_hospitalisation - date_onset
  ) %>%
  
  mutate(delay_cat = case_when(
    # criteria                                   # new value if TRUE
    days_onset_hosp < 2                        ~ "<2 days",
    days_onset_hosp >= 2 & days_onset_hosp < 5 ~ "2-5 days",
    days_onset_hosp >= 5                       ~ ">5 days",
    is.na(days_onset_hosp)                     ~ NA_character_,
    TRUE                                       ~ "Check me")
  ) %>%
  
  mutate(delay_cat = fct_recode(
    delay_cat,
    "Less than 2 days" = "<2 days",
    "2 to 5 days"      = "2-5 days",
    "More than 5 days" = ">5 days")
  )