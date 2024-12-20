#
#
#    LINELIST DGPH R4EPI EXAMPLE
#
#
# Version 0.1
# 2024-11-29


# Load Packages -------------------------
pacman::p_load(
  rio, # importing data
  here, # relative file pathways
  janitor, # data cleaning and tables
  lubridate, # working with dates
  matchmaker, # dictionary-based cleaning
  epikit, # age_categories() function
  tidyverse, # data management and visualization
  #
  styler, # source code formatting
  lintr, # detects bad code patterns, which are not errors
  #
  skimr, # preview tibbles (aka data frames)
  todor, # add TODO comments to your project
  
  # Working with Dates 
  lubridate,  # general package for handling and converting dates  
  parsedate,  # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  
  # Working with Strings
  stringr,
  tools,

  # Work with Factors
  forcats,       # factors]
  
  # Build complex Tables
  kableExtra,
  
  # Table Viz
  flextable,      # make HTML tables 
  officer,
  
  # GG Plot Extras
  ggforce,
)


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

# linelist

# Visualize Table  ---------------------------

table <- linelist %>% 
  group_by(hospital, outcome) %>%
  summarise(                                           # Create new summary columns of indicators of interest
    N = n(),                                            # Number of rows per hospital-outcome group     
    ct_value = median(ct_blood, na.rm=T)) %>%          # median CT value per group
  bind_rows(                                           # Bind the previous table with this mini-table of totals
    linelist %>% 
      filter(!is.na(outcome) & hospital != "Missing") %>%
      group_by(outcome) %>%                            # Grouped only by outcome, not by hospital    
      summarise(
        N = n(),                                       # Number of rows for whole dataset     
        ct_value = median(ct_blood, na.rm=T))) %>%
  
  mutate(hospital = replace_na(hospital, "Total")) %>%
  
  pivot_wider( 
    values_from = c(N, ct_value), 
    names_from = outcome
  ) %>%
  
  mutate(                                              # Add new columns
    N_Known = N_Death + N_Recover,                               # number with known outcome
    Pct_Death = scales::percent(N_Death / N_Known, 0.1),         # percent cases who died (to 1 decimal)
    Pct_Recover = scales::percent(N_Recover / N_Known, 0.1)) %>%    # percent who recovered (to 1 decimal)
  
  select(                                              # Re-order columns
    hospital, N_Known,                                   # Intro columns
    N_Recover, Pct_Recover, ct_value_Recover,            # Recovered columns
    N_Death, Pct_Death, ct_value_Death) %>%            # Death columns

  arrange(N_Known) 

my_table <- flextable(table) 
border_style = officer::fp_border(color="black", width=1)

my_table %>%           # table is piped in from above
  add_header_row(
    top = TRUE,                # New header goes on top of existing header row
    values = c("Hospital",     # Header values for each column below
               "Total cases with known outcome", 
               "Recovered",    # This will be the top-level header for this and two next columns
               "",
               "",
               "Died",         # This will be the top-level header for this and two next columns
               "",             # Leave blank, as it will be merged with "Died"
               "")) %>% 
  set_header_labels(         # Rename the columns in original header row
    hospital = "", 
    N_Known = "",                  
    N_Recover = "Total",
    Pct_Recover = "% of cases",
    ct_value_Recover = "Median CT values",
    N_Death = "Total",
    Pct_Death = "% of cases",
    ct_value_Death = "Median CT values")  %>% 
  merge_at(i = 1, j = 3:5, part = "header") %>% # Horizontally merge columns 3 to 5 in new header row
  merge_at(i = 1, j = 6:8, part = "header") %>%  
  border_remove() %>%  
  theme_booktabs() %>% 
  vline(part = "all", j = 2, border = border_style) %>%   # at column 2 
  vline(part = "all", j = 5, border = border_style) %>%   # at column 5
  merge_at(i = 1:2, j = 1, part = "header") %>% 
  merge_at(i = 1:2, j = 2, part = "header") %>% 
  width(j=1, width = 2.7) %>% 
  width(j=2, width = 1.5) %>% 
  width(j=c(4,5,7,8), width = 1) %>% 
  flextable::align(., align = "center", j = c(2:8), part = "all") %>% 
  bg(., part = "body", bg = "gray95")  %>% 
  bg(., j=c(1:8), i= ~ hospital == "Military Hospital", part = "body", bg = "#91c293") %>% 
  colformat_num(., j = c(4,7), digits = 1) %>%
  bold(i = 1, bold = TRUE, part = "header") %>% 
  bold(i = 7, bold = TRUE, part = "body")



# GG Plot Intro -----------------------------------
ggplot(data = linelist, mapping = aes(x = age, y = wt_kg))+
  geom_point()  

ll_long <- linelist %>%
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

ll_long

ggplot(ll_long, aes(x = value)) +
  geom_histogram(
    bins = 30, 
    fill = "steelblue", 
    binwidth = 7,
    alpha = 0.5) +
  facet_wrap(~ variable, scales = "free")


# scatterplot
ggplot(data = linelist,   # set data
       mapping = aes(     # map aesthetics to column values
         x = age,           # map x-axis to age            
         y = wt_kg,         # map y-axis to weight
         color = age,       # map color to age
         size = bmi))+      # map size to age
  geom_point(             # display data as points      # points display as diamonds
    alpha = 0.3)            # point transparency at 30%


# Age split Charts ---------------------------

# Build a long form table with gender, age_group and age and bmi

ll_bmi_long <- linelist %>%
  mutate(
    age_group = case_when(
      age < 18 ~ "Child",
      age >= 18 ~ "Adult"
    )
  ) %>%
  select(c(gender, age, wt_kg, age_group)) %>%
  pivot_longer(cols = c(age, wt_kg), names_to = "variable", values_to = "value") %>%
  mutate(
    variable = stringr::str_glue("{gender}-{age_group}-{variable}")
  )

ggplot(ll_bmi_long, aes(x = value)) +
  geom_histogram(
    bins = 30, 
    fill = "steelblue", 
    binwidth = 7,
    alpha = 0.5) +
  facet_wrap(~ variable, scales = "free")