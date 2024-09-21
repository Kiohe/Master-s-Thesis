pacman::p_load(tidyverse, here)

dat_basic <- read_csv(here::here("data/panel/dat_panel_basic.csv"))

## clean basic variables
dat_basic <- 
  dat_basic |> 
  mutate(
    born_year = year - age,
    sex = if_else(sex == 2, 1, 0)
  )

## make dummy variables for each contract type
dat_basic <- 
  dat_basic |> 
  mutate(
    regular = case_when(contract_type == 1 ~ 1, TRUE ~ 0),  
    part_time = case_when(contract_type == 2 ~ 1, TRUE ~ 0), 
    dispatched = case_when(contract_type == 3 ~ 1, TRUE ~ 0),
    fixed_term = case_when(contract_type == 4 ~ 1, TRUE ~ 0),
    shokutaku = case_when(contract_type == 5 ~ 1, TRUE ~ 0),
    other_emp = case_when(contract_type == 6 ~ 1, TRUE ~ 0),
  )

## make dummy variables for education level
dat_basic <- 
  dat_basic |> 
  filter(educ <= 8) |> 
  mutate(edu_PhD = case_when(educ == 8 ~ 1, TRUE ~ 0),
         edu_grad = case_when(educ >= 7 ~ 1, TRUE ~ 0), #master or above
         edu_cg = case_when(educ == 6 ~ 1, TRUE ~ 0),
         edu_hs = case_when(educ == 2 ~ 1, TRUE ~ 0),
         edu_vocat = case_when(educ == 3 ~ 1, TRUE ~ 0),
         edu_shortcg = case_when(educ == 4 ~ 1, TRUE ~ 0),
         edu_kosen = case_when(educ == 5 ~ 1, TRUE ~ 0)
         )

# Transform Hourly wage
dat_basic <-
  dat_basic |>
  filter(annual_wage_main < 99999,
         working_hour_weekly < 9999,
         working_day_weekly < 99) |> 
  mutate(working_hour_annualy = 52 * working_hour_weekly,
         wage_per_hour = 10000 * annual_wage_main / working_hour_annualy,
         logwage = log(wage_per_hour))

# mean(dat_basic$wage_per_hour, na.rm = TRUE)

## Training status
dat_basic <- 
  dat_basic |> 
  mutate(d_OJT = if_else(OJT < 5, 1, 0),
         d_OFF_JT = if_else(OFF_JT > 2, 1, 0)
         )

##書き出し  
write_csv(dat_basic, file = here::here("data/panel/dat_basic.csv"))
