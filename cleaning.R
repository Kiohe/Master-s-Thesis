pacman::p_load(tidyverse, here)

setwd("C:/masters_thesis/data/raw")

## Loading Data Adding a Year Variable
dat_name <- dir()
list_year <- 
  c(2015, 2016, 2016, 2017, 2017, 2018, 2018,
    2019, 2019, 2019, 2020, 2020, 2020, 2020,
    2021, 2921, 2022, 2022)

dat_list <- map2(dat_name, list_year, ~ {
  read_csv(.x) |> 
    mutate(year = .y)
})

object_names <- 
  str_remove(dat_name, "\\.csv$") %>%
  paste0("dat_", .)

list2env(set_names(dat_list, object_names), envir = .GlobalEnv)

dat_2017 <- 
  left_join(dat_2017, dat_2017special, by = "PKEY")

dat_2018 <- 
  left_join(dat_2018, dat_2018special, by = "PKEY")

dat_2019 <- 
  left_join(dat_2019, dat_2019special, by = "PKEY")

dat_2020 <- 
  left_join(dat_2020, dat_2020special, by = "pkey")

dat_2021 <- 
  left_join(dat_2021, dat_2021special, by = "pkey")

dat_2022 <- 
  left_join(dat_2022, dat_2022special, by = "pkey")

dat_2023 <- 
  left_join(dat_2023, dat_2023special, by = "pkey")

# bind_rows()

## Selecting Variables
## (ID, year, age, wage, traingng, contract type, sex, education)

dat_2016_basic <-
dat_2016 |>
select(
  ID = PKEY,
  year = year,
  contract_type = Q18,
  sex = Q1,
  age = Q2,
  educ = Q12,
  hourly_wage = Q38,
  learning_gotoschool = Q47_1,
  learning_seminar = Q47_2,
  learning_home = Q47_3,
  learning_e= Q47_4,
  learning_books = Q47_5,
  learning_internet = Q47_6,
  learning_listen = Q47_7,
  learning_nothing = Q47_8,
  OJT = Q54,
  OFF_JT = Q55,
  annual_wage_main = Q85_1,
  working_day_weekly = Q34_1,
  working_hour_weekly = Q34_2
    )

dat_2017_basic <-
  dat_2017 |>
  select(
    ID = PKEY,
    year = year.x,
    sex = Y17_Q1,
    age = Y17_Q2,
    educ = Y17_Q5,
    contract_type = Y17_Q19,
    working_day_weekly = Y17_Q36_1,
    working_hour_weekly = Y17_Q36_2,
    hourly_wage = Y17_Q38,
    learning_gotoschool = Y17_Q52_1,
    learning_seminar = Y17_Q52_2,
    learning_home = Y17_Q52_3,
    learning_e= Y17_Q52_4,
    learning_books = Y17_Q52_5,
    learning_internet = Y17_Q52_6,
    learning_listen = Y17_Q52_7,
    learning_nothing = Y17_Q52_8,
    OJT = Y17_Q59,
    OFF_JT = Y17_Q60,
    annual_wage_main = Y17_Q91_1
  )

dat_2018_basic <-
  dat_2018 |>
  select(
    ID = PKEY,
    year = year.x,
    sex = Y18_Q1,
    age = Y18_Q2,
    educ = Y18_Q5,
    contract_type = Y18_Q19,
    working_day_weekly = Y18_Q40_1,
    working_hour_weekly = Y18_Q40_2,
    hourly_wage = Y18_Q42,
    learning_gotoschool = Y18_Q57_1,
    learning_seminar = Y18_Q57_2,
    learning_home = Y18_Q57_3,
    learning_e= Y18_Q57_4,
    learning_books = Y18_Q57_5,
    learning_internet = Y18_Q57_6,
    learning_listen = Y18_Q57_7,
    learning_nothing = Y18_Q57_8,
    OJT = Y18_Q64,
    OFF_JT = Y18_Q65,
    annual_wage_main = Y18_Q98_1
  )


dat_2019_basic <-
  dat_2019 |>
  select(
    ID = PKEY,
    year = year.x,
    sex = Y19_Q1,
    age = Y19_Q2,
    educ = Y19_Q5,
    contract_type = Y19_Q19,
    working_day_weekly = Y19_Q37_1,
    working_hour_weekly = Y19_Q37_2,
    hourly_wage = Y19_Q45,
    learning_gotoschool = Y19_Q60_1,
    learning_seminar = Y19_Q60_2,
    learning_home = Y19_Q60_3,
    learning_e= Y19_Q60_4,
    learning_books = Y19_Q60_5,
    learning_internet = Y19_Q60_6,
    learning_listen = Y19_Q60_7,
    learning_nothing = Y19_Q60_8,
    OJT = Y19_Q67,
    OFF_JT = Y19_Q68,
    annual_wage_main = Y19_Q100_1
  )

dat_2020_basic <-
  dat_2020 |>
  select(
    ID = pkey,
    year = year.x,
    sex = y20_q1,
    age = y20_q2,
    educ = y20_q5,
    contract_type = y20_q19,
    working_day_weekly = y20_q37_1,
    working_hour_weekly = y20_q37_2,
    hourly_wage = y20_q39,
    learning_gotoschool = y20_q54_1,
    learning_seminar = y20_q54_2,
    learning_home = y20_q54_3,
    learning_e= y20_q54_4,
    learning_books = y20_q54_5,
    learning_internet = y20_q54_6,
    learning_listen = y20_q54_7,
    learning_nothing = y20_q54_8,
    OJT = y20_q61,
    OFF_JT = y20_q62,
    annual_wage_main = y20_q94_1
  )

dat_2021_basic <-
  dat_2021 |>
  select(
    ID = pkey,
    year = year.x,
    sex = y21_q1,
    age = y21_q2,
    educ = y21_q5,
    contract_type = y21_q19,
    working_day_weekly = y21_q37_1,
    working_hour_weekly = y21_q37_2,
    hourly_wage = y21_q45,
    learning_gotoschool = y21_q60_1,
    learning_seminar = y21_q60_2,
    learning_home = y21_q60_3,
    learning_e= y21_q60_4,
    learning_books = y21_q60_5,
    learning_internet = y21_q60_6,
    learning_listen = y21_q60_7,
    learning_nothing = y21_q60_8,
    OJT = y21_q67,
    OFF_JT = y21_q68,
    annual_wage_main = y21_q100_1
  )

dat_2022_basic <-
  dat_2022 |>
  select(
    ID = pkey,
    year = year.x,
    sex = y22_q1,
    age = y22_q2,
    educ = y22_q5,
    contract_type = y22_q19,
    working_day_weekly = y22_q37_1,
    working_hour_weekly = y22_q37_2,
    hourly_wage = y22_q43,
    learning_gotoschool = y22_q58_1,
    learning_seminar = y22_q58_2,
    learning_home = y22_q58_3,
    learning_e= y22_q58_4,
    learning_books = y22_q58_5,
    learning_internet = y22_q58_6,
    learning_listen = y22_q58_7,
    learning_nothing = y22_q58_8,
    OJT = y22_q65,
    OFF_JT = y22_q66,
    annual_wage_main = y22_q100_1
  )

dat_2023_basic <-
  dat_2023 |>
  select(
    ID = pkey,
    year = year.x,
    sex = y23_q1,
    age = y23_q2,
    educ = y23_q5,
    contract_type = y23_q19,
    working_day_weekly = y23_q37_1,
    working_hour_weekly = y23_q37_2,
    hourly_wage = y23_q43,
    learning_gotoschool = y23_q58_1,
    learning_seminar = y23_q58_2,
    learning_home = y23_q58_3,
    learning_e= y23_q58_4,
    learning_books = y23_q58_5,
    learning_internet = y23_q58_6,
    learning_listen = y23_q58_7,
    learning_nothing = y23_q58_8,
    OJT = y23_q65,
    OFF_JT = y23_q66,
    annual_wage_main = y23_q99_1
  )

dat_basic <- 
bind_rows(dat_2016_basic, 
          dat_2017_basic, 
          dat_2018_basic, 
          dat_2019_basic, 
          dat_2020_basic,
          dat_2021_basic,
          dat_2022_basic,
          dat_2023_basic
          )

write_csv(dat_basic, file = here::here("../masters_thesis/data/panel/dat_panel_basic.csv"))
