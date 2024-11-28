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
# 2.	居住地
# 4.	就業状況
# 5.	学歴
# 6.	主な稼ぎ手か
# 7.	卒業学部
# 8.	家族(子供の数)
# 9.	持ち家
# 10.	子どもの年齢
# 11.	職種
# 12.	役職
# 13.	業種
# 14.	従業員規模


## selecting variables
dat_2016_cov <-
  dat_2016 |>
  select(
    ID = PKEY,
    year = year,
    pref = Q4,
    d_marriage = Q5,
    d_child = Q6,
    num_child = Q7,
    first_child_age = Q8_1,
    live_alone = Q9_1,
    main_earner = Q10,
    house_type = Q11,
    last_decem_work = Q16,
    industry = Q28,
    firm_size = Q29,
    occupation = Q30,
    d_term_fixed = Q31,
    position = Q35,
    num_quit = Q43,
    first_contract_type = BQ62,
    first_industry = BQ63,
    first_firm_size = BQ64,
    first_occupation = BQ65,
    grade_midschool = Q82,
    d_quit = Q46_1,
    q_nonreg_reg = Q46_6,
    d_reg_nonreg = Q46_7,
    year_entry_current = QN83_7,
    month_entry_current = QN83_8,
    

    life_satisfuction = Q44,
    mental_tension = Q50_5,
    mental_depressed = Q50_6,
    job_levelup = Q53,
    job_variation = Q58_1,
    job_satisfuction = Q59_1,
    career_perspective = Q59_4,
    
  ) |>
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 36 ~ 8,
      industry >= 37 & industry <= 43 ~ 9,
      industry >= 44 & industry <= 49 ~ 10,
      industry == 50 | industry == 58 | industry == 61 ~ 11,
      
      industry >= 62 & industry <= 63 ~ 12,
      industry >= 51 & industry <= 52 ~ 13,
      industry == 57 | industry == 59 ~ 14,
      industry == 55 ~ 15,
      industry >= 53 & industry <= 54 ~ 16,
      industry == 56 ~ 17,
      industry >= 64 & industry <= 65 | industry == 60 ~ 18,
      industry == 66 ~ 19,
      industry == 67 ~ 20, 
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  ) 
  
           

dat_2017_cov <-
  dat_2017 |>
  select(
    ID = PKEY,
    year = year.x,
    pref = Y17_Q4,
    grad_major = Y17_Q6,
    d_marriage = Y17_Q9,
    d_child = Y17_Q10,
    num_child = Y17_Q11,
    first_child_age = Y17_Q12_1,
    live_alone = Y17_Q14_1,
    main_earner = Y17_Q15,
    house_type = Y17_Q13,
    last_decem_work = Y17_Q17,
    industry = Y17_Q29,
    firm_size = Y17_Q30,
    occupation = Y17_Q31,
    d_term_fixed = Y17_Q33,
    position = Y17_Q32,
    num_quit = Y17_Q46,
    first_contract_type = Y17_BQ71,
    first_industry = Y17_BQ72,
    first_firm_size = Y17_BQ73,
    first_occupation = Y17_BQ74,
    grade_midschool = Y17_BQ89,
    d_quit = Y17_Q50_1,
    q_nonreg_reg = Y17_Q51_5,
    d_reg_nonreg = Y17_Q51_5,
    year_entry_current = Y17_QN90_11,
    month_entry_current =Y17_QN90_12,
    
    life_satisfuction = Y17_Q48,
    mental_tension = Y17_Q55_5,
    mental_depressed = Y17_Q55_6,
    job_levelup = Y17_Q58,
    job_variation = Y17_Q63_1,
    job_satisfuction = Y17_Q64_1,
    career_perspective = Y17_Q64_4
    
  )|> 
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 36 ~ 8,
      industry >= 37 & industry <= 43 ~ 9,
      industry >= 44 & industry <= 49 ~ 10,
      industry == 50 | industry == 58 | industry == 61 ~ 11,
      
      industry >= 62 & industry <= 63 ~ 12,
      industry >= 51 & industry <= 52 ~ 13,
      industry == 57 | industry == 59 ~ 14,
      industry == 55 ~ 15,
      industry >= 53 & industry <= 54 ~ 16,
      industry == 56 ~ 17,
      industry >= 64 & industry <= 65 | industry == 60 ~ 18,
      industry == 66 ~ 19,
      industry == 67 ~ 20, 
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  )

dat_2018_cov <-
  dat_2018 |>
  select(
    ID = PKEY,
    year = year.x,
    pref = Y18_Q4,
    grad_major = Y18_Q6,
    d_marriage = Y18_Q9,
    d_child = Y18_Q10,
    num_child =Y18_Q11,
    first_child_age = Y18_Q12_1,
    live_alone = Y18_Q14_1,
    main_earner = Y18_Q15,
    house_type = Y18_Q13,
    last_decem_work = Y18_Q17,
    industry = Y18_Q33,
    firm_size = Y18_Q34,
    occupation = Y18_Q35,
    d_term_fixed = Y18_Q37,
    position = Y18_Q36,
    num_quit = Y18_Q51,
    first_contract_type = Y18_CQ78,
    first_industry = Y18_CQ79,
    first_firm_size = Y18_CQ80,
    first_occupation = Y18_CQ81,
    grade_midschool = Y18_BQ96,
    d_quit = Y18_Q55_1,
    q_nonreg_reg = Y18_Q56_1,
    d_reg_nonreg = Y18_Q56_2,
    year_entry_current = Y18_QN97_11,
    month_entry_current =Y18_QN97_12,
    
    life_satisfuction = Y18_Q53,
    mental_tension = Y18_Q60_5,
    mental_depressed = Y18_Q60_6,
    job_levelup = Y18_Q63,
    job_variation = Y18_Q68_1,
    job_satisfuction = Y18_Q69_1,
    career_perspective = Y18_Q69_4
    
  )|> 
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 36 ~ 8,
      industry >= 37 & industry <= 43 ~ 9,
      industry >= 44 & industry <= 49 ~ 10,
      industry == 50 | industry == 58 | industry == 61 ~ 11,
      
      industry >= 62 & industry <= 63 ~ 12,
      industry >= 51 & industry <= 52 ~ 13,
      industry == 57 | industry == 59 ~ 14,
      industry == 55 ~ 15,
      industry >= 53 & industry <= 54 ~ 16,
      industry == 56 ~ 17,
      industry >= 64 & industry <= 65 | industry == 60 ~ 18,
      industry == 66 ~ 19,
      industry == 67 ~ 20, 
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  )

dat_2019_cov <-
  dat_2019 |>
  select(
    ID = PKEY,
    year = year.x,
    pref = Y19_Q4,
    grad_major = Y19_Q6,
    d_marriage = Y19_Q9,
    d_child = Y19_Q10,
    num_child = Y19_Q11,
    first_child_age = Y19_Q12_1,
    live_alone = Y19_Q14_1,
    main_earner = Y19_Q15,
    house_type = Y19_Q13,
    last_decem_work = Y19_Q17,
    industry = Y19_Q30,
    firm_size = Y19_Q31,
    occupation = Y19_Q32,
    d_term_fixed = Y19_Q34,
    position = Y19_Q33,
    num_quit = Y19_Q53,
    first_contract_type = Y19_BQ80,
    first_industry = Y19_BQ81,
    first_firm_size = Y19_BQ82,
    first_occupation = Y19_BQ83,
    grade_midschool = Y19_BQ98,
    regular_same = Y19_Q70_7,
    d_quit = Y19_Q59_1,
    q_nonreg_reg = Y19_Q59_5,
    d_reg_nonreg = Y19_Q59_6,
    year_entry_current = Y19_QN99_11,
    month_entry_current =Y19_QN99_12,
    
    life_satisfuction = Y19_Q57,
    mental_tension = Y19_Q63_5,
    mental_depressed = Y19_Q63_6,
    job_levelup = Y19_Q66,
    job_variation = Y19_Q71_1,
    job_satisfuction = Y19_Q72_1,
    career_perspective = Y19_Q72_4
    
  )|> 
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 36 ~ 8,
      industry >= 37 & industry <= 43 ~ 9,
      industry >= 44 & industry <= 49 ~ 10,
      industry == 50 | industry == 58 | industry == 61 ~ 11,
      
      industry >= 62 & industry <= 63 ~ 12,
      industry >= 51 & industry <= 52 ~ 13,
      industry == 57 | industry == 59 ~ 14,
      industry == 55 ~ 15,
      industry >= 53 & industry <= 54 ~ 16,
      industry == 56 ~ 17,
      industry >= 64 & industry <= 65 | industry == 60 ~ 18,
      industry == 66 ~ 19,
      industry == 67 ~ 20, 
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  )

dat_2020_cov <-
  dat_2020 |>
  select(
    ID = pkey,
    year = year.x,
    pref = y20_q4,
    grad_major = y20_q6,
    d_marriage = y20_q9,
    d_child = y20_q10,
    num_child = y20_q11,
    first_child_age = y20_q12_1,
    live_alone = y20_q14_1,
    main_earner = y20_q15,
    house_type = y20_q13,
    last_decem_work = y20_q17,
    industry = y20_q30,
    firm_size = y20_q31,
    occupation = y20_q32,
    d_term_fixed = y20_q34,
    position = y20_q33,
    num_quit = y20_q49,
    first_contract_type = y20_bq74,
    first_industry = y20_bq75,
    first_firm_size = y20_bq76,
    first_occupation = y20_bq77,
    grade_midschool = y20_bq92,
    regular_same = y20_q64_7,
    d_quit = y20_q53_1,
    q_nonreg_reg = y20_q53_5,
    d_reg_nonreg = y20_q53_6,
    year_entry_current = y20_qn93_11,
    month_entry_current = y20_qn93_12,
    
    life_satisfuction = y20_q51,
    mental_tension = y20_q57_5,
    mental_depressed = y20_q57_6,
    job_levelup = y20_q60,
    job_variation = y20_q65_1,
    job_satisfuction = y20_q66_1,
    career_perspective = y20_q66_4
    
  )|> 
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 36 ~ 8,
      industry >= 37 & industry <= 43 ~ 9,
      industry >= 44 & industry <= 49 ~ 10,
      industry == 50 | industry == 58 | industry == 61 ~ 11,
      
      industry >= 62 & industry <= 63 ~ 12,
      industry >= 51 & industry <= 52 ~ 13,
      industry == 57 | industry == 59 ~ 14,
      industry == 55 ~ 15,
      industry >= 53 & industry <= 54 ~ 16,
      industry == 56 ~ 17,
      industry >= 64 & industry <= 65 | industry == 60 ~ 18,
      industry == 66 ~ 19,
      industry == 67 ~ 20, 
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  )

dat_2021_cov <-
  dat_2021 |>
  select(
    ID = pkey,
    year = year.x,
    pref = y21_q4,
    grad_major = y21_q6,
    d_marriage = y21_q9,
    d_child = y21_q10,
    num_child = y21_q11,
    first_child_age = y21_q12_1,
    live_alone = y21_q14_1,
    main_earner = y21_q15,
    house_type = y21_q13,
    last_decem_work = y21_q17,
    industry = y21_q30,
    firm_size = y21_q31,
    occupation = y21_q32,
    d_term_fixed = y21_q34,
    position = y21_q33,
    num_quit = y21_q55,
    first_contract_type = y21_bq80,
    first_industry = y21_bq81,
    first_firm_size = y21_bq82,
    first_occupation = y21_bq83,
    grade_midschool = y21_bq98,
    d_quit = y21_q59_1,
    q_nonreg_reg = y21_q59_5,
    d_reg_nonreg = y21_q59_6,
    year_entry_current = y21_qn99_11,
    month_entry_current = y21_qn99_12,
    
    life_satisfuction = y21_q57,
    mental_tension = y21_q63_5,
    mental_depressed = y21_q63_6,
    job_levelup = y21_q66,
    job_variation = y21_q71_1,
    job_satisfuction = y21_q72_1,
    career_perspective = y21_q72_4
    
  )|> 
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 36 ~ 8,
      industry >= 37 & industry <= 43 ~ 9,
      industry >= 44 & industry <= 49 ~ 10,
      industry == 50 | industry == 58 | industry == 61 ~ 11,
      
      industry >= 62 & industry <= 63 ~ 12,
      industry >= 51 & industry <= 52 ~ 13,
      industry == 57 | industry == 59 ~ 14,
      industry == 55 ~ 15,
      industry >= 53 & industry <= 54 ~ 16,
      industry == 56 ~ 17,
      industry >= 64 & industry <= 65 | industry == 60 ~ 18,
      industry == 66 ~ 19,
      industry == 67 ~ 20, 
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  )

dat_2022_cov <-
  dat_2022 |>
  select(
    ID = pkey,
    year = year.x,
    pref = y22_q4,
    grad_major = y22_q6,
    d_marriage = y22_q9,
    d_child = y22_q10,
    num_child = y22_q11,
    first_child_age = y22_q12_1,
    live_alone = y22_q14_1,
    main_earner = y22_q15,
    house_type = y22_q13,
    last_decem_work = y22_q17,
    industry = y22_q30,
    firm_size = y22_q31,
    occupation = y22_q32,
    d_term_fixed = y22_q34,
    position = y22_q33,
    num_quit = y22_q53,
    first_contract_type = y22_bq80,
    first_industry = y22_bq81,
    first_firm_size = y22_bq82,
    first_occupation = y22_bq83,
    grade_midschool = y22_bq98,
    d_quit = y22_q57_1,
    q_nonreg_reg = y22_q57_5,
    d_reg_nonreg = y22_q57_6,
    year_entry_current = y22_qn99_11,
    month_entry_current = y22_qn99_12,
    
    life_satisfuction = y22_q55,
    mental_tension = y22_q61_5,
    mental_depressed = y22_q61_6,
    job_levelup = y22_q64,
    job_variation = y22_q69_1,
    job_satisfuction = y22_q70_1,
    career_perspective = y22_q70_4
    
  )|> 
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 36 ~ 8,
      industry >= 37 & industry <= 43 ~ 9,
      industry >= 44 & industry <= 49 ~ 10,
      industry == 50 | industry == 58 | industry == 61 ~ 11,

      industry >= 62 & industry <= 63 ~ 12,
      industry >= 51 & industry <= 52 ~ 13,
      industry == 57 | industry == 59 ~ 14,
      industry == 55 ~ 15,
      industry >= 53 & industry <= 54 ~ 16,
      industry == 56 ~ 17,
      industry >= 64 & industry <= 65 | industry == 60 ~ 18,
      industry == 66 ~ 19,
      industry == 67 ~ 20, 
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  )

dat_2023_cov <-
  dat_2023 |>
  select(
    ID = pkey,
    year = year.x,
    pref = y23_q4,
    grad_major = y23_q6,
    d_marriage = y23_q9,
    d_child = y23_q10,
    num_child = y23_q11,
    first_child_age = y23_q12_1,
    live_alone = y23_q14_1,
    main_earner = y23_q15,
    house_type = y23_q13,
    last_decem_work = y23_q17,
    industry = y23_q30,
    firm_size = y23_q31,
    occupation = y23_q32,
    d_term_fixed = y23_q34,
    position = y23_q33,
    num_quit = y23_q53,
    first_contract_type = y23_bq79,
    first_industry = y23_bq80_1,
    first_industry_2 =y23_bq80_2,
    first_firm_size = y23_bq81,
    first_occupation = y23_bq82_1,
    first_occupation_2 = y23_bq82_2,
    grade_midschool = y23_bq97,
    d_quit = y23_q57_1,
    q_nonreg_reg = y23_q57_5,
    d_reg_nonreg = y23_q57_6,
    year_entry_current = y23_qn98_11,
    month_entry_current = y23_qn98_12,
    
    life_satisfuction = y23_q55,
    mental_tension = y23_q61_5,
    mental_depressed = y23_q61_6,
    job_levelup = y23_q64,
    job_variation = y23_q70_1,
    job_satisfuction = y23_q71_1,
    career_perspective = y23_q71_4
    
  )|> 
  mutate(
    coarse_industry = case_when(
      industry == 1 ~ 1,
      industry == 2 ~ 3,
      industry >= 3 & industry <= 5 ~ 4,
      industry >= 6 & industry <= 25 ~ 5,
      industry == 26 ~ 6,
      industry >= 27 & industry <= 31 ~ 7,
      industry >= 32 & industry <= 39 ~ 8,
      industry >= 40 & industry <= 46 ~ 9,
      industry >= 47 & industry <= 52 ~ 10,
      industry == 53 | industry == 63 | industry == 64 ~ 11,
      industry >= 66 & industry <= 68 ~ 12,
      industry >= 58 & industry <= 59 ~ 13,
      industry >= 60 & industry <= 62 ~ 14,
      industry == 56 | industry == 57 ~ 15,
      industry >= 54 & industry <= 55 ~ 16,
      industry == 69 | industry == 70 ~ 17,
      industry >= 71 & industry <= 73 | industry == 65 ~ 18,
      industry == 74 | industry == 75 ~ 19,
      industry == 76 ~ 20,  ##
    ),
    life_satisfuction = if_else(life_satisfuction <= 2, 1, 0),
    mental_tension = if_else(mental_tension >= 4, 1, 0),
    mental_depressed = if_else(mental_depressed >= 4, 1, 0),
    job_levelup = case_when(job_levelup <= 2 ~ 1,
                            job_levelup >= 3 & job_levelup <= 5 ~ 0,
                            job_levelup == 6 ~ NA),
    job_variation = if_else(job_variation <= 2, 1, 0),
    job_satisfuction = if_else(job_satisfuction <= 2, 1, 0),
    career_perspective = if_else(career_perspective <= 2, 1, 0)
  )

## cleaning data
dat_2016_cov <- 
  dat_2016_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
    )

dat_2017_cov <- 
  dat_2017_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
  )

dat_2018_cov <- 
  dat_2018_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
  )

dat_2019_cov <- 
  dat_2019_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
  )

dat_2020_cov <- 
  dat_2020_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
  )

dat_2021_cov <- 
  dat_2021_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
  )

dat_2022_cov <- 
  dat_2022_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
  )

dat_2023_cov <- 
  dat_2023_cov |>
  filter(firm_size < 14,
         d_term_fixed < 3) |> 
  mutate(
    d_marriage = if_else(d_marriage == 1, 1, 0),
    d_child = if_else(d_child == 1, 1, 0),
    d_hokkaido = if_else(pref == 1, 1, 0),
    d_tohoku = if_else(pref >= 2 & pref <= 7, 1, 0),
    d_kanto = if_else(pref >= 8 & pref <= 14, 1, 0),
    d_hokuriku = if_else(pref >= 15 & pref <= 18, 1, 0),
    d_koshinetsu = if_else(pref >= 19 & pref <= 20, 1, 0),
    d_tokai = if_else(pref >= 21 & pref <= 23, 1, 0),
    d_kinki = if_else(pref >= 24 & pref <= 30, 1, 0),
    d_sanin = if_else(pref >= 31 & pref <= 32, 1, 0),
    d_sanyo = if_else(pref >= 33 & pref <= 35, 1, 0),
    d_shikoku = if_else(pref >= 36 & pref <= 39, 1, 0),
    d_kyushu = if_else(pref >= 40 & pref <= 46, 1, 0),
    d_okinawa = if_else(pref == 47, 1, 0),
    d_oversea = if_else(pref == 48, 1, 0),
    d_main_earner = if_else(main_earner == 1, 1, 0),
    d_house_possesion = if_else(house_type <= 2, 1, 0),
    d_last_decem_work_main = if_else(last_decem_work <= 2, 1, 0),
    d_last_decem_work_atleast = if_else(last_decem_work <= 4, 1, 0),
    d_last_decem_work_nojob = if_else(last_decem_work <= 7, 1, 0),
    d_public_sector = if_else(firm_size == 13, 1, 0),
  )

dat_cov <- 
  bind_rows(dat_2016_cov, 
            dat_2017_cov, 
            dat_2018_cov, 
            dat_2019_cov, 
            dat_2020_cov,
            dat_2021_cov,
            dat_2022_cov,
            dat_2023_cov
  )

dat_cov <- 
  dat_cov |> 
  filter(year_entry_current < 99999,
         month_entry_current < 999)

dat_cov <- dat_cov |> 
  fastDummies::dummy_cols(
    select_columns = "year_entry_current",
    remove_first_dummy = FALSE,
    ignore_na = TRUE,
    split = FALSE
  )

write_csv(dat_cov, file = here::here("../masters_thesis/data/panel/dat_panel_cov.csv"))
