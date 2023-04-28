

library(tidyverse)

loans <- read_csv("data/lending_club_loans.csv")

loans_trim <- loans %>% 
  select(loan_amnt:int_rate,
         sub_grade,
         emp_length:loan_status,
         purpose,
         dti,
         earliest_cr_line:inq_last_6mths,
         open_acc:revol_util,
         pub_rec_bankruptcies)

loans_clean <- loans_trim %>% 
  filter(loan_status %in% c("Charged Off", "Default", "Fully Paid",
                            "In Grace Period", "Late (16-30 days)",
                            "Late (31-120 days)	")) %>%
  mutate(loan_paid_flag = as.factor(
    case_when(loan_status %in% c("Fully Paid", "In Grace Period") ~ 1, 
              TRUE ~ 0)), .before = loan_amnt) %>% 
  select(-loan_status) %>% 
  mutate(term_mths = as.factor(case_when(term == "36 months" ~ as.numeric(36),
                               term == "60 months" ~ as.numeric(60),
                               TRUE ~ NA)), .before = term) %>% 
  select(-term) %>%
  mutate(int_rate = as.numeric(str_remove(int_rate, "%"))) %>%
  mutate(sub_grade = as.factor(str_remove(sub_grade, pattern = "[0-9]+"))) %>% 
  mutate(emp_length = as.factor(
    case_when(emp_length %in% c("< 1 year", "1 year", "2 years") ~ "0-2 years",
              emp_length %in% c("3 years", "4 years", "5 years") ~ "3-5 years",
              emp_length %in% c("6 years", "8 years", "9 years") ~ "6-9 years",
              TRUE ~ "10+ years"))) %>%
  mutate(home_ownership = as.factor(home_ownership)) %>% 
  mutate(income_verification_flag = as.factor(case_when(
    verification_status %in% c("Verified", "Source Verified") ~ 1,
    TRUE ~ 0
  )), .after = verification_status) %>% 
  select(-verification_status) %>% 
  mutate(issue_d = dmy(str_c("1", issue_d, sep = "-"))) %>% 
  mutate(earliest_cr_line = dmy(str_c("1", earliest_cr_line, sep = "-"))) %>% 
  mutate(revol_util = as.numeric(str_remove(revol_util, "%"))) %>% 
  mutate(revol_util = replace_na(revol_util, median(revol_util, na.rm = TRUE))) %>% 
  mutate(pub_rec_bankruptcies = replace_na(pub_rec_bankruptcies, median(pub_rec_bankruptcies, na.rm = TRUE)))
  
  

loans_model_data <- loans_clean %>% 
  mutate(loan_amnt = log10(loan_amnt),
         funded_amnt = log10(funded_amnt),
         int_rate = log10(int_rate),
         annual_inc = log10(annual_inc),
         open_acc = log10(open_acc)) %>% 
  mutate(loan_paid_flag = as.factor(case_when(loan_paid_flag == 1 ~ "Yes",
                                              loan_paid_flag == 0 ~ "No"))) %>% 
  mutate(purpose = as.factor(purpose)) %>% 
  select(-issue_d, -earliest_cr_line) %>% 
  filter(fico_range_low >= 300,
         fico_range_low <= 850,
         fico_range_high >= 300,
         fico_range_high <= 850) %>% 
  drop_na()
  
  

loans_trim_viz <- loans %>% 
  select(-emp_title,
         -title,
         -url,
         -desc,
         -zip_code,
         -next_pymnt_d,
         -mths_since_last_major_derog,
         -annual_inc_joint,
         -dti_joint,
         -verification_status_joint,
         -tot_coll_amt,
         -tot_cur_bal,
         -open_acc_6m,
         -open_il_6m,
         -open_il_12m,
         -open_il_24m,
         -mths_since_rcnt_il,
         -total_bal_il,
         -il_util,
         -open_rv_12m,
         -open_rv_24m,
         -max_bal_bc,
         -all_util,
         -total_rev_hi_lim,
         -inq_fi,
         -total_cu_tl,
         -inq_last_12m,
         -acc_open_past_24mths,
         -avg_cur_bal,
         -bc_open_to_buy,
         -bc_util,
         -mo_sin_old_il_acct,
         -mo_sin_old_rev_tl_op,
         -mo_sin_rcnt_rev_tl_op,
         -mo_sin_rcnt_tl,
         -mort_acc,
         -mths_since_recent_bc,
         -mths_since_recent_bc_dlq,
         -mths_since_recent_inq,
         -mths_since_recent_revol_delinq,
         -num_accts_ever_120_pd,
         -num_actv_bc_tl,
         -num_actv_rev_tl,
         -num_bc_tl,
         -num_il_tl,
         -num_bc_sats,
         -num_op_rev_tl,
         -num_rev_accts,
         -num_rev_tl_bal_gt_0,
         -num_sats,
         -num_tl_120dpd_2m,
         -num_tl_30dpd,
         -num_tl_90g_dpd_24m,
         -num_tl_op_past_12m,
         -pct_tl_nvr_dlq,
         -percent_bc_gt_75,
         -tot_hi_cred_lim,
         -total_bal_ex_mort,
         -total_bc_limit,
         -total_il_high_credit_limit,
         -mths_since_last_record,
         -mths_since_last_delinq,
         -initial_list_status,
         -application_type,
         -policy_code,
         -collections_12_mths_ex_med,
         -acc_now_delinq,
         -recoveries,
         -collection_recovery_fee,
         -chargeoff_within_12_mths,
         -delinq_amnt,
         -tax_liens,
         -id,
         -member_id,
         -out_prncp_inv,
         -pymnt_plan,
         -out_prncp,
         -total_rec_late_fee
  ) %>% 
  filter(loan_status %in% c("Charged Off", "Default", "Fully Paid",
                            "In Grace Period", "Late (16-30 days)",
                            "Late (31-120 days)	")) %>% 
  drop_na()


loans_clean_viz <- loans_trim_viz %>% 
  mutate(term_mths = as.factor(case_when(term == "36 months" ~ as.numeric(36),
                               term == "60 months" ~ as.numeric(60),
                               TRUE ~ NA)) )%>% 
  select(-term) %>% 
  mutate(int_rate = as.numeric(str_remove(int_rate, "%"))) %>% 
  mutate(emp_length = as.factor(
    case_when(emp_length %in% c("< 1 year", "1 year", "2 years") ~ "0-2 years",
              emp_length %in% c("3 years", "4 years", "5 years") ~ "3-5 years",
              emp_length %in% c("6 years", "8 years", "9 years") ~ "6-9 years",
              TRUE ~ "10+ years"))) %>% 
  mutate(sub_grade = as.factor(str_remove(sub_grade, pattern = "[0-9]+"))) %>% 
  mutate(income_verification_flag = as.factor(case_when(
    verification_status %in% c("Verified", "Source Verified") ~ 1,
    TRUE ~ 0
  )), .after = verification_status) %>% 
  select(-verification_status) %>% 
  mutate(loan_paid_flag = as.factor(
    case_when(loan_status %in% c("Fully Paid", "In Grace Period") ~ "Yes", 
              TRUE ~ "No")), .before = loan_amnt) %>% 
  select(-loan_status) %>% 
  mutate(issue_d = dmy(str_c("1", issue_d, sep = "-"))) %>% 
  mutate(earliest_cr_line = dmy(str_c("1", earliest_cr_line, sep = "-"))) %>% 
  mutate(revol_util = as.numeric(str_remove(revol_util, "%"))) %>% 
  mutate(revol_util = replace_na(revol_util, median(revol_util, na.rm = TRUE))) %>% 
  mutate(pub_rec_bankruptcies = replace_na(pub_rec_bankruptcies, median(pub_rec_bankruptcies, na.rm = TRUE))) %>% 
  mutate(last_credit_pull_d = dmy(str_c("1", last_credit_pull_d, sep = "-"))) %>% 
  mutate(last_pymnt_d = dmy(str_c("1", last_pymnt_d, sep = "-"))) %>% 
  mutate(purpose = as.factor(purpose)) %>% 
  mutate(home_ownership = as.factor(home_ownership)) %>% 
  filter(fico_range_low >= 300,
         fico_range_low <= 850,
         fico_range_high >= 300,
         fico_range_high <= 850,
         last_fico_range_low >= 300,
         last_fico_range_low <= 850,
         last_fico_range_high >= 300,
         last_fico_range_high <= 850) %>% 
  mutate(profit = (total_pymnt - loan_amnt)) %>% 
  filter(loan_amnt >= total_rec_prncp)






