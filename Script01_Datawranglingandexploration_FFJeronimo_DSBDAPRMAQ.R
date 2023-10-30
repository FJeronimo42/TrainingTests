# Script 01 - Data wrangling and exploration ----

# Library ---- 
pacman::p_load(DataExplorer, skimr, MuMIn, SmartEDA, tidymodels, tidyverse)

# Data wrangling ----
germ_data <- read.table('Data/german.data', sep = '', head = F) %>% 
  rename('chec_acco' = V1,
         'dura_mont' = V2,
         'cred_hist' = V3,
         'purp_loan' = V4,
         'cred_amou' = V5,
         'savi_bond' = V6,
         'pres_empl' = V7,
         'inst_rate' = V8,
         'stat_asex' = V9,
         'debt_guar' = V10,
         'pres_resi' = V11,
         'prop_good' = V12,
         'agei_year' = V13,
         'inst_plan' = V14,
         'hous_type' = V15,
         'cred_bank' = V16,
         'jobs_type' = V17,
         'peop_liab' = V18,
         'tele_regi' = V19,
         'fore_work' = V20,
         'payr_stat' = V21) %>% 
  mutate(payr_stat = as.factor(payr_stat)) %>% 
  mutate(payr_stat = recode(payr_stat,
                            '2' = '0')) %>% 
  glimpse()

# Data exploration ----
ExpReport(data = germ_data,
          op_file = 'germ_data.html')

create_report(data = germ_data)

summary(germ_data)

skim(germ_data)

# Logistic regression
mdls_rslt <- glm(data = germ_data, 
                 formula = payr_stat ~ .,
                 family = binomial(link = 'logit'))
summary(mdls_rslt)

mdls_fitt <- glm(data = germ_data, 
                 formula = payr_stat ~
                   chec_acco + dura_mont + cred_hist + purp_loan + 
                   cred_amou + savi_bond + inst_rate +  stat_asex+
                   debt_guar + inst_plan + fore_work,
                 # hous_type + prop_good + pres_empl
                 family = binomial(link = 'logit'))
summary(mdls_fitt)

options(na.action = 'na.fail')
#dredge(mdls_rslt, m.lim = c(0, 10))
