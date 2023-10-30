# Script 04 - Data analyses - random forest  ----

# Library ----
pacman::p_load(tidymodels, tidyverse)

# Data split ----
set.seed(42)

data_split <- initial_split(germ_data,
                            prop = 0.75,
                            strata = payr_stat) %>% 
  glimpse()

# Training data
data_trai <- data_split %>%
  training() %>% 
  glimpse()

# Test data
data_test <- data_split %>%
  testing() %>% 
  glimpse()

# ML logistic regression model ----
# Model
show_engines('rand_forest')
require('kknn')

rand_frst <- rand_forest() %>%
  set_engine('ranger') %>%
  set_mode('classification') %>%
  fit(formula = payr_stat ~ chec_acco + dura_mont + cred_hist + purp_loan + 
        cred_amou + savi_bond + inst_rate +  stat_asex + 
        debt_guar + inst_plan + fore_work, 
      data = data_trai)

# tidy(rand_frst) %>% 
#   print(n = 50)
# 
# tidy(rand_frst, 
#      exponentiate = TRUE) %>% 
#   print(n = 50)

# Predictions 
pred_clas <- predict(rand_frst,
                     new_data = data_test,
                     type = 'class') %>% 
  glimpse()

pred_prob <- predict(rand_frst,
                     new_data = data_test,
                     type = 'prob') %>% 
  glimpse()

modl_rslt <- data_test %>%
  select(payr_stat) %>%
  bind_cols(pred_clas, pred_prob) %>% 
  glimpse()

# Performance metrics ----
conf_mat(data = modl_rslt, 
         truth = payr_stat,
         estimate = .pred_class)

perf_metr <- metric_set(yardstick::accuracy,
                        yardstick::mcc, 
                        yardstick::sens, 
                        yardstick::spec, 
                        yardstick::precision, 
                        yardstick::f_meas)

perf_metr(data = modl_rslt, 
          truth = payr_stat,
          estimate = .pred_class)  

# ROC
roc_auc(modl_rslt, 
        truth = payr_stat,
        .pred_1) 

roc_data3 <- roc_curve(modl_rslt, 
                      truth = payr_stat, 
                      .pred_1) %>% 
  mutate(model = 'Random Forest') %>% 
  glimpse()

roc_data %>%  
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(col='tomato',lwd = 1) +
  geom_abline(linetype = 3) + 
  coord_equal()+
  theme_bw()

