# Script 02 - Data analyses - logistic regression  ----

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
regr_logi <- logistic_reg() %>%
  set_engine('glm') %>%
  set_mode('classification') %>%
  fit(formula = payr_stat ~ chec_acco + dura_mont + cred_hist + purp_loan + 
        cred_amou + savi_bond + inst_rate +  stat_asex + 
        debt_guar + inst_plan + fore_work, 
      data = data_trai)

tidy(regr_logi) %>% 
  print(n = 50)

tidy(regr_logi, 
     exponentiate = TRUE) %>% 
  print(n = 50)

# Predictions 
pred_clas <- predict(regr_logi,
                     new_data = data_test,
                     type = 'class') %>% 
  glimpse()

pred_prob <- predict(regr_logi,
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

roc_data1 <- roc_curve(modl_rslt, 
                      truth = payr_stat, 
                      .pred_1) %>% 
  mutate(model = 'Logistic Regression') %>% 
  glimpse()
                      

roc_data %>%  
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(col='tomato',lwd = 1) +
  geom_abline(linetype = 3) + 
  coord_equal()+
  theme_bw()