# Script 07 - Data viz  ----
roc_plot <- rbind(roc_data1, roc_data2, roc_data3, roc_data4, roc_data5)

roc_plot


f1 <- roc_plot %>%  
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(lwd = 1.05) +
  geom_abline(linetype = 3, lwd = 1.05) + 
  coord_equal()+
  scale_color_viridis_d(labels = c('Decision Tree (0.71)',
                                   'KNN (0.72)',
                                   'Logistic Regression (0.76)',
                                   'Random Forest (0.76)',
                                   'SVM (0.78)'))+
  labs(x = '1 - Specificity', y = 'Sensivity', color = 'Model')+
  theme_bw()+
  theme(axis.title = element_text(face = 'bold', size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(face = 'bold', size = 12),
        legend.text =  element_text(size = 10),
        legend.background =  element_blank(),
        legend.box  =  element_blank(),
        legend.position = c(.160, .875))
f1

ggsave(plot = f1,
       'figura1_roc.png',
       width = 18,
       height = 18,
       units = 'cm',
       dpi = 600)

metrics <- read.csv('Data/models_metrics.csv', sep = ';') %>% 
  dplyr::filter(metric != 'ROC AUC') %>% 
  group_by(metric) %>%
  arrange(metric, desc(value)) %>%
  glimpse()

f2 <- metrics %>%  
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_bar(stat = 'identity')+
  scale_fill_viridis_d()+
  coord_flip()+
  labs(x = 'Model', y = 'Value', fill = 'Model')+
  facet_wrap(~metric, scales = 'free')+
  theme_bw()+
  theme(axis.title = element_text(face = 'bold', size = 14),
        axis.text = element_text(size = 12),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(face = 'bold', size = 14),
        legend.title = element_text(face = 'bold', size = 12),
        legend.text =  element_text(size = 10),
        legend.position = 'bottom')

f2

ggsave(plot = f2,
       'figura2_met.png',
       width = 27,
       height = 18,
       units = 'cm',
       dpi = 600)