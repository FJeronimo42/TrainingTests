# Universidade Federal do Paraná
# BB087 - REcologia Vegetal (2024) - Profa. Isabela Galarda Varassin
# Aula: Análise Descritiva de Dados
# Prof. Fernando Fortunato Jeronimo (PPG Botânica UFPR)
# Código criado por Fernando F. Jeronimo e revisado com Microsoft Copilot

# Determinar diretório de trabalho
setwd('caminho/para/a/pasta')
getwd()

# Instalação dos pacotes necessários
install.packages('tidyverse')
install.packages('rstatix')
install.packages('esquisse')
install.packages('scales')

# Carregando pacotes necessários
library(tidyverse)
library(rstatix)
library(esquisse)
library(scales)

# Carregando os dados
# Dados Iris
iris_data <- iris %>% 
  as.data.frame() %>% 
  glimpse()

# CSV ou TXT
meus_dados <- read.csv('nome_do_arquivo.csv') %>% 
  glimpse()

meus_dados <- read.csv('nome_do_arquivo.csv', 
                       head = T, 
                       sep = '') %>% 
  glimpse()

# Computar estatistica descritiva
summary(iris_data)

stats_iris <- iris_data %>% 
  group_by(Species) %>% 
  get_summary_stats(type = 'full')

stats_iris

# Gráficos básicos
# Dispersão
FIG.DIS <- ggplot(data = iris_data, 
                  mapping = aes(x = Sepal.Length, 
                                y = Petal.Length,
                                shape = Species, 
                                color = Species))+
  geom_smooth(method = 'lm', 
              aes(group = Species, 
                  color = Species), 
              fill = 'lightgrey')+
  geom_point(size = 3, 
             alpha = 0.75)+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12, 
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = 'bottom',
        strip.text = element_text(size = 12, 
                                  face='italic', 
                                  color = '#000000'))+
  scale_color_brewer(palette = 'Set1')+
  labs(x = 'Sepal Length', 
       y = 'Pepal Length')

FIG.DIS



# Boxplot 
FIG.BOX <- ggplot(data = iris_data, 
                  mapping = aes(x = Species, 
                                y = Petal.Length,
                                fill = Species))+
  geom_boxplot(width = 0.5)+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12,
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = '',
        strip.text = element_text(size = 12, 
                                  face ='italic', 
                                  color = '#000000'))+
  scale_fill_brewer(palette = 'Set1')+
  labs(x = 'Species', y = 'Pepal Length')+
  stat_summary(fun = mean, color = 'grey', 
               position = position_dodge(0.9),
               geom = 'point', 
               shape = 18, size=3,
               show.legend = FALSE)

FIG.BOX

# Violin 
FIG.VIO <- ggplot(data = iris_data, 
                  mapping = aes(x = Species, 
                                y = Petal.Length,
                                fill = Species))+
  geom_violin()+
  geom_jitter(width = 0.1,
              alpha = 0.5,
              color = 'grey')+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12,
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = '',
        strip.text = element_text(size = 12, 
                                  face ='italic', 
                                  color = '#000000'))+
  scale_fill_brewer(palette = 'Set1')+
  labs(x = 'Species', y = 'Pepal Length')+
  stat_summary(fun = mean, color = 'black', 
               position = position_dodge(0.9),
               geom = 'point', 
               shape = 18, size=3,
               show.legend = FALSE)

FIG.VIO



# Densidade
FIG.DEN <- ggplot(data = iris_data, 
                  mapping = aes(x = Petal.Length,
                                fill = Species,
                                alpha = 0.75))+
  geom_density(stat = 'density')+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12,
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = '',
        strip.text = element_text(size = 12, 
                                  face ='italic', 
                                  color = '#000000'))+
  scale_fill_brewer(palette = 'Set1')+
  labs(x = 'Species', y = 'Pepal Length')


FIG.DEN

# Barras
FIG.BAR <- ggplot(data = iris_data, 
                  mapping = aes(x = Species,
                                fill = Species))+
  geom_bar(stat = 'count',
           position = 'stack')+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12,
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = '',
        strip.text = element_text(size = 12, 
                                  face ='italic', 
                                  color = '#000000'))+
  scale_fill_brewer(palette = 'Set1')+
  labs(x = 'Species', y = 'Pepal Length')

FIG.BAR

# Barras
FIG.BAR2 <- ggplot(data = stats_iris %>% 
                     filter(variable == 'Petal.Length'), 
                  mapping = aes(y = mean,
                                x = Species,
                                fill = Species))+
  geom_bar(stat = 'identity')+
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd), 
                width = 0.25, 
                linewidth = 1,
                color = 'black')+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12,
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = '',
        strip.text = element_text(size = 12, 
                                  face ='italic', 
                                  color = '#000000'))+
  scale_fill_brewer(palette = 'Set1')+
  labs(x = 'Species', y = 'Petal Length')

FIG.BAR2

# Histograma
FIG.HIS <- ggplot(data = iris_data,
                   mapping = aes(x = Petal.Length,
                                 fill = Species))+
  geom_histogram()+
  theme_classic()+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12,
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = '',
        strip.text = element_text(size = 12, 
                                  face ='italic', 
                                  color = '#000000'))+
  scale_fill_brewer(palette = 'Set1')+
  labs(x = 'Species', y = 'Petal Length')

FIG.HIS

# Pizza
iris_pizza <- iris_data %>%
  group_by(Species) %>% 
  tally() %>%  
  glimpse()

FIG.PIZ <- ggplot(data = iris_pizza, 
                  mapping = aes(x = '',
                                y = n,
                                fill = Species))+
  geom_bar(stat = 'identity', width = 1)+
  coord_polar('y', start = 0)+
  theme_bw()+
  labs(x = ' ', y = ' ')+
  theme(plot.title = element_blank(),
        axis.title = element_text (size = 12, 
                                   face = 'bold', 
                                   color = '#000000'),
        axis.text = element_text (size = 10, 
                                  color = '#000000'),
        legend.title = element_text (size = 12,
                                     face = 'bold'),
        legend.text = element_text (size = 10),
        legend.position = 'left',
        strip.text = element_text(size = 12, 
                                  face ='italic', 
                                  color = '#000000'))+
  scale_fill_brewer(palette = 'Set1')+
  geom_text(aes(y = n/3 + c(0, cumsum(n)[-length(n)]), 
                label = percent((n/(n*3)))), size = 5)

FIG.PIZ

# Para salvar a figura
ggsave('nome_da_figura.png',
       FIG.ESCOLHIDA,
       # width = 9,
       # height = 9,
       # units = 'cm',
       dpi = 600)


# Se você é uma pessoa cansada:
squisser(iris_data)