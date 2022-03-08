library(tidyverse)
library(shinydashboard)

dados <- read_csv("base de dados.csv")

head(dados)

dados[is.na(dados)] <- 0

dados$score <- apply(dados[3:88], 1, mean)

dados <- dados %>% 
  mutate(rank = min_rank((dados$score)))


ggplot(dados, aes(x = dados$score, fill = as.factor(dados$rank))) +
  geom_bar(width = 0.1) +
  theme_bw() +
  scale_fill_brewer(palette = 'PuBu') +
  scale_x_continuous('Score final', limits = c(1, 5)) 
  
  

ggplot(dados) +
  geom_histogram(aes(x = dados$`Há quanto tempo está na empresa (meses)`), bins= 10) +
  theme_minimal()

ggplot(dados, aes(x = dados$`A empresa consulta o funcionário antes de promover mudanças no trabalho....3`)) +
  geom_histogram(bins = 10) +
  theme_minimal()+
  scale_x_continuous('A empresa consulta o funcionário antes de promover mudanças no trabalho', limits = c(0,5.3))

ggplot(dados, aes(x = dados$score)) +
  geom_histogram(binwidth = 0.25) +
  scale_x_continuous('Score final', limits = c(0,5))
