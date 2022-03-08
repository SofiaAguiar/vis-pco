if(!require(tidyverse)){install.packages("tidyverse");library(tidyverse)}
if(!require(gt)){install.packages("gt");library(gt)}
if(!require(lubridate)){install.packages("lubridate");library(lubridate)}
if(!require(ggthemes)){install.packages("ggthemes");library(ggthemes)}
library(matrixStats)

cores_verde <- colorRampPalette(c("#4fb6a7","#D0EBE8"))
cores_roxo <- colorRampPalette(c("#652177", "#D6C5DB"))
cores<- colorRampPalette(c("#4fb6a7","#652177"))
cores2<- colorRampPalette(c("#4fb6a7","#D0EBE8","#D6C5DB","#652177"))

tema <- theme_hc()+ 
  theme(axis.title.y = element_text(angle = 90,
                                    margin = margin(0.3, 0.5, 0.3, 0, "cm")),
                          legend.position = "right",
                          plot.margin = margin(0.3, 1, 0.3, 1, "cm"))
theme_set(tema)



dados <- read_csv('base com tipo.csv')


dados <- replace(x = dados, list = is.na(dados), values = 0)
dados$Categoria <- as.factor(dados$Categoria)



dados <- dados %>% 
  mutate(media = (id1 + id2 + id3 + id4 + id5 + id6 + id7 + id8 + id9 + id10)/10) %>% 
  mutate(dp = rowSds(as.matrix(dados[,c(3,4,5,6,7,8,9,10,11,12)])))

ggplot(dados, aes(x = media)) +
  geom_histogram(color = "white", fill = "#4fb6a7") +
  labs(x = 'Nota', 
       y = 'Frequência', 
       title = 'Nota média por pergunta')

categ <- dados %>% 
  group_by(Categoria) %>% 
  summarise(media = mean(media))

ggplot(categ, aes(x = Categoria, y = media, fill = Categoria)) + 
  geom_col() + 
  labs(x = "Categoria",
       y = "Nota média",
       fill = "",
       title = "Nota média por categoria de pergunta") +
  theme(legend.position = "bottom", axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values = cores(length(unique(categ$Categoria))))



autonomia <- dados %>% 
  filter(Categoria == 'AUTONOMIA')

ggplot(autonomia, aes(x = Pergunta, y = media)) + 
  geom_col(color = "white", fill = "#4fb6a7") + 
  labs(x = "Pergunta",
       y = "Nota média",
       title = "Nota média por pergunta sobre autonomia") +
  theme(axis.text.x = element_text(angle = -10)) +
  geom_errorbar(aes(x=Pergunta, ymin=media-dp, ymax=media+dp), width=0.2, colour="orange", alpha=0.9, size=1)
  