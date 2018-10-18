# Exercícios aula 07
lista.de.pacotes = c("tidyverse","lubridate","janitor",
                     "readxl","stringr","repmis","janitor",
                     "survey","srvyr", "scale") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()



##

data(api)

srs_design_srvyr <- apisrs %>% 
  as_survey(fpc = fpc) %>%
  mutate(nivel=case_when(
    stype=="E"~"Fundamental",
    stype=="M"~"Fundamental",
    stype=="H"~"Médio"
  ))
resolucao <- srs_design_srvyr %>%
  group_by(nivel) %>%
  summarize(proporcao = survey_mean(vartype = "cv"),
            n=survey_total(vartype = "ci"))
#SIMULADO
data(api)

amostra_expandida <- apisrs %>% #simple random simple
  as_survey(weight = pw) %>%
  mutate(nivel=case_when(
    stype=="E"~"Fundamental",
    stype=="M"~"Fundamental",
    stype=="H"~"Médio"
  ))
out <- amostra_expandida %>%
  group_by(nivel) %>%
  summarise(api_diff = survey_mean(api00 - api99, vartype = "ci"))#media(de 99 e 2000) das diferenças por nivel
out %>% #por nível fundamental ou medio, usadno os intervalos
  ggplot(aes(x = nivel, y = api_diff, fill = nivel ,color=nivel,
             ymax = api_diff_upp, ymin = api_diff_low)) +
  geom_bar(stat = "identity",alpha= 0.3) +
  geom_errorbar(width = 0, size=2) +
  geom_text(aes(label=round(api_diff,0)),color="black", size =4,vjust= -0.8, hjust=1)+
  labs(y = "Percent", fill="nivel") +
facet_grid(~nivel) +
  scale_y_continuous(labels=scales::percent)#não faz muito sentido aqui...so pra saber
#https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/

#ggplot
srs_design_srvyr %>% 
  data.frame() %>% 
  ggplot(aes(x=api99/1000, 
                 y = api00/1000, 
                 color = nivel,
                 shape = stype,
             group = nivel)) + #agrupa uma varável ver a tendencia
  geom_point()+
  geom_smooth(method ="lm", se=F ) +  #lm - modelo linear
  scale_color_manual(values = c("#ff0000", "aquamarine4")) +
  scale_x_continuous(limits = c(0.25,1),#o eixo vai começar em 0 e termianr em 1
                     labels=scales::percent) +
scale_y_continuous(limits = c(0,1),#o eixo vai começar em 0 e termianr em 1
                   labels=scales::percent) +
  theme_minimal()+
  facet_wrap (~stype)