# Exercícios aula 04
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

######################
# Exercícios Aula 06 #
######################

# 1.Carregue os dados rds ----


decisoes <- readRDS("~/Marony/decisoes.rds")
decisoes
#14
decisoes.drogas <- decisoes %>% 
  filter(!is.na(txt_decisao)) %>%
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[óo]pico|maconha|haxixe|coca[íi]na"),
         droga=case_when(
           droga==TRUE ~ "droga",
           droga==FALSE ~ "n_droga"
         )) %>%
  group_by(juiz,droga) %>%
  summarise(n=n()) %>%
  spread(droga,n,fill = 0) %>%
  mutate(total=droga+n_droga,
         proporcao=droga/total)
decisoes.drogas

#15 qual a quantidade total de processos por juiz
#! -> sinal logico que indica diferente
# n() conta número de linhas
quant.mes.juiz <- decisoes %>% 
  mutate(mes = month(dmy(data_decisao))) %>%
  filter(!is.na(mes))%>%
  group_by(juiz,mes) %>%
  summarise(quant.proc=n()) %>%
  spread(mes,quant.proc,fill = 0) 
quant.mes.juiz

#usando separadores de coluna
decisoes_sep <- decisoes %>%
  select(n_processo, classe_assunto) %>%
  separate(classe_assunto,
           c('classe', 'assunto'),
           sep = '/',
           extra = 'merge',
           fill = 'right')
decisoes_sep


# Crie um objeto contendo informações sobre os tamanhos das bancadas dos ----
# partidos (arquivo `bancadas.rds`), suas respectivas coligações 
# eleitorais para 2018 (arquivo `coligacoes.xlsx`) e o 
# grau de concordância com a agenda do Gov 
# Temer (arquivo `governismo_temer.xlsx`). 

# Bônus: use `group_by` e `summarise` para identificar qual candidato tem a ----
# coligação com menor média de concordância e qual candidato 
# tem a maior proporção total de assentos.