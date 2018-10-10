# Exercícios aula 05
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()



# Carregue o arquivo `decisoes.rds` em um objeto chamado `decisoes`. ----

decisoes <- readRDS("~/Marony/decisoes.rds")

# Qual quantidade mensal de decisões por juiz?

juiz_mes <- decisoes %>% 
  mutate(mes = month(dmy(data_decisao))) %>%
  filter(!is.na(mes)) %>%
  group_by(juiz,mes) %>%
  summarise(n=n()) %>%
  spread(mes,n,fill = 0)

#novo exemplo
novo_exemplo_sep <- decisoes %>% 
  select(n_processo, classe_assunto) %>% 
  separate(classe_assunto, c('classe', 'assunto'), sep = ' / ', 
           extra = 'merge', fill = 'right') %>% 
  #neste caso usa o count quando só for contagem
  count(assunto, sort = TRUE)

#masi um exemplo processos nested - aninhando

processos <- readRDS("~/Marony/dados/processos_nested.rds")
processos
#dá uma olhada no pacote survey
d_partes <- processos %>% 
  select(n_processo, partes) %>% 
  unnest(partes)
d_partes



# Crie um objeto contendo informações sobre os tamanhos das bancadas dos ----
# partidos (arquivo `bancadas.rds`), suas respectivas coligações 
# eleitorais para 2018 (arquivo `coligacoes.xlsx`) e o 
# grau de concordância com a agenda do Gov 
# Temer (arquivo `governismo_temer.xlsx`). 

# Bônus: use `group_by` e `summarise` para identificar qual candidato tem a ----
# coligação com menor média de concordância e qual candidato 
# tem a maior proporção total de assentos.