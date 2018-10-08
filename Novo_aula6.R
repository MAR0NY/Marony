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

exemplo.inner <- decisoes %>% 
  filter(data_registro == "18/01/2018", !is.na(id_decisao)) %>% 
  select(id_decisao, n_processo) %>% 
  inner_join(processos, "n_processo")

inner.decisoes.final <- inner_join(exemplo.inner, processos, "n_processo")

#exemplo 2
exemplo.inner2 <- decisoes %>% 
  filter(data_registro == "18/01/2018", !is.na(id_decisao)) %>% 
  select(id_decisao, n_processo) %>% 
  dplyr::rename(numero_processo=n_processo) %>%
  inner_join(processos, by=c("numero_processo"="n_processo"))
             

             
#right join

right.decisoes <- decisoes %>% 
  filter(data_registro == "18/01/2018", !is.na(id_decisao)) %>% 
  select(id_decisao, n_processo) %>% 
  right_join(processos, "n_processo")
#se quiser trazer somente algumas colunas de outras tabela
# right_join(processos %>% dplyr::select(n_processo,partes))

#mais um exemplo
decisoes.selecao <- decisoes %>% 
  dplyr::select(n_processo,partes)
  right.decisoes2 <- right_join(decisoes.selecao, "n_processo")


# Crie um objeto contendo informações sobre os tamanhos das bancadas dos ----
# partidos (arquivo `bancadas.rds`), suas respectivas coligações 
# eleitorais para 2018 (arquivo `coligacoes.xlsx`) e o 
# grau de concordância com a agenda do Gov 
# Temer (arquivo `governismo_temer.xlsx`). 

    coligacoes <- read_excel("Dados/coligacoes.xlsx")
    governismo <- governismo_temer <- read_excel("Dados/governismo_temer.xlsx")
    bancadas <- readRDS("~/Marony/Dados/bancadas.rds") 
#juntando com coligações
   bancadas.coligacoes <- bancadas %>% 
     left_join(coligacoes) %>% 
     left_join(governismo)
  
    
  
# Bônus: use `group_by` e `summarise` para identificar qual candidato tem a ----
# coligação com menor média de concordância e qual candidato 
# tem a maior proporção total de assentos.