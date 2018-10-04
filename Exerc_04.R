# Exercícios aula 04
lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

######################
# Exercícios Aula 02 #
######################

# 1.Carregue os dados rds ----


decisoes <- readRDS("~/Marony/decisoes.rds")
decisoes




# 2.Observe os dados ----

decisoes


# 3.selecione as colunas que acabam com "cisao". ----
decisoes.exem <- decisoes %>% 
  select(id_decisao, n_processo, municipio, juiz)
decisoes.exem

decisoes.end.cisao <- decisoes %>% 
  select(id_decisao, ends_with("cisao"))
decisoes.end.cisao

# 4.tire as colunas de texto = 'txt_decisao' e classe/assunto = 'classe_assunto'. ----
### Dica: veja os exemplos de `?select` em `Drop variables ...`

#para retirar colunas, basta usar o select com o (-)
decisoes
decisoes.peq <- decisoes %>% 
  select(-txt_decisao, -classe_assunto)
decisoes.peq


# 5.filtre apenas casos em que `id_decisao` ? igual a NA` ----
decisoes.na <- decisoes %>%
  filter(is.na(id_decisao))
decisoes.na

#a fun??o is.na indicaca quais lementos est?o faltando> is.na(id_decisao)
#retira da coluna id_decisao os valores NA

# 6.filtre todas as decisões de 2018. ----
### Dica: função `lubridate::year()`
decisoes2018 <- decisoes %>%
  filter(year(dmy(data_decisao)) == 2018)
decisoes2018

#7. exempplo mutate. Quanto tempo entre a data do registro e a data da decis?o
#mutate vai criar uma nova coluna a partir de outra
decisoes.tempo <- decisoes %>% 
  select(n_processo, data_decisao, data_registro) %>% 
  mutate(tempo = dmy(data_registro) - dmy(data_decisao))
decisoes.tempo

# 7.Crie uma coluna binária `drogas` que vale `TRUE` se no texto da decisão algo é falado de drogas e `FALSE` caso contrário. ----
### Dica: `str_detect`
### Obs.: Considere tanto a palavra 'droga' como seus sinônimos, 
### ou algum exemplo de droga e retire os casos em que `txt_decisao` é vazio
decisoes.sobre.droga <- decisoes %>% 
  #transforma a coluna txt_decis?o > onde tiver NA vai colocar false 
  filter(!is.na(txt_decisao)) %>% 
  #tolower coloca tudo em minusculo
  #na fun?ao mutate vai criar a variavel droga
  mutate(txt_decisao = tolower(txt_decisao),
         droga = str_detect(txt_decisao,
                            "droga|entorpecente|psicotr[?o]pico|maconha|haxixe|coca[?i]na")) %>%
  dplyr::select(n_processo,droga)
decisoes.sobre.droga

#8 quantas decisoes tem sobre droga?

decisoes.sobre.droga %>%
  group_by(droga) %>%
  summarise(n=n())%>%
  head()

#9 
# 8.Quem são os cinco relatores mais prolixos? ----
### Dica: use `str_length()`
### Lembre-se da função `head()`
decisoes %>% 
  filter(!is.na(txt_decisao)) %>% 
  mutate(tamanho = str_length(txt_decisao)) %>% 
  group_by(juiz) %>% 
  summarise(n = n(), 
            tamanho_mediana = median(tamanho)) %>% 
  filter(n >= 10) %>% 
  arrange(desc(tamanho_mediana)) %>%
  head(5)
#10  filtra ju?zes que t?m `Z` ou `z` no nome
decisoes %>% 
  select(juiz) %>% 
  filter(str_detect(juiz, regex("z", ignore_case = TRUE))) %>% 
  # conta e ordena os juizes em ordem decrescente
  count(juiz, sort = TRUE) %>%
  head()
#11
decisoes %>% 
  select(n_processo, municipio, data_decisao) %>%
  #        pega ano da decis?o
  mutate(ano_julgamento = year(dmy(data_decisao)),
         # pega o ano do processo 0057003-20.2017.8.26.0000" -> "2017"
         ano_proc = str_sub(n_processo, 12, 15),
         # transforma o ano em inteiro
         ano_proc = as.numeric(ano_proc),
         # calcula o tempo em anos
         tempo_anos = ano_julgamento - ano_proc) %>% 
  group_by(municipio) %>% 
  summarise(n = n(),
            media_anos = mean(tempo_anos),
            min_anos = min(tempo_anos),
            max_anos = max(tempo_anos)) 
#12
decisoes %>% 
  count(juiz, sort = TRUE) %>% 
  mutate(prop = n / sum(n), 
         prop = scales::percent(prop))
#13 sem formato de %
decisoes %>% 
  count(juiz, sort = TRUE) %>% 
  mutate(prop = prop.table(n))

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
