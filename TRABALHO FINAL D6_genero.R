##TRABALHO FINAL D6

lista.de.pacotes = c("tidyverse","lubridate","janitor","readxl","stringr","repmis") # escreva a lista de pacotes
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

#CARREGANDO PLANILHAS
Universal2016 <- read.csv2("Chamada 01-2016 - Universal.csv")
Universal2018 <- read_csv2("Chamada 28-2018 - Universal.csv")

#número de processos por gênero
proc.por.genero <- Universal2016 %>%
  group_by(Sexo) %>% 
  summarise(proc.por.genero = n()) %>% 
  arrange(desc(proc.por.genero))

#número de processos por ca ordenado
processos.por.ca <- Universal2016 %>%
  group_by(CA, Sexo) %>% 
  summarise(num.proc.ca = n()) %>% 
  mutate(prop.ca = round(prop.table(num.proc.ca), digits = 2)) 
  
  filtro.mulheres <- processos.por.ca %>%
   
  summarise(maxfem = max(num.proc.ca))
  
  grafico1 <- (ggplot(processos.por.ca, aes(x = Sexo,  colour = CA, size = Sexo)) +
                  geom_bar()+
                  facet_wrap(~Sexo))
grafico1

grafico2 <- (ggplot(processos.por.ca, aes(x = num.proc.ca, y = Sexo, colour = CA, size = num.proc.ca)) +
               geom_boxplot()
)
               
grafico2

#número de processos por diretoria        
processos.por.diretoria <- Universal2016 %>%
  group_by(Diretoria) %>% 
  summarise(num.proc.diretoria = n()) %>% 
  mutate(prop.dir = round(100*prop.table(num.proc.diretoria), digits = 2))

#número de processos por diretoria  e genero     
proc.diretoria.genero <- Universal2016 %>%
  group_by(Diretoria, Sexo) %>% 
  summarise(num.proc.diretoria = n() %>% 
  mutate(prop.dir = round(100*prop.table(num.proc.diretoria), digits = 2)) 
  
#número processos por região
processos.por.regiao<- Universal2016 %>%
  filter(!is.na(RegiÆo)) %>%
  group_by(RegiÆo,Sexo) %>% 
  summarise(num.proc.regiao = n()) %>% 
  arrange(desc(num.proc.regiao))

grafico.reg <- (ggplot(processos.por.regiao, aes(x = RegiÆo,  colour = Sexo, size = Sexo)) +
               geom_bar()
)
    

grafico.reg
