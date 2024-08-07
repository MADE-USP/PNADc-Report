
library(PNADcIBGE)
library(survey)
library(convey)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(zoo)
library(ggplot2)

#Comando para importar os dados da PNADC

i=2024 #ano
q=1 #trimestre

pnad <- get_pnadc(year = i,                        
                  quarter = q,
                  defyear = 2023,
                  defperiod = 1,
                  deflator = T,
                  labels = FALSE, 
                  vars = c("V1028", #pesos com p?s-estratifica??o
                           "V2007", # Sexo
                           "V2009", #Idade
                           "V2010", # Raca
                          # "V4012", #=1 contribuinte INSS =2 não contribuinte
                         #  "VD4001", # Condi??o em rela??o ? for?a de trabalho
                          # "VD4002", # Condi??o de ocupa??o
                        #   "VD4004A", # Subocupa??o por insufici?ncia de horas habitualmente trabalhadas
                           "VD4009", # Posi??o na ocupa??o e categoria do emprego do trabalho principal da  semana de refer?ncia para pessoas de 14 anos ou mais de idade
                           "VD4016", # Rendimento mensal habitual do trabalho principal
                         #  "VD4017", # Rendimento mensal efetivo do trabalho principal
                         #  "VD4030", # Motivo pelo qual n?o procurou/gostaria/estava dispon?vel para trabalho
                         #  "V3009A", # Qual foi o curso mais elevado que frequentou anteriormente?
                         #  "VD3001", #N?vel de instru??o mais elevado alcan?ado 
                          # "V3012", #Concluiu primeiros anos
                         #  "VD4020", #Rendimento mensal efetivo de todos os trabalhos para pessoas de 14 anos ou mais de idade
                         #  "VD4012",
                           #V4012", # se =1 contribuinte INSS = 2 nao contribuinte
                           "VD4010")) #setor do trabalho principal





######Salvar no PC pra não precisar baixar sempre a base


#save(pnad,file='C:/Users/Marina Sanches/OneDrive/Made/pnadc_012023.RData')
#save(pnad,file="H:/Meu Drive/Documentos/Made/relatorio_pnad/pnadc_042023.RData")
#save(pnad,file='C:/Users/Tainari/Documents/1.1-Mestrado/1.2 - Doutorado/MADE/Relatorio_PNAD/pnadc_012023.RData')
save(pnad,file='C:/Users/USER/Documents/PNAD/pnadc_032023.RData')

####### Baixar a base 
#load('C:/Users/USER/Documents/PNAD/pnadc_032023.RData')
load('C:/Users/USER/Documents/PNAD/pnadc_032023.RData')
#load('C:/Users/Marina Sanches/OneDrive/Made/pnadc_012023.RData')
#load("H:/Meu Drive/Documentos/Made/relatorio_pnad/pnadc_012023.RData")
#load('C:/Users/Tainari/Documents/1.1-Mestrado/1.2 - Doutorado/MADE/Relatorio_PNAD/pnadc_012023.RData')


######## Baixar série histórica
#load('H:/Meu Drive/Documentos/Made/relatorio_pnad/dados_serie_historica_032023_FINAL_ze_08_02.RData')


i=2024 #ano
q=1 #trimestre

################################################################################

#Transformando as variáveis


#Transformando raca de 6 para 2 (1= Branca, 2= Negra) 
pnad$variables$V2010[pnad$variables$V2010 %in% c("2", "4")] <- "2"
pnad$variables$V2010[pnad$variables$V2010 %in% c("5", "9")] <- NA
pnad$variables$V2010[pnad$variables$V2010 %in% c("3")] <- NA

#Calculo do rendimento mensal efetivo por trabalhador (trabalho principal), deflacionado:
#pnad$variables$EfetivaDef22 <- pnad$variables$Efetivo * pnad$variables$VD4017

##Calculo do rendimento mensal efetivo por trabalhador (em todos os trabalhos), deflacionado:
#pnad$variables$EfetivaDef22todos <- pnad$variables$Efetivo * pnad$variables$VD4020
#
#Calculo do rendimento mensal habitual por trabalhador, deflacionado:
pnad$variables$HabitualDef22 <- pnad$variables$Habitual * pnad$variables$VD4016

#Variavel de linha de pobreza
pnad$variables$Linha_pobreza[pnad$variables$HabitualDef22 %in% c("01","02","03","04")] <- "1"
pnad$variables$Linha_pobreza <- case_when(pnad$variables$HabitualDef22 < 100 ~ 1,
                                          pnad$variables$HabitualDef22 >= 100 ~ 0)

Linha_extrema_pobreza <- case_when(pnad$variables$HabitualDef22 < 50 ~ 1,
                                   pnad$variables$HabitualDef22 >= 50 ~ 0)

#Transformando Escolaridade de 15 para 6 (onde 1 = pre-fundamental, 2=ensino fundamental, 3=ensino medio, 4=ensino superior, 5=especializacao pos-graduacao, 6=mestrado/doutorado)
#pnad$variables$V3009A[pnad$variables$V3009A %in% c("05","07","08")] <- "2"
#pnad$variables$V3009A[pnad$variables$V3009A %in% c("01","02","03","04")] <- "1"
#pnad$variables$V3009A[pnad$variables$V3009A %in% c("06","09","10","11")] <- "3"
#pnad$variables$V3009A[pnad$variables$V3009A %in% c("12")] <- "4"
#pnad$variables$V3009A[pnad$variables$V3009A %in% c("13")] <- "5"
#pnad$variables$V3009A[pnad$variables$V3009A %in% c("14","15")] <- "6"


#Transformando posição no mercado de trabalho (1=carteira assinada, 2=sem carteira, 3=conta-propria, 4=empregador, 5=militares e servico estatuario e 6=auxiliar familiar)
#### DEFINIR MELHOR POSTERIORMENTE####
pnad$variables$VD4009[pnad$variables$VD4009 %in% c("01","03","05")] <- "1" #trabalhadores com carteira assinada
pnad$variables$VD4009[pnad$variables$VD4009 %in% c("02","04","06")] <- "0" #trabalhadores sem carteira assinada


#Transformando para regiao 1=norte, 2=nordeste, 3=centro-oeste, 4=sudeste, 5=sul
pnad$variables$UF[pnad$variables$UF %in% c("11", "12", "13", "14", "15", "16", "17")] <- "Norte"
pnad$variables$UF[pnad$variables$UF %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29")] <- "Nordeste"
pnad$variables$UF[pnad$variables$UF %in% c("31", "32", "33", "35")] <- "Sudeste"
pnad$variables$UF[pnad$variables$UF %in% c("41", "42", "43")] <- "Sul"
pnad$variables$UF[pnad$variables$UF %in% c("50", "51", "52", "53")] <- "Centro-Oeste"

#Transformando VD4004A e VD4002 --> de NA para 0
pnad$variables$VD4004A[is.na(pnad$variables$VD4004A)&(pnad$variables$VD4002 == "1")] <- "0"

categoriasregiao<-c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
                    "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
                    "Homem Branco Nordeste","Homem Negro Nordeste",
                    "Mulher Branca Nordeste", "Mulher Negra Nordeste",
                    "Homem Branco Norte", "Homem Negro Norte",
                    "Mulher Branca Norte", "Mulher Negra Norte",
                    "Homem Branco Sudeste", "Homem Negro Sudeste",
                    "Mulher Branca Sudeste", "Mulher Negra Sudeste",
                    "Homem Branco Sul", "Homem Negro Sul",
                    "Mulher Branca Sul", "Mulher Negra Sul")

categorias<-c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

#######MUDANCA NA ORDEM DOS ESTADOS, TEM QUE SEGUIR A ORDEM ALFABETICA################
regiaobrasil<-c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

#Subset de mulheres trabalhadoras domésticas
#domesticas <- subset(pnad, V4012=="Trabalhador doméstico" & V2007=="Mulher")

################################################################################
################## CALCULO DOS INDICADORES #####################################

#PEA POR GRUPO (VD4001)

#PEA.RG <- svyby(formula = ~ VD4001, 
#                by = ~ interaction(V2010, V2007), #Raca, Genero
#                design = pnad, 
#                svymean, # Fun??o para gerar estat?stica de interesse
#                na.rm = T) # Remover valores faltantes 
#
#row.names(PEA.RG) <-categorias
#PEA.RG <- PEA.RG %>%
#  select(-'se1', -'interaction(V2010, V2007)', -'se2')%>%
#  rename('PEA' = VD40011, 'Fora da PEA' = VD40012)
#
#PEA.RG$Ano <- i
#PEA.RG$Quarter <- q
#PEA.RG$Day <- 01
#PEA.RG$Month <- ifelse(PEA.RG$Quarter==1,1,
#                       ifelse(PEA.RG$Quarter==2,4,
#                              ifelse(PEA.RG$Quarter==3,7,10)))
#PEA.RG$Trimestre<-as.yearqtr(with(PEA.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#PEA.RG$Trimestre<- as.character(PEA.RG$Trimestre)
#PEA.RG <- PEA.RG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#PEA.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#PEA.RG.sh <- rbind(PEA.RG.sh,PEA.RG)
#PEA.RG <- PEA.RG %>%
#  select(-'Trimestre')
#
#
#PEA POR GRUPO (VD4001) e por regiao 

#PEA.RG.REG <- svyby(formula = ~ VD4001, 
#                    by = ~ interaction(V2010, V2007, UF), #Raca, Genero
#                    design = pnad, 
#                    svymean, # Fun??o para gerar estat?stica de interesse
#                    na.rm = T) # Remover valores faltantes 
#
#row.names(PEA.RG.REG) <-categoriasregiao
#PEA.RG.REG <- PEA.RG.REG %>%
#  select(-'se1', -'interaction(V2010, V2007, UF)', -'se2')%>%
#  rename('PEA' = VD40011, 'Fora da PEA' = VD40012)
#
#PEA.RG.REG$Ano <- i
#PEA.RG.REG$Quarter <- q
#PEA.RG.REG$Day <- 01
#PEA.RG.REG$Month <- ifelse(PEA.RG.REG$Quarter==1,1,
#                           ifelse(PEA.RG.REG$Quarter==2,4,
#                                  ifelse(PEA.RG.REG$Quarter==3,7,10)))
#PEA.RG.REG$Trimestre<-as.yearqtr(with(PEA.RG.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#PEA.RG.REG$Trimestre<- as.character(PEA.RG.REG$Trimestre)
#PEA.RG.REG <- PEA.RG.REG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#PEA.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
#                              "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
#                              "Homem Branco Nordeste","Homem Negro Nordeste",
#                              "Mulher Branca Nordeste", "Mulher Negra Nordeste",
#                              "Homem Branco Norte", "Homem Negro Norte",
#                              "Mulher Branca Norte", "Mulher Negra Norte",
#                              "Homem Branco Sudeste", "Homem Negro Sudeste",
#                              "Mulher Branca Sudeste", "Mulher Negra Sudeste",
#                              "Homem Branco Sul", "Homem Negro Sul",
#                              "Mulher Branca Sul", "Mulher Negra Sul")
#
#PEA.RG.REG.sh <- rbind(PEA.RG.REG.sh,PEA.RG.REG)
#
#PEA.RG.REG <- PEA.RG.REG %>%
#  select(-'Trimestre')
#
##PEA POR regiao (VD4001)
#
##PEA.REG <- svyby(formula = ~ VD4001, 
#                 by = ~ interaction(UF), 
#                 design = pnad, 
#                 svymean, # Fun??o para gerar estat?stica de interesse
#                 na.rm = T) # Remover valores faltantes 
#row.names(PEA.REG) <-regiaobrasil
#PEA.REG <- PEA.REG %>%
#  select(-'se1', -'interaction(UF)', -'se2')%>%
#  rename('PEA' = VD40011, 'Fora da PEA' = VD40012)
#PEA.REG$Ano <- i
#PEA.REG$Quarter <- q
#PEA.REG$Day <- 01
#PEA.REG$Month <- ifelse(PEA.REG$Quarter==1,1,
#                        ifelse(PEA.REG$Quarter==2,4,
#                               ifelse(PEA.REG$Quarter==3,7,10)))
#PEA.REG$Trimestre<-as.yearqtr(with(PEA.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#PEA.REG$Trimestre<- as.character(PEA.REG$Trimestre)
#PEA.REG <- PEA.REG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#PEA.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
#PEA.REG.sh <- rbind(PEA.REG.sh,PEA.REG)
#PEA.REG <- PEA.REG %>%
#  select(-'Trimestre')
#
### Emprego e Desemprego
#
#### Ocupação


# Taxa de ocupação média  (VD4002)

#EmpregoMedia <- svymean(x=~VD4002, 
#                        design = pnad, 
#                        na.rm = T) # Remover valores faltantes 
#Tx_emprego <- as.data.frame(EmpregoMedia[1])
#colnames(Tx_emprego)[colnames(Tx_emprego) == "EmpregoMedia[1]"] ="Taxa de emprego"
#
#Tx_emprego$Ano <- i
#Tx_emprego$Quarter <- q
#Tx_emprego$Day <- 01
#Tx_emprego$Month <- ifelse(Tx_emprego$Quarter==1,1,
#                           ifelse(Tx_emprego$Quarter==2,4,
#                                  ifelse(Tx_emprego$Quarter==3,7,10)))
#Tx_emprego$Trimestre<-as.yearqtr(with(Tx_emprego,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#Tx_emprego$Trimestre<- as.character(Tx_emprego$Trimestre)
#
#Tx_emprego <- Tx_emprego %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#Tx_emprego.sh<- rbind(Tx_emprego.sh,Tx_emprego)
#
#
#Tx_desemprego <- as.data.frame(EmpregoMedia[2])
#colnames(Tx_desemprego)[colnames(Tx_desemprego) == "EmpregoMedia[2]"] ="Taxa de desemprego"
#Tx_desemprego$Ano <- i
#Tx_desemprego$Quarter <- q
#Tx_desemprego$Day <- 01
#Tx_desemprego$Month <- ifelse(Tx_desemprego$Quarter==1,1,
#                              ifelse(Tx_desemprego$Quarter==2,4,
#                                     ifelse(Tx_desemprego$Quarter==3,7,10)))
#Tx_desemprego$Trimestre<-as.yearqtr(with(Tx_desemprego,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#Tx_desemprego$Trimestre<- as.character(Tx_desemprego$Trimestre)
#
#Tx_desemprego <- Tx_desemprego %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#Tx_desemprego.sh<- rbind(Tx_desemprego.sh,Tx_desemprego)
#
## TAXA DE OCUPACAO POR GRUPO (VD4002)
#
#EMPREGO.RG <- svyby(formula = ~ VD4002, #Condicao em relacao a forca de trabalho
#                    by = ~ interaction(V2010, V2007), #Raca, Genero,
#                    design = pnad, 
#                    svymean, # Fun??o para gerar estat?stica de interesse
#                    na.rm = T) # Remover valores faltantes 
#
#row.names(EMPREGO.RG) <-categorias
#EMPREGO.RG <- EMPREGO.RG %>%
#  select(-'se1', -'interaction(V2010, V2007)', -'se2', -VD40022)%>%
#  rename('Ocupado' = VD40021 )
#EMPREGO.RG$Ano <- i
#EMPREGO.RG$Quarter <- q
#EMPREGO.RG$Day <- 01
#EMPREGO.RG$Month <- ifelse(EMPREGO.RG$Quarter==1,1,
#                           ifelse(EMPREGO.RG$Quarter==2,4,
#                                  ifelse(EMPREGO.RG$Quarter==3,7,10)))
#EMPREGO.RG$Trimestre<-as.yearqtr(with(EMPREGO.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#EMPREGO.RG$Trimestre<- as.character(EMPREGO.RG$Trimestre)
#EMPREGO.RG <- EMPREGO.RG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#EMPREGO.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#EMPREGO.RG.sh <- rbind(EMPREGO.RG.sh,EMPREGO.RG)
#EMPREGO.RG <- EMPREGO.RG %>%
#  select(-'Trimestre')
#
#
##TAXA DE OCUPACAO (VD4002)  por grupos e região 

#EMPREGO.RG.REG <- svyby(formula = ~ VD4002, #Condicao em relacao a forca de trabalho
#                        by = ~ interaction(V2010, V2007, UF), #Raca, Genero,
#                        design = pnad, 
#                        svymean, # Fun??o para gerar estat?stica de interesse
#                        na.rm = T) # Remover valores faltantes 
#
#row.names(EMPREGO.RG.REG) <-categoriasregiao
#EMPREGO.RG.REG <- EMPREGO.RG.REG %>%
#  select(-'se1', -'interaction(V2010, V2007, UF)', -'se2', -VD40022)%>%
#  rename('Ocupado' = VD40021 )
#
#EMPREGO.RG.REG$Ano <- i
#EMPREGO.RG.REG$Quarter <- q
#EMPREGO.RG.REG$Day <- 01
#EMPREGO.RG.REG$Month <- ifelse(EMPREGO.RG.REG$Quarter==1,1,
#                               ifelse(EMPREGO.RG.REG$Quarter==2,4,
#                                      ifelse(EMPREGO.RG.REG$Quarter==3,7,10)))
#EMPREGO.RG.REG$Trimestre<-as.yearqtr(with(EMPREGO.RG.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#EMPREGO.RG.REG$Trimestre<- as.character(EMPREGO.RG.REG$Trimestre)
#
#EMPREGO.RG.REG <- EMPREGO.RG.REG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#EMPREGO.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
#                                  "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
#                                  "Homem Branco Nordeste","Homem Negro Nordeste",
#                                  "Mulher Branca Nordeste", "Mulher Negra Nordeste",
#                                  "Homem Branco Norte", "Homem Negro Norte",
#                                  "Mulher Branca Norte", "Mulher Negra Norte",
#                                  "Homem Branco Sudeste", "Homem Negro Sudeste",
#                                  "Mulher Branca Sudeste", "Mulher Negra Sudeste",
#                                  "Homem Branco Sul", "Homem Negro Sul",
#                                  "Mulher Branca Sul", "Mulher Negra Sul")
#EMPREGO.RG.REG.sh <- rbind(EMPREGO.RG.REG.sh,EMPREGO.RG.REG)
#EMPREGO.RG.REG <- EMPREGO.RG.REG %>%
#  select(-'Trimestre')
#
#
##TAXA DE OCUPACAO  (VD4002) apenas por região 
#
#EMPREGO.REG <- svyby(formula = ~ VD4002, #Condicao em relacao a forca de trabalho
#                     by = ~ interaction(UF), #Raca, Genero,
#                     design = pnad, 
#                     svymean, # Fun??o para gerar estat?stica de interesse
#                     na.rm = T) # Remover valores faltantes 
#
#row.names(EMPREGO.REG) <-regiaobrasil
#EMPREGO.REG <- EMPREGO.REG %>%
#  select(-'se1', -'interaction(UF)', -'se2', -VD40022)%>%
#  rename('Ocupado' = VD40021 )
#
#EMPREGO.REG$Ano <- i
#EMPREGO.REG$Quarter <- q
#EMPREGO.REG$Day <- 01
#EMPREGO.REG$Month <- ifelse(EMPREGO.REG$Quarter==1,1,
#                            ifelse(EMPREGO.REG$Quarter==2,4,
#                                   ifelse(EMPREGO.REG$Quarter==3,7,10)))
#EMPREGO.REG$Trimestre<-as.yearqtr(with(EMPREGO.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#EMPREGO.REG$Trimestre<- as.character(EMPREGO.REG$Trimestre)
#
#EMPREGO.REG <- EMPREGO.REG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#EMPREGO.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
#
#EMPREGO.REG.sh <- rbind(EMPREGO.REG.sh,EMPREGO.REG)
#
#EMPREGO.REG <- EMPREGO.REG %>%
#  select(-'Trimestre')
#
#### Carteira assinada

# Taxa de informalidade media  (VD4009)

informalidadeMedia <- svymean(x=~VD4009, 
                              design = pnad, 
                              na.rm = T) # Remover valores faltantes 


informalidadeMedia <- as.data.frame(informalidadeMedia[2])
colnames(informalidadeMedia)[colnames(informalidadeMedia) == "informalidadeMedia[2]"] ="Taxa média de informalidade"

informalidadeMedia$Ano <- i
informalidadeMedia$Quarter <- q
informalidadeMedia$Day <- 01
informalidadeMedia$Month <- ifelse(informalidadeMedia$Quarter==1,1,
                                   ifelse(informalidadeMedia$Quarter==2,4,
                                          ifelse(informalidadeMedia$Quarter==3,7,10)))
informalidadeMedia$Trimestre<-as.yearqtr(with(informalidadeMedia,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
informalidadeMedia$Trimestre<- as.character(informalidadeMedia$Trimestre)

informalidadeMedia <- informalidadeMedia %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

informalidadeMedia.sh<- rbind(informalidadeMedia.sh,informalidadeMedia)

# TAXA DE INFORMALIDADE POR GRUPO 

informal.RG<- svyby(formula = ~ VD4009, # trabalha sem carteira assinada
                    by = ~ interaction(V2010, V2007), #Raca, Genero
                    design = pnad, 
                    svymean, # Funcao para gerar estatistica de interesse
                    na.rm = T) # Remover valores faltantes 

#row.names(informal.RG) <- categorias
#informal.RG <- informal.RG %>%
#  select(-'se1', -'interaction(V2010, V2007)', -'se2')%>%
#  rename('carteira assinada'=VD40091, 'sem carteira assinada'=VD40122) 
#
#informal.RG$Ano <- i
#informal.RG$Quarter <- q
#informal.RG$Day <- 01
#informal.RG$Month <- ifelse(informal.RG$Quarter==1,1,
#                            ifelse(informal.RG$Quarter==2,4,
#                                   ifelse(informal.RG$Quarter==3,7,10)))
#informal.RG$Trimestre<-as.yearqtr(with(informal.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#informal.RG$Trimestre<- as.character(informal.RG$Trimestre)

#informal.RG <- informal.RG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')

informal.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")


informal.RG.sh <- rbind(informal.RG.sh,informal.RG)

informal.RG <- informal.RG %>%
  select(-'Trimestre')

# TAXA DE INFORMALIDADE por grupo e por regiao

informal.RG.REG <- svyby(formula = ~ VD4009, # contribui para o INSS
                         by = ~ interaction(V2010, V2007, UF), #Raca, Genero
                         design = pnad, 
                         svymean, # Fun??o para gerar estat?stica de interesse
                         na.rm = T) # Remover valores faltantes 

row.names(informal.RG.REG) <- categoriasregiao
informal.RG.REG <- informal.RG.REG %>%
  select(-'se1', -'interaction(V2010, V2007, UF)', -'se2')%>%
  rename('contribuinte INSS'=VD40121, 'não contribuinte INSS'=VD40122) 

informal.RG.REG$Ano <- i
informal.RG.REG$Quarter <- q
informal.RG.REG$Day <- 01
informal.RG.REG$Month <- ifelse(informal.RG.REG$Quarter==1,1,
                                ifelse(informal.RG.REG$Quarter==2,4,
                                       ifelse(informal.RG.REG$Quarter==3,7,10)))
informal.RG.REG$Trimestre<-as.yearqtr(with(informal.RG.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
informal.RG.REG$Trimestre<- as.character(informal.RG.REG$Trimestre)

informal.RG.REG <- informal.RG.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

informal.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
                                   "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
                                   "Homem Branco Nordeste","Homem Negro Nordeste",
                                   "Mulher Branca Nordeste", "Mulher Negra Nordeste",
                                   "Homem Branco Norte", "Homem Negro Norte",
                                   "Mulher Branca Norte", "Mulher Negra Norte",
                                   "Homem Branco Sudeste", "Homem Negro Sudeste",
                                   "Mulher Branca Sudeste", "Mulher Negra Sudeste",
                                   "Homem Branco Sul", "Homem Negro Sul",
                                   "Mulher Branca Sul", "Mulher Negra Sul")

informal.RG.REG.sh <- rbind(informal.RG.REG.sh,informal.RG.REG)

informal.RG.REG <- informal.RG.REG %>%
  select(-'Trimestre')

# TAXA DE INFORMALIDADE somente por regiao

informal.REG<- svyby(formula = ~ VD4012, # contribui para o INSS
                     by = ~ interaction(UF), #Raca, Genero
                     design = pnad, 
                     svymean, # Fun??o para gerar estat?stica de interesse
                     na.rm = T) # Remover valores faltantes 

row.names(informal.REG) <- regiaobrasil
informal.REG <- informal.REG %>%
  select(-'se1', -'interaction(UF)', -'se2')%>%
  rename('contribuinte INSS'=VD40121, 'não contribuinte INSS'=VD40122) 

informal.REG$Ano <- i
informal.REG$Quarter <- q
informal.REG$Day <- 01
informal.REG$Month <- ifelse(informal.REG$Quarter==1,1,
                             ifelse(informal.REG$Quarter==2,4,
                                    ifelse(informal.REG$Quarter==3,7,10)))
informal.REG$Trimestre<-as.yearqtr(with(informal.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
informal.REG$Trimestre<- as.character(informal.REG$Trimestre)

informal.REG <- informal.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

informal.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

informal.REG.sh <- rbind(informal.REG.sh,informal.REG)

informal.REG <- informal.REG %>%
  select(-'Trimestre')

### Subocupação
#SUBOCUPACAO POR HORAS TRABALHADAS POR GRUPO (VD4004A)

#SUBOCUPHORAS.RG <- svyby(formula = ~ VD4004A, #Condicao em relacao a forca de trabalho
#                         by = ~ interaction(V2010, V2007), #Raca, Genero
#                         design = pnad, 
#                         svymean, # Fun??o para gerar estat?stica de interesse
#                         na.rm = T) # Remover valores faltantes 
#
#row.names(SUBOCUPHORAS.RG ) <-categorias
#SUBOCUPHORAS.RG <- SUBOCUPHORAS.RG %>%
#  select(-'se1', -'interaction(V2010, V2007)', -'se2', -VD4004A0)%>%
#  rename('Subocupado' = VD4004A1) # fiquei na duvida se é isso mesmo 
#
#SUBOCUPHORAS.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#
## SUBOCUPACAO POR HORAS TRABALHADAS  (VD4004A) somente por região
#
#SUBOCUPHORAS.REG <- svyby(formula = ~ VD4004A, #Condicao em relacao a forca de trabalho
#                          by = ~ interaction(UF), #Raca, Genero
#                          design = pnad, 
#                          svymean, # Fun??o para gerar estat?stica de interesse
#                          na.rm = T) # Remover valores faltantes 
#
#row.names(SUBOCUPHORAS.REG ) <-regiaobrasil
#SUBOCUPHORAS.REG <- SUBOCUPHORAS.REG %>%
#  select(-'se1', -'interaction(UF)', -'se2', -VD4004A0)%>%
#  rename('Subocupado' = VD4004A1) # fiquei na duvida se é isso mesmo 
#
#SUBOCUPHORAS.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
#
#### Trabalho por Setor
#
##Trabalho principal por setor - por grupo

#trabalhosetor.RG<- svyby(formula = ~ VD4010, 
#                         by = ~ interaction(V2010, V2007), #Raca, Genero
#                         design = pnad, 
#                         svymean, # Fun??o para gerar estat?stica de interesse
#                         na.rm = T) # Remover valores faltantes 
#row.names(trabalhosetor.RG) <- categorias                               
#
#trabalhosetor.RG <- trabalhosetor.RG %>%
#  select(-'interaction(V2010, V2007)', -'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9', -'se10', -'se11', -'se12')%>%
#  rename('agropecuária'=VD401001, 'indústria'=VD401002, 'construção'=VD401003, 'comércio'=VD401004, 'transporte'=VD401005, 'alimentação'=VD401006, 'informação'=VD401007, 'administraçãopública'=VD401008, 'educação'=VD401009, 'outrosserviços'=VD401010, 'serviçosdomésticos'=VD401011, 'maldefinido'=VD401012) 
#
#trabalhosetor.RG$Ano <- i
#trabalhosetor.RG$Quarter <- q
#trabalhosetor.RG$Day <- 01
#trabalhosetor.RG$Month <- ifelse(trabalhosetor.RG$Quarter==1,1,
#                                 ifelse(trabalhosetor.RG$Quarter==2,4,
#                                        ifelse(trabalhosetor.RG$Quarter==3,7,10)))
#trabalhosetor.RG$Trimestre<-as.yearqtr(with(trabalhosetor.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#trabalhosetor.RG$Trimestre<- as.character(trabalhosetor.RG$Trimestre)
#
#trabalhosetor.RG <- trabalhosetor.RG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#trabalhosetor.RG["Categoria"] <- c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#
#trabalhosetor.RG <- trabalhosetor.RG %>%
#  select(Categoria,Trimestre,agropecuária,indústria,construção,comércio,transporte,alimentação,
#         informação,administraçãopública,educação,outrosserviços,serviçosdomésticos,maldefinido)
#
#trabalhosetor.RG <- trabalhosetor.RG %>%
#  rename('Agropecuária'='agropecuária', 'Indústria'='indústria', 'Construção'='construção', 'Comércio'='comércio', 'Transporte'='transporte', 'Alimentação'='alimentação', 'Informação'='informação', 'Administração Pública'='administraçãopública', 'Educação'='educação', 'Outros serviços'='outrosserviços', 'Serviços domésticos'='serviçosdomésticos', 'Mal definido'='maldefinido') 
#
#trabalhosetor.RG.sh <- rbind(trabalhosetor.RG.sh,trabalhosetor.RG)
#
##Trabalho principal por setor - por grupo e por região
#
#trabalhosetor.RG.REG<- svyby(formula = ~ VD4010, 
#                             by = ~ interaction(V2010, V2007, UF), #Raca, Genero
#                             design = pnad, 
#                             svymean, # Fun??o para gerar estat?stica de interesse
#                             na.rm = T) # Remover valores faltantes 
#row.names(trabalhosetor.RG.REG) <- categoriasregiao                               
#
#trabalhosetor.RG.REG <- trabalhosetor.RG.REG %>%
#  select(-'interaction(V2010, V2007, UF)', -'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9', -'se10', -'se11', -'se12')%>%
#  rename('Agropecuária'=VD401001, 'Indústria'=VD401002, 'Construção'=VD401003, 'Comércio'=VD401004, 'Transporte'=VD401005, 'Alimentação'=VD401006, 'Informação'=VD401007, 'Administração Pública'=VD401008, 'Educação'=VD401009, 'Outros serviços'=VD401010, 'Serviços domésticos'=VD401011, 'Mal definido'=VD401012) 
#
#trabalhosetor.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
#                                        "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
#                                        "Homem Branco Nordeste","Homem Negro Nordeste",
#                                        "Mulher Branca Nordeste", "Mulher Negra Nordeste",
#                                        "Homem Branco Norte", "Homem Negro Norte",
#                                        "Mulher Branca Norte", "Mulher Negra Norte",
#                                        "Homem Branco Sudeste", "Homem Negro Sudeste",
#                                        "Mulher Branca Sudeste", "Mulher Negra Sudeste",
#                                        "Homem Branco Sul", "Homem Negro Sul",
#                                        "Mulher Branca Sul", "Mulher Negra Sul")
#
## Trabalho principal por setor - somente por região
#
#
#trabalhosetor.REG<- svyby(formula = ~ VD4010, 
#                          by = ~ interaction(UF), #Raca, Genero
#                          design = pnad, 
#                          svymean, # Fun??o para gerar estat?stica de interesse
#                          na.rm = T) # Remover valores faltantes 
#row.names(trabalhosetor.REG) <- regiaobrasil                               
#
#trabalhosetor.REG <- trabalhosetor.REG %>%
#  select(-'interaction(UF)', -'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9', -'se10', -'se11', -'se12')%>%
#  rename('agropecuária'=VD401001, 'indústria'=VD401002, 'construção'=VD401003, 'comércio'=VD401004, 'transporte'=VD401005, 'alimentação'=VD401006, 'informação'=VD401007, 'administraçãopública'=VD401008, 'educação'=VD401009, 'outrosserviços'=VD401010, 'serviçosdomésticos'=VD401011, 'maldefinido'=VD401012) 
#
#
#trabalhosetor.REG$Ano <- i
#trabalhosetor.REG$Quarter <- q
#trabalhosetor.REG$Day <- 01
#trabalhosetor.REG$Month <- ifelse(trabalhosetor.REG$Quarter==1,1,
#                                  ifelse(trabalhosetor.REG$Quarter==2,4,
#                                         ifelse(trabalhosetor.REG$Quarter==3,7,10)))
#trabalhosetor.REG$Trimestre<-as.yearqtr(with(trabalhosetor.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#trabalhosetor.REG$Trimestre<- as.character(trabalhosetor.REG$Trimestre)
#
#trabalhosetor.REG <- trabalhosetor.REG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#trabalhosetor.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
#
#trabalhosetor.REG <- trabalhosetor.REG %>%
#  select(Regiao,Trimestre,agropecuária,indústria,construção,comércio,transporte,alimentação,
#         informação,administraçãopública,educação,outrosserviços,serviçosdomésticos,maldefinido)
#
#trabalhosetor.REG <- trabalhosetor.REG %>%
#  rename('Agropecuária'='agropecuária', 'Indústria'='indústria', 'Construção'='construção', 'Comércio'='comércio', 'Transporte'='transporte', 'Alimentação'='alimentação', 'Informação'='informação', 'Administração Pública'='administraçãopública', 'Educação'='educação', 'Outros serviços'='outrosserviços', 'Serviços domésticos'='serviçosdomésticos', 'Mal definido'='maldefinido') 
#
#trabalhosetor.REG.sh <- rbind(trabalhosetor.REG.sh,trabalhosetor.REG)
#
### Escolaridade da Força de Trabalho
#
## subset pessoas ocupadas
#pnad.ocup <- subset(pnad, pnad$variables$VD4001 %in% c("1"))
#
## ESCOLARIDADE POR GRUPO (V3009A)
#
#ESCOLARIDADE.RG <- svyby(formula = ~ V3009A, 
#                         by = ~ interaction(V2010, V2007), #Raca, Genero
#                         design = pnad.ocup, 
#                         svymean,   
#                         na.rm = T) # Remover valores faltantes 
#
#row.names(ESCOLARIDADE.RG) <- categorias
#ESCOLARIDADE.RG <- ESCOLARIDADE.RG %>%
#  select(-'interaction(V2010, V2007)',-'se1',-'se2',-'se3',-'se4',-'se5',-'se6')%>%
#  rename('Pré-fundamental'=V3009A1,'Ensino fundamental'=V3009A2,'Ensino médio'=V3009A3, 'Ensino superior'= V3009A4,'Especializacao/pós-graduação'= V3009A5, 'Mestrado/doutorado'= V3009A6)
#
##ESCOLARIDADE POR GRUPO (V3009A) e por região
#
#ESCOLARIDADE.RG.REG <- svyby(formula = ~ V3009A, # Renda
#                             by = ~ interaction(V2010, V2007, UF), #Raca, Genero
#                             design = pnad.ocup, 
#                             svymean, # Fun??o para gerar estat?stica de interesse
#                             na.rm = T) # Remover valores faltantes 
#
#row.names(ESCOLARIDADE.RG.REG) <- categoriasregiao
#ESCOLARIDADE.RG.REG <- ESCOLARIDADE.RG.REG %>%
#  select(-'interaction(V2010, V2007, UF)',-'se1',-'se2',-'se3',-'se4',-'se5',-'se6')%>%
#  rename('Pré-fundamental'=V3009A1,'Ensino fundamental'=V3009A2,'Ensino médio'=V3009A3, 'Ensino superior'= V3009A4,'Especializacao/pós-graduação'= V3009A5, 'Mestrado/doutorado'= V3009A6)
#
## ESCOLARIDADE (V3009A) somente por região
#
#ESCOLARIDADE.REG <- svyby(formula = ~ V3009A, # Renda
#                          by = ~ interaction(UF), #Raca, Genero
#                          design = pnad.ocup, 
#                          svymean, # Fun??o para gerar estat?stica de interesse
#                          na.rm = T) # Remover valores faltantes 
#
#row.names(ESCOLARIDADE.REG) <- regiaobrasil
#ESCOLARIDADE.REG <- ESCOLARIDADE.REG %>%
#  select(-'interaction(UF)',-'se1',-'se2',-'se3',-'se4',-'se5',-'se6')%>%
#  rename('Pré-fundamental'=V3009A1,'Ensino fundamental'=V3009A2,'Ensino médio'=V3009A3, 'Ensino superior'= V3009A4,'Especializacao/pós-graduação'= V3009A5, 'Mestrado/doutorado'= V3009A6)
#
## Rendimento
#
### Rendimento Médio Efetivo e Habitual
#
## RENDA EFETIVA média (VD4016)
#
#RendaEfetivaMedia <- svymean(x=~EfetivaDef22, 
#                             design = pnad, 
#                             na.rm = T) # Remover valores faltantes 
#
#RendaEfetivaMedia <- as.data.frame(RendaEfetivaMedia[1])
#colnames(RendaEfetivaMedia)[colnames(RendaEfetivaMedia) == "RendaEfetivaMedia[1]"] ="RendaEfetivaMedia"
#
#RendaEfetivaMedia$Ano <- i
#RendaEfetivaMedia$Quarter <- q
#RendaEfetivaMedia$Day <- 01
#RendaEfetivaMedia$Month <- ifelse(RendaEfetivaMedia$Quarter==1,1,
#                                  ifelse(RendaEfetivaMedia$Quarter==2,4,
#                                         ifelse(RendaEfetivaMedia$Quarter==3,7,10)))
#RendaEfetivaMedia$Trimestre<-as.yearqtr(with(RendaEfetivaMedia,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#RendaEfetivaMedia$Trimestre<- as.character(RendaEfetivaMedia$Trimestre)
#
#RendaEfetivaMedia <- RendaEfetivaMedia %>%
#  select('Trimestre','RendaEfetivaMedia',-'Ano', -'Quarter', -'Day',-'Month')
#
#RendaEfetivaMedia.sh<- rbind(RendaEfetivaMedia.sh,RendaEfetivaMedia)
#
## RENDA EFETIVA POR GRUPO (VD4016)
#
#RendaEfet.RG <- svyby(formula = ~ EfetivaDef22, # Renda
#                      by = ~ interaction(V2010, V2007), #Raca, Genero
#                      design = pnad, 
#                      svymean, # Fun??o para gerar estat?stica de interesse
#                      na.rm = T) # Remover valores faltantes 
#
#row.names(RendaEfet.RG) <-categorias
#RendaEfet.RG <- RendaEfet.RG %>%
#  select(-'interaction(V2010, V2007)', -'se')%>%
#  rename('Renda_Efetiva_RG' = EfetivaDef22)
#
#RendaEfet.RG$Ano <- i
#RendaEfet.RG$Quarter <- q
#RendaEfet.RG$Day <- 01
#RendaEfet.RG$Month <- ifelse(RendaEfet.RG$Quarter==1,1,
#                             ifelse(RendaEfet.RG$Quarter==2,4,
#                                    ifelse(RendaEfet.RG$Quarter==3,7,10)))
#RendaEfet.RG$Trimestre<-as.yearqtr(with(RendaEfet.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#RendaEfet.RG$Trimestre<- as.character(RendaEfet.RG$Trimestre)
#
#RendaEfet.RG <- RendaEfet.RG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#RendaEfet.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#
#RendaEfet.RG <- RendaEfet.RG %>%
#  select('Categoria','Trimestre', 'Renda_Efetiva_RG')
#
#RendaEfet.RG.sh <- rbind(RendaEfet.RG.sh,RendaEfet.RG)
#
## RENDA habitual média  (VD4016)


RendaHabitMedia <- svymean(x=~HabitualDef22, 
                           design = pnad, 
                           na.rm = T) # Remover valores faltantes 
RendaHabitMedia <- as.data.frame(RendaHabitMedia[1])
colnames(RendaHabitMedia)[colnames(RendaHabitMedia) == "RendaHabitMedia[1]"] ="RendaHabitMedia"

RendaHabitMedia$Ano <- i
RendaHabitMedia$Quarter <- q
RendaHabitMedia$Day <- 01
RendaHabitMedia$Month <- ifelse(RendaHabitMedia$Quarter==1,1,
                                ifelse(RendaHabitMedia$Quarter==2,4,
                                       ifelse(RendaHabitMedia$Quarter==3,7,10)))
RendaHabitMedia$Trimestre<-as.yearqtr(with(RendaHabitMedia,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
RendaHabitMedia$Trimestre<- as.character(RendaHabitMedia$Trimestre)

RendaHabitMedia <- RendaHabitMedia %>%
  select('Trimestre','RendaHabitMedia',-'Ano', -'Quarter', -'Day',-'Month')

RendaHabitMedia.sh<- rbind(RendaHabitMedia.sh,RendaHabitMedia)

# RENDA HABITUAL POR GRUPO (VD4017)

RendaHabit.RG <- svyby(formula = ~ HabitualDef22, # Renda
                       by = ~ interaction(V2010, V2007), #Raca, Genero
                       design = pnad, 
                       svymean, # Fun??o para gerar estat?stica de interesse
                       na.rm = T) # Remover valores faltantes 

row.names(RendaHabit.RG ) <-categorias
RendaHabit.RG <- RendaHabit.RG %>%
  select(-'interaction(V2010, V2007)', -'se')%>%
  rename('Renda_Habitual_RG' = HabitualDef22)

RendaHabit.RG$Ano <- i
RendaHabit.RG$Quarter <- q
RendaHabit.RG$Day <- 01
RendaHabit.RG$Month <- ifelse(RendaHabit.RG$Quarter==1,1,
                              ifelse(RendaHabit.RG$Quarter==2,4,
                                     ifelse(RendaHabit.RG$Quarter==3,7,10)))
RendaHabit.RG$Trimestre<-as.yearqtr(with(RendaHabit.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
RendaHabit.RG$Trimestre<- as.character(RendaHabit.RG$Trimestre)

RendaHabit.RG <- RendaHabit.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

RendaHabit.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

RendaHabit.RG <- RendaHabit.RG %>%
  select("Categoria", "Trimestre", "Renda_Habitual_RG")

RendaHabit.RG.sh <- rbind(RendaHabit.RG.sh,RendaHabit.RG)

#Coeficiente de pobreza
Pessoas_na_pobreza <- svymean(x=~Linha_pobreza,
                              design = pnad, 
                              na.rm = T) # Remover valores faltantes 


## Índice de Gini

## Indice de GINI - por regiao
#
#pnad <- pnad %>%
#  convey::convey_prep()
#giniUF <- svyby(formula=~VD4020, by=~UF, design=pnad, FUN=svygini, na.rm=TRUE)
#
#giniUF <- giniUF %>%
#  select(-se.VD4020, -UF) %>%
#  rename("gini" = VD4020)
#
#giniUF$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
#
#giniUF$Ano <- i
#giniUF$Quarter <- q
#giniUF$Day <- 01
#giniUF$Month <- ifelse(giniUF$Quarter==1,1,
#                       ifelse(giniUF$Quarter==2,4,
#                              ifelse(giniUF$Quarter==3,7,10)))
#giniUF$Trimestre<-as.yearqtr(with(giniUF,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#giniUF$Trimestre<- as.character(giniUF$Trimestre)
#
#giniUF <- giniUF %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#giniUF.sh <- rbind(giniUF.sh,giniUF)
#
#giniUF <- giniUF %>%
#  select(-'Trimestre')
#
##Indice de GINI -BR
#
##pnad <- pnad %>%
##convey::convey_prep()
#giniBR <- svygini(formula=~VD4020, design=pnad, na.rm=TRUE)
#giniBR <- data.frame(giniBR)
#

#giniBR$Ano <- i
#giniBR$Quarter <- q
#giniBR$Day <- 01
#giniBR$Month <- ifelse(giniBR$Quarter==1,1,
#                       ifelse(giniBR$Quarter==2,4,
#                              ifelse(giniBR$Quarter==3,7,10)))
#giniBR$Trimestre<-as.yearqtr(with(giniBR,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#giniBR$Trimestre<- as.character(giniBR$Trimestre)
#
#giniBR <- giniBR %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month',-'SE')
#
#colnames(giniBR.sh)[colnames(giniBR.sh) == "Gini Brasil"] ="gini"
#
#giniBR.sh <- rbind(giniBR.sh,giniBR)
#
#giniBR <- giniBR %>%
#  select(-'Trimestre')
#
### Indice de GINI - por raça e gênero
#
##pnad <- pnad %>%
##convey::convey_prep()
#giniRG <- svyby(formula=~VD4020, by=~ interaction(V2010, V2007), design=pnad, FUN=svygini, na.rm=TRUE)
#row.names(giniRG) <- categorias 
#giniRG <- giniRG %>%
#  select(-'interaction(V2010, V2007)', -'se.VD4020')%>%
#  rename('gini' = VD4020)
#
#giniRG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#
#
#giniRG$Ano <- i
#giniRG$Quarter <- q
#giniRG$Day <- 01
#giniRG$Month <- ifelse(giniRG$Quarter==1,1,
#                       ifelse(giniRG$Quarter==2,4,
#                              ifelse(giniRG$Quarter==3,7,10)))
#giniRG$Trimestre<-as.yearqtr(with(giniRG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#giniRG$Trimestre<- as.character(giniRG$Trimestre)
#
#giniRG <- giniRG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#colnames(giniRG.sh)[colnames(giniRG.sh) == "Gini Brasil"] ="gini"
#
#giniRG.sh <- rbind(giniRG.sh,giniRG)
#
#giniRG <- giniRG %>%
#  select(-'Trimestre')
#
### Percentis da Renda
#
## apropriação da renda
# obtendo cada um dos decis de renda e o último percentil (ou seja, o valor equivalente ao 1% mais rico da população)

#pnad$variables$EfetivaDef23todos <- pnad$variables$Efetivo * pnad$variables$VD4020
#quanti2303 <- svyquantile(x = ~ EfetivaDef23todos,
#                          design = pnad,
#                          quantiles = c(seq(from = .1, to = .9, by = .1), .99),
#                          na.rm = T)
#
#quanti2303
#
##obtendo renda total
#
#rentot2303 <- svytotal(x = ~ EfetivaDef23todos,
#                       design = pnad,
#                       na.rm = T)
#
#rentot2303
#
#sum(pnad$variables$EfetivaDef23todos, na.rm = T) - rentot2303[1]
#
##apropriação da renda
#
#
#quantis <- c(0, quanti2303$EfetivaDef23todos[,1], max(pnad$variables$EfetivaDef23todos, na.rm = T))
#
#names(quantis) <- c("0", rownames(quanti2303$EfetivaDef23todos), "Max")
#
#for (quant in 2:length(quantis)) {
#  
#  rotulo <- paste(names(quantis)[c(quant - 1, quant)], collapse = " - ")
#  
#  pnad$variables$Quant[pnad$variables$EfetivaDef23todos >= quantis[quant - 1] &
#                         pnad$variables$EfetivaDef23todos <= quantis[quant]] <- rotulo
#  
#}
#
#pnad.Quant0 <- subset(pnad, Quant == "0 - 0.1")
#
#unique(pnad.Quant0$prob[pnad.Quant0$variables$EfetivaDef23todos >= 17000])
#
#cat <- setdiff(unique(pnad$variables$Quant), NA) #Obter os decis
#cat
#ordem_final <- c("0 - 0.1","0.1 - 0.2","0.2 - 0.3","0.3 - 0.4", 
#                 "0.4 - 0.5", "0.5 - 0.6",  "0.6 - 0.7" , "0.7 - 0.8",
#                 "0.8 - 0.9", "0.9 - 0.99", "0.99 - Max") #estabelecer vetor de ordem dos quantis
#ordem_real <- c()
#
#for (value in ordem_final){
#  for (i in 1:length(cat)){
#    if(cat[[i]]==value){
#      ordem_real <- append(ordem_real, i)
#    }
#  }
#}
#cat <- cat[ordem_real] #Ordenar os decis automaticamente
#
#totquant2303 <- c()
#
#for (quant in cat) {
#  
#  total <- svytotal(x = ~ EfetivaDef23todos,
#                    design = subset(pnad, Quant == quant),
#                    na.rm = T)
#  
#  totquant2303 <- c(totquant2303, total)
#  
#}
#
#
#names(totquant2303) <- c(paste(seq(0, .9, .1)), "0.99")
#
#quantis_de_renda23_04 <- as.data.frame(totquant2303/rentot2303 * 100)
#quantis_de_renda23_04 <- quantis_de_renda23_04 %>% 
#  rename('Percentual da Renda Apropriada' = `totquant2303/rentot2303 * 100`)
#
#percentis <- totquant2303/rentot2303 * 100
#
#percentis <- as.data.frame(percentis)
#
#colnames(percentis)[colnames(percentis) == "percentis"] ="% da renda apropriado"
#
#
### Curva de Lorenz
## Carrega o pacote ggplot2
#library(ggplot2)
#
## Dados
#x <- seq(0, 1, 0.1)
#y <- c(cumsum(totquant2303[-11]/rentot2303), 1)
#
## Cria o gráfico com ggplot2
#grafico <- ggplot(data.frame(x, y), aes(x, y)) +
#  geom_line(color = "#45ff66", size=1.5) +
#  labs(x = "Quantil", y = "% da Renda", title = "Curva de Lorenz") 
#
## Adiciona a linha de 45 graus
#grafico + geom_abline(intercept = 0, slope = 1, color = "#eb52ff",size=0.7)
#curva_de_lorenz <- grafico + geom_abline(intercept = 0, slope = 1, color = "#eb52ff",size=0.7)
#ggsave("curva_de_lorenz_2023_03.png", plot = curva_de_lorenz)
#
#
#
#### Salvando dados
#
### todos os dados
rm(pnad,pnad.Quant0,pnad.ocup)
#load("C:/Users/USER/Documents/PNAD/PNAD_dados_relatorio_04_2023.RData")
#save.image("C:/Users/USER/Documents/PNAD/PNAD_dados_relatorio_04_2023.RData")
save.image("H:/Meu Drive/Documentos/Made/relatorio_pnad/PNAD_dados_relatorio_04_2023.RData")

## série histórica 
rm (EMPREGO.REG, EMPREGO.RG, EMPREGO.RG.REG, giniBR, giniRG, giniUF, informal.REG, 
    informal.RG, informal.RG.REG, informalidadeMedia, PEA.REG, PEA.RG, PEA.RG.REG,
    RendaEfet.RG, RendaEfetivaMedia, RendaHabit.RG, trabalhosetor.REG, trabalhosetor.RG, 
    trabalhosetor.RG.REG, Tx_desemprego, Tx_emprego,  i, q, SUBOCUPHORAS.REG, 
    SUBOCUPHORAS.RG, percentis, curva_de_lorenz) #tudo que não tem SH 

#save.image("C:/Users/USER/Documents/PNAD/dados_serie_historica_042023.RData")
save.image("H:/Meu Drive/Documentos/Made/relatorio_pnad/dados_serie_historica_042023.RData")



# Passando os dados para excel
library(dplyr)
library(tidyverse)

setwd("H:/Meu Drive/Documentos/Made/relatorio_pnad/tabelas")

lista.RG = list(EMPREGO.RG.sh,giniRG.sh,informal.RG.sh,PEA.RG.sh,RendaEfet.RG.sh,RendaHabit.RG.sh, trabalhosetor.RG.sh)
final.RG <- lista.RG %>% reduce (inner_join, by =c('Categoria','Trimestre'))

lista.REG <- list(EMPREGO.REG.sh,giniUF.sh,informal.REG.sh,PEA.REG.sh, trabalhosetor.REG.sh)
final.REG <- lista.REG %>% reduce (inner_join, by =c('Regiao','Trimestre'))

lista.RG.REG <- list(EMPREGO.RG.REG.sh, informal.RG.REG.sh, PEA.RG.REG.sh)
final.RG.REG <- lista.RG.REG %>% reduce (inner_join, by =c('Categoria_regiao','Trimestre'))

lista.outras <- list(giniBR.sh,informalidadeMedia.sh, RendaEfetivaMedia.sh, RendaHabitMedia.sh, Tx_desemprego.sh,Tx_emprego.sh)
final.outras <- lista.outras %>% reduce (inner_join, by ='Trimestre')

lista.novas.RG <- list(carteira.RG, pobreza.RG, extrema.pobreza.RG)
final.novas.RG <- lista.novas.RG %>% reduce (inner_join, by =c('Regiao','Trimestre'))

ista.novas.RG.REG

lista.novas.REG

#Transformar em csv
write.csv(quantis_de_renda23_04, "RelatorioPNAD_Dados_Renda_2304.csv")  #Gráfico de perecentis de renda
write.csv(final.RG, file = "RelatorioPNAD_Dados_RG_2304.csv")
write.csv(final.REG, file = "RelatorioPNAD_Dados_REG_2304.csv")
write.csv(final.RG.REG, file = "RelatorioPNAD_Dados_RG_REG_2304.csv")
write.csv(final.outras, file ="RelatorioPNAD_Dados_outras_sh_2304.csv")

