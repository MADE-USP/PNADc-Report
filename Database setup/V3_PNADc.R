# Opening packages--------------------------------------------------------------

library(PNADcIBGE)
library(survey)
library(convey)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(zoo)
library(ggplot2)

# Defining year and quarter of reference----------------------------------------

i=2024 #year
q=1 #quarter


# Importing data into an R object (pnad) using get_pnadc command----------------

pnad <- get_pnadc(year = i,                        
                  quarter = q,
                  defyear = 2023, # The year of the deflator data to be downloaded
                  defperiod = 1, # The quarter period of the deflator data to be downloaded
                  deflator = T, # If T, deflator variables will be available
                  labels = FALSE, # If FALSE, labels corresponding to the survey's dictionary will not be presented
                  vars = c("V1028", # weights (w/ post stratification)
                           "V2007", # Sex
                           "V2005", # Condição no domicílio
                           "V2009", # Age
                           "V2010", # Race
                           "V4012", # Employment
                           "VD4001", # Employment situation (Condition in relation to the labor force)
                           "VD4002", # Employment situation
                           "VD4004A",# Underoccupation (hours worked)
                           "VD4009", # Position in occupation and employment category (age > 14)
                           "VD4016", # usual ("habitual") monthly income (main job)
                           "VD4017", # Actual ("efetivo") monthly income (main job)
                           "VD4030", # Reason for not looking for work/not wanting to work/unavailable to start a job
                           "V3009A", # Highest attended course
                           "VD3001", # Highest education level 
                           "V3012",  # Passed the first grade of this course
                           "VD4020", # Actual ("efetivo") monthly income from all jobs (> 14) 
                           "VD4012", # Contributes to social security institute (> 14)
                           "VD4010")) # Sector of main job

# Saving data base on computer--------------------------------------------------

save(pnad,file='C:/Users/lauro/Desktop/PNADc/pnadc_teste_todos_dados')

# Loading database--------------------------------------------------------------

load('C:/Users/lauro/Desktop/PNADc/pnadc_teste_todos_dados')

# Loading historic series-------------------------------------------------------

load('C:/Users/lauro/Downloads/dados_serie_historica_042023 (1).RData')

# Adjusting gender, race and region variables-----------------------------------

# Aggregating race variables----------------------------------------------------

pnad$variables$V2010[pnad$variables$V2010 %in% c("2", "4")] <- "2"
pnad$variables$V2010[pnad$variables$V2010 %in% c("5", "9")] <- NA
pnad$variables$V2010[pnad$variables$V2010 %in% c("3")] <- NA

# The code chunks above are combining categories
# 1 = white; 2 = black; 3 = yellow; 
# 4 = people of colour; 5 = indigenous; 9 = ignored

#unique(pnad$variables$V2010)
# Function should return 1, 2, NA as values


# Calculating actual ("efetivo") monthly income (main job)----------------------
# Deflated

pnad$variables$EfetivaDef22 <- pnad$variables$Efetivo * pnad$variables$VD4017

# Calculating actual ("efetivo") monthly income (ALL jobs)----------------------
# Deflated

pnad$variables$EfetivaDef22todos <- pnad$variables$Efetivo * pnad$variables$VD4020


# Calculating usual ("habitual") monthly income---------------------------------
# Deflated

pnad$variables$HabitualDef22 <- pnad$variables$Habitual * pnad$variables$VD4016


# Aggregating school categories-------------------------------------------------

pnad$variables$V3009A[pnad$variables$V3009A %in% c("01","02","03","04")] <- "1"
pnad$variables$V3009A[pnad$variables$V3009A %in% c("05","07","08")] <- "2"
pnad$variables$V3009A[pnad$variables$V3009A %in% c("06","09","10","11")] <- "3"
pnad$variables$V3009A[pnad$variables$V3009A %in% c("12")] <- "4"
pnad$variables$V3009A[pnad$variables$V3009A %in% c("13")] <- "5"
pnad$variables$V3009A[pnad$variables$V3009A %in% c("14","15")] <- "6"

# The code chunks above are combining categories
# 1 = pre-school (pré-fundamental); 2 = elementary school ("ensino fundamental"); 
# 3 = high school ("ensino medio"; 4 = higher education (ensino superior); 
# 5 = postgrad specialization (especializacao pos-graduacao); 
# 6 = master's/doctoal (mestrado/doutorado)

#unique(pnad$variables$V3009A)

#Transforming region variables--------------------------------------------------

pnad$variables$UF[pnad$variables$UF %in% c("11", "12", "13", "14", "15", "16", "17")] <- "Norte"
pnad$variables$UF[pnad$variables$UF %in% c("21", "22", "23", "24", "25", "26", "27", "28", "29")] <- "Nordeste"
pnad$variables$UF[pnad$variables$UF %in% c("31", "32", "33", "35")] <- "Sudeste"
pnad$variables$UF[pnad$variables$UF %in% c("41", "42", "43")] <- "Sul"
pnad$variables$UF[pnad$variables$UF %in% c("50", "51", "52", "53")] <- "Centro-Oeste"

# The code chunks above are combining the federative units into regions
#1 = norte; 2 = nordeste; 3 = centro-oeste; 4 = sudeste; 5 = sul

# Updating missing values (from NA to 0)----------------------------------------
# VD4004A Underoccupation (hours worked) ("Subocupação por insuficiência de horas habitualmente trabalhadas")
# VD4002 Employment situation ("Condição de ocupação")

pnad$variables$VD4004A[is.na(pnad$variables$VD4004A)&(pnad$variables$VD4002 == "1")] <- "0"

# Combining race, sex and region categories-------------------------------------

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

# Creating object (in alphabetic order)-----------------------------------------
regiaobrasil<-c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

# Creating a subset of women performing domestic work---------------------------
#domesticas <- subset(pnad, V4012=="Trabalhador doméstico" & V2007=="Mulher")

# Calculating indicators--------------------------------------------------------

# Economically active population by group ("PEA por grupo")---------------------
# Variable VD4001 ("Condição em relação à força de trabalho")

PEA.RG <- svyby(formula = ~ VD4001, 
                by = ~ interaction(V2010, V2007), #Race (V2010) and gender (V2007)
                design = pnad, 
                svymean, # Generates statistic of interest
                  na.rm = T) # Dropping NA 

row.names(PEA.RG) <- categorias

PEA.RG <- PEA.RG %>%
  select(-'se1', -'interaction(V2010, V2007)', -'se2') %>%
  rename('PEA' = VD40011, 'Fora da PEA' = VD40012)

# Adding column with year and quarter to the data frame-------------------------
PEA.RG$Ano <- i
PEA.RG$Quarter <- q
PEA.RG$Day <- 01
PEA.RG$Month <- ifelse(PEA.RG$Quarter==1,1,
                       ifelse(PEA.RG$Quarter==2,4,
                              ifelse(PEA.RG$Quarter==3,7,10)))
PEA.RG$Trimestre<-as.yearqtr(with(PEA.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")


PEA.RG <- PEA.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

PEA.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

PEA.RG$Trimestre <- as.character(PEA.RG$Trimestre)

PEA.RG.sh <- rbind(PEA.RG.sh,PEA.RG)

PEA.RG <- PEA.RG %>%
  select(-'Trimestre')

# Economically active population by group (VD4001) and region-------------------
# "PEA por grupo e região"
# Variable: VD4001 ("Condição em relação à força de trabalho")


PEA.RG.REG <- svyby(formula = ~ VD4001, 
                    by = ~ interaction(V2010, V2007, UF), #Race (V2010) and gender (V2007)
                    design = pnad, 
                    svymean, # Generates statistic of interest
                    na.rm = T) # Dropping NA 

# Naming rows of the dataframe
row.names(PEA.RG.REG) <-categoriasregiao

# Removing columns
PEA.RG.REG <- PEA.RG.REG %>%
  select(-'se1', -'interaction(V2010, V2007, UF)', -'se2')%>%
  rename('PEA' = VD40011, 'Fora da PEA' = VD40012)

# Adding column with year and quarter to the data frame-------------------------
PEA.RG.REG$Ano <- i
PEA.RG.REG$Quarter <- q
PEA.RG.REG$Day <- 01
PEA.RG.REG$Month <- ifelse(PEA.RG.REG$Quarter==1,1,
                           ifelse(PEA.RG.REG$Quarter==2,4,
                                  ifelse(PEA.RG.REG$Quarter==3,7,10)))

# Creating column
PEA.RG.REG$Trimestre<-as.yearqtr(with(PEA.RG.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")

# Remoing extra columns
PEA.RG.REG <- PEA.RG.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

PEA.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
                              "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
                              "Homem Branco Nordeste","Homem Negro Nordeste",
                              "Mulher Branca Nordeste", "Mulher Negra Nordeste",
                              "Homem Branco Norte", "Homem Negro Norte",
                              "Mulher Branca Norte", "Mulher Negra Norte",
                              "Homem Branco Sudeste", "Homem Negro Sudeste",
                              "Mulher Branca Sudeste", "Mulher Negra Sudeste",
                              "Homem Branco Sul", "Homem Negro Sul",
                              "Mulher Branca Sul", "Mulher Negra Sul")

PEA.RG.REG$Trimestre <- as.character(PEA.RG.REG$Trimestre)

# Binding
PEA.RG.REG.sh <- rbind(PEA.RG.REG.sh,PEA.RG.REG)

PEA.RG.REG <- PEA.RG.REG %>%
  select(-'Trimestre')

# Economically active population by region--------------------------------------
#"PEA POR regiao"
# Variable: VD4001 ("Condição em relação à força de trabalho")

PEA.REG <- svyby(formula = ~ VD4001, 
                 by = ~ interaction(UF), 
                 design = pnad, 
                 svymean, # Generates statistic of interest
                 na.rm = T) # Dropping NA 

# Naming rows of the dataframe
row.names(PEA.REG) <- regiaobrasil

# Removing columns of the dataframe
PEA.REG <- PEA.REG %>%
  select(-'se1', -'interaction(UF)', -'se2')%>%
  rename('PEA' = VD40011, 'Fora da PEA' = VD40012)

# Adding column with year and quarter to the data frame-------------------------
PEA.REG$Ano <- i
PEA.REG$Quarter <- q
PEA.REG$Day <- 01
PEA.REG$Month <- ifelse(PEA.REG$Quarter==1,1,
                        ifelse(PEA.REG$Quarter==2,4,
                               ifelse(PEA.REG$Quarter==3,7,10)))

# Adding column "Trimestre"
PEA.REG$Trimestre <- as.yearqtr(with(PEA.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")

# Removing columns
PEA.REG <- PEA.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

PEA.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

PEA.REG$Trimestre <- as.character(PEA.REG$Trimestre)

# Binding to historic series
PEA.REG.sh <- rbind(PEA.REG.sh,PEA.REG)

PEA.REG <- PEA.REG %>%
  select(-'Trimestre')

# Employment and unemployment---------------------------------------------------

# Occupation--------------------------------------------------------------------

# Average occupation rate ("Taxa de ocupação média")----------------------------
# Variable: VD4002 ("Condição de ocupação")

EmpregoMedia <- svymean(x=~VD4002, 
                        design = pnad, 
                        na.rm = T) # Dropping NA 

Tx_emprego <- as.data.frame(EmpregoMedia[1])

# Renaming columns
colnames(Tx_emprego)[colnames(Tx_emprego) == "EmpregoMedia[1]"] ="Taxa de emprego"


# Adding column with year and quarter to the data frame-------------------------
Tx_emprego$Ano <- i
Tx_emprego$Quarter <- q
Tx_emprego$Day <- 01
Tx_emprego$Month <- ifelse(Tx_emprego$Quarter==1,1,
                           ifelse(Tx_emprego$Quarter==2,4,
                                  ifelse(Tx_emprego$Quarter==3,7,10)))

# Adding column "Trimestre"
Tx_emprego$Trimestre<-as.yearqtr(with(Tx_emprego,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")

Tx_emprego$Trimestre<- as.character(Tx_emprego$Trimestre)

# Removing unnecessary columns
Tx_emprego <- Tx_emprego %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

# Binding to historic series
Tx_emprego.sh <- rbind(Tx_emprego.sh,Tx_emprego)

# Creating data frame
Tx_desemprego <- as.data.frame(EmpregoMedia[2])
colnames(Tx_desemprego)[colnames(Tx_desemprego) == "EmpregoMedia[2]"] ="Taxa de desemprego"
Tx_desemprego$Ano <- i
Tx_desemprego$Quarter <- q
Tx_desemprego$Day <- 01
Tx_desemprego$Month <- ifelse(Tx_desemprego$Quarter==1,1,
                              ifelse(Tx_desemprego$Quarter==2,4,
                                     ifelse(Tx_desemprego$Quarter==3,7,10)))

# Adding column "Trimestre"
Tx_desemprego$Trimestre<-as.yearqtr(with(Tx_desemprego,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
Tx_desemprego$Trimestre<- as.character(Tx_desemprego$Trimestre)

#Removing unnecessary columns
Tx_desemprego <- Tx_desemprego %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')


# Binding to historic series
Tx_desemprego.sh<- rbind(Tx_desemprego.sh,Tx_desemprego)

# Occupation rate by gender and race ("Taxa de ocupação por grupo")-------------
# Variable: VD4002 ("Condição de ocupação")

EMPREGO.RG <- svyby(formula = ~ VD4002, # "Condicao de ocupação"
                    by = ~ interaction(V2010, V2007), #Race (V2010) and gender (V2007)
                    design = pnad, 
                    svymean, # Generates statistic of interest
                    na.rm = T) # dROPPING na 

#Adding names to rows
row.names(EMPREGO.RG) <- categorias

# Removing unnecessary columns
EMPREGO.RG <- EMPREGO.RG %>%
  select(-'se1', -'interaction(V2010, V2007)', -'se2', -VD40022)%>%
  rename('Ocupado' = VD40021 )

# Adding column with year and quarter to the data frame-------------------------
EMPREGO.RG$Ano <- i
EMPREGO.RG$Quarter <- q
EMPREGO.RG$Day <- 01
EMPREGO.RG$Month <- ifelse(EMPREGO.RG$Quarter==1,1,
                           ifelse(EMPREGO.RG$Quarter==2,4,
                                  ifelse(EMPREGO.RG$Quarter==3,7,10)))

# Creating columns "Trimestre"
EMPREGO.RG$Trimestre<-as.yearqtr(with(EMPREGO.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
EMPREGO.RG$Trimestre<- as.character(EMPREGO.RG$Trimestre)

# Removing columns
EMPREGO.RG <- EMPREGO.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')
EMPREGO.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

# Binding to historic series
EMPREGO.RG.sh <- rbind(EMPREGO.RG.sh,EMPREGO.RG)
EMPREGO.RG <- EMPREGO.RG %>%
  select(-'Trimestre')

#  Occupation rate by gender, race and region ("Taxa de ocupação por grupo")----
# Variable: VD4002 ("Condição de ocupação") 

EMPREGO.RG.REG <- svyby(formula = ~ VD4002, # "Condição de ocupação"
                        by = ~ interaction(V2010, V2007, UF), #Race (V2010) and gender (V2007)
                        design = pnad, 
                        svymean, # Generates statistic of interest
                        na.rm = T) # Dropping NA 

# Adding names to rows
row.names(EMPREGO.RG.REG) <-categoriasregiao

# Removing columns
EMPREGO.RG.REG <- EMPREGO.RG.REG %>%
  select(-'se1', -'interaction(V2010, V2007, UF)', -'se2', -VD40022)%>%
  rename('Ocupado' = VD40021 )

# Adding column with year and quarter to the data frame------------------------- 
EMPREGO.RG.REG$Ano <- i
EMPREGO.RG.REG$Quarter <- q
EMPREGO.RG.REG$Day <- 01
EMPREGO.RG.REG$Month <- ifelse(EMPREGO.RG.REG$Quarter==1,1,
                               ifelse(EMPREGO.RG.REG$Quarter==2,4,
                                      ifelse(EMPREGO.RG.REG$Quarter==3,7,10)))

# Adding column "Trimestre"
EMPREGO.RG.REG$Trimestre<-as.yearqtr(with(EMPREGO.RG.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
EMPREGO.RG.REG$Trimestre<- as.character(EMPREGO.RG.REG$Trimestre)

# Removing columns
EMPREGO.RG.REG <- EMPREGO.RG.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

EMPREGO.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
                                  "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
                                  "Homem Branco Nordeste","Homem Negro Nordeste",
                                  "Mulher Branca Nordeste", "Mulher Negra Nordeste",
                                  "Homem Branco Norte", "Homem Negro Norte",
                                  "Mulher Branca Norte", "Mulher Negra Norte",
                                  "Homem Branco Sudeste", "Homem Negro Sudeste",
                                  "Mulher Branca Sudeste", "Mulher Negra Sudeste",
                                  "Homem Branco Sul", "Homem Negro Sul",
                                  "Mulher Branca Sul", "Mulher Negra Sul")

# Binding to historic series
EMPREGO.RG.REG.sh <- rbind(EMPREGO.RG.REG.sh,EMPREGO.RG.REG)
EMPREGO.RG.REG <- EMPREGO.RG.REG %>%
  select(-'Trimestre')

# Occupation rate by region----------------------------------------------------- 
# Variable: VD4002 ("Condição de ocupação")

EMPREGO.REG <- svyby(formula = ~ VD4002, # "Condicao de ocupação"
                     by = ~ interaction(UF), 
                     design = pnad, 
                     svymean, # Generates statistic of interest
                     na.rm = T) # Dropping NA 

# Naming rows
row.names(EMPREGO.REG) <-regiaobrasil

# Removing extra columns
EMPREGO.REG <- EMPREGO.REG %>%
  select(-'se1', -'interaction(UF)', -'se2', -VD40022)%>%
  rename('Ocupado' = VD40021 )

# Adding column with year and quarter to the data frame-------------------------
EMPREGO.REG$Ano <- i
EMPREGO.REG$Quarter <- q
EMPREGO.REG$Day <- 01
EMPREGO.REG$Month <- ifelse(EMPREGO.REG$Quarter==1,1,
                            ifelse(EMPREGO.REG$Quarter==2,4,
                                   ifelse(EMPREGO.REG$Quarter==3,7,10)))
# Creating column "Trimestre"
EMPREGO.REG$Trimestre<-as.yearqtr(with(EMPREGO.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
EMPREGO.REG$Trimestre<- as.character(EMPREGO.REG$Trimestre)

# Removing unnecessary columns
EMPREGO.REG <- EMPREGO.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

EMPREGO.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

#Binding to historic series
EMPREGO.REG.sh <- rbind(EMPREGO.REG.sh,EMPREGO.REG)


EMPREGO.REG <- EMPREGO.REG %>%
  select(-'Trimestre')

# Informality-------------------------------------------------------------------

# Position in occupation and employment category--------------------------------
# Variable: VD4009("Posição na ocupação e categoria do emprego")

pnad$variables$VD4009[
  pnad$variables$VD4009 %in% c("01", "03")] <- "0" #Formal employees ("carteira assinada") 

pnad$variables$VD4009[
  pnad$variables$VD4009 %in% c("02", "04")] <- "1" #Informal workers (no "carteira assinada") 

pnad$variables$VD4009[
  pnad$variables$VD4009 %in% c("05", "06", "07")] <- "2" # public sector

pnad$variables$VD4009[
  pnad$variables$VD4009 == "08" & 
    pnad$variables$VD4012 == "1"] <- "3"  # formal employer (contributes)

pnad$variables$VD4009[
  pnad$variables$VD4009 == "08" & 
    pnad$variables$VD4012 == "2"] <- "8"  # Informal employers (no contributions)

pnad$variables$VD4009[
  pnad$variables$VD4009 == "09" & 
    pnad$variables$VD4012 == "1"] <- "4"  # Formal self-employed (contributes)

pnad$variables$VD4009[
  pnad$variables$VD4009 == "09" & 
    pnad$variables$VD4012 == "2"] <- "5"  # Informal self-employed (no contributions)

pnad$variables$VD4009[
  pnad$variables$VD4009 == "10" & 
    pnad$variables$VD4012 == "1"] <- "6"  # Formal houseworker (contributes)

pnad$variables$VD4009[
  pnad$variables$VD4009 == "10" & 
    pnad$variables$VD4012 == "2"] <- "7"  # Informal houseworkers (no contributions)


# Checking for unique values----------------------------------------------------

unique(pnad$variables$VD4009)

# Function should return the following values: 0,1,2,3,4,5,6,7,8,NA

# Checking totals---------------------------------------------------------------

totalCategorias <- svytotal(x=~VD4009,
                            design = pnad,
                            na.rm=T)

print(totalCategorias)

sum(as.data.frame(totalCategorias)$total)

# Total: 100.202.616 (Employed population in the 1st quarter 2024)

# Informality (VD4009) ---------------------------------------------------------

#Calculating proportions--------------------------------------------------------

propCategorias <- svymean(x=~VD4009, 
                          design = pnad, 
                          na.rm = T) # Dropping NA 

print(propCategorias)

# Median informality (all groups)-----------------------------------------------

informalidadeMedia <- sum(propCategorias[c("VD40091", "VD40095", "VD40097", "VD40098")])

# It [informalidadeMedia] considers the following categories: 
#VD40091: Informal workers (no "carteira assinada")
#VD40095: Informal self-employed (no contributions)
#VD40097: Informal houseworkers (no contributions)
#VD40098: Informal employers (no contributions)

#Creating data frame for variable informalidadeMedia---------------------------

informalidadeMedia <- data.frame(Sum_of_Proportions = informalidadeMedia)

# Renaming columns--------------------------------------------------------------

colnames(informalidadeMedia)[colnames(informalidadeMedia) == "Sum_of_Proportions"] ="Taxa média de informalidade"

# Adding column with year and quarter to the data frame-------------------------

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

#Historic series----------------------------------------------------------------
#informalidadeMedia.sh <- rbind(informalidadeMedia.sh,informalidadeMedia)

# Not applicable. New methodology

# Informality by situation of occupation, gender and race-----------------------

informal.RG <- svyby(formula = ~ VD4009, # position in occupation, sorted by contributions as well
                     by = ~ interaction(V2010, V2007), # race (V2010) and gender (V2007)
                     design = pnad, 
                     svymean, # Calculating median value
                     na.rm = T) # NA's are not considered 

row.names(informal.RG) <- categorias

informal.RG <- informal.RG %>%
  select(-'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9',
         -'interaction(V2010, V2007)') %>%
  rename('carteira assinada'=VD40090, 'sem carteira assinada'=VD40091,
         'setor público'=VD40092,'empregadores formais'=VD40093,
         'autônomo contribuinte'=VD40094,'autônomo informal'=VD40095,
         'trabalhador familiar contribuinte'=VD40096,
         'trabalhador familiar informal'=VD40097, 
         'empregadores informais'=VD40098) 

# Adding column with year and quarter to the data frame-------------------------

informal.RG$Ano <- i
informal.RG$Quarter <- q
informal.RG$Day <- 01
informal.RG$Month <- ifelse(informal.RG$Quarter==1,1,
                            ifelse(informal.RG$Quarter==2,4,
                                   ifelse(informal.RG$Quarter==3,7,10)))
informal.RG$Trimestre<-as.yearqtr(with(informal.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
informal.RG$Trimestre<- as.character(informal.RG$Trimestre)

informal.RG <- informal.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')


# Historic series---------------------------------------------------------------

#informal.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#
#informal.RG.sh <- rbind(informal.RG.sh,informal.RG)
#
#informal.RG <- informal.RG %>%
#  select(-'Trimestre')

# Not applicable, since we are using this methodology for the first time

# Aggregating data by gender and race into two groups ((in)formal)--------------

agreInformal.RG <- informal.RG %>%
  mutate(categorias = rownames(informal.RG)) %>%
  rowwise() %>%
  mutate(
    FormalidadeMedia.RG = sum(c_across(c('carteira assinada', 'setor público', 
                                         'autônomo contribuinte', 'trabalhador familiar contribuinte',
                                         'empregadores formais')), 
                              na.rm = TRUE),
    InformalidadeMedia.RG = sum(c_across(c('sem carteira assinada', 
                                           'autônomo informal', 'trabalhador familiar informal',
                                           'empregadores informais')), 
                                na.rm = TRUE)
  ) %>%
  select(categorias, FormalidadeMedia.RG, InformalidadeMedia.RG) %>%
  ungroup()

agreInformal.RG <- agreInformal.RG %>%
  column_to_rownames(var = "categorias")

# The function aggregates the data into two columns, keeping gender/race
# identification in the rows

# Adding column with year and quarter to the data frame-------------------------

agreInformal.RG$Ano <- i
agreInformal.RG$Quarter <- q
agreInformal.RG$Day <- 01
agreInformal.RG$Month <- ifelse(agreInformal.RG$Quarter==1,1,
                                ifelse(agreInformal.RG$Quarter==2,4,
                                       ifelse(agreInformal.RG$Quarter==3,7,10)))
agreInformal.RG$Trimestre<-as.yearqtr(with(agreInformal.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
agreInformal.RG$Trimestre<- as.character(agreInformal.RG$Trimestre)

agreInformal.RG <- agreInformal.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

# To-do: Include code chunk to historic series----------------------------------

# Informality by group and region-----------------------------------------------

informal.RG.REG <- svyby(formula = ~ VD4009, # situation of occupation
                         by = ~ interaction(V2010, V2007, UF), #Race, Gender, UF
                         design = pnad, 
                         svymean, # Median
                         na.rm = T) # drop NA's 

row.names(informal.RG.REG) <- categoriasregiao

informal.RG.REG <- informal.RG.REG %>%
  select(-'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9',
         -'interaction(V2010, V2007, UF)') %>%
  rename('carteira assinada'=VD40090, 'sem carteira assinada'=VD40091,
         'setor público'=VD40092,'empregadores formais'=VD40093,
         'autônomo contribuinte'=VD40094,'autônomo informal'=VD40095,
         'trabalhador familiar contribuinte'=VD40096,
         'trabalhador familiar informal'=VD40097, 
         'empregadores informais'=VD40098) 

# Adding column with year and quarter to the data frame-------------------------

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


# Historic series---------------------------------------------------------------

#informal.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
#                                   "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
#                                   "Homem Branco Nordeste","Homem Negro Nordeste",
#                                   "Mulher Branca Nordeste", "Mulher Negra Nordeste",
#                                   "Homem Branco Norte", "Homem Negro Norte",
#                                   "Mulher Branca Norte", "Mulher Negra Norte",
#                                   "Homem Branco Sudeste", "Homem Negro Sudeste",
#                                   "Mulher Branca Sudeste", "Mulher Negra Sudeste",
#                                   "Homem Branco Sul", "Homem Negro Sul",
#                                   "Mulher Branca Sul", "Mulher Negra Sul")
#
#informal.RG.REG.sh <- rbind(informal.RG.REG.sh,informal.RG.REG)
#
#informal.RG.REG <- informal.RG.REG %>%
#  select(-'Trimestre')

# Aggregating the data by gender, race and region-------------------------------

agreInformal.RG.REG <- informal.RG.REG %>%
  mutate(categorias = rownames(informal.RG.REG)) %>%
  rowwise() %>%
  mutate(
    Formalidade.RG.REG = sum(c_across(c('carteira assinada', 'setor público', 
                                        'autônomo contribuinte', 'trabalhador familiar contribuinte',
                                        'empregadores formais')), 
                             na.rm = TRUE),
    Informalidade.RG.REG = sum(c_across(c('sem carteira assinada', 
                                          'autônomo informal', 'trabalhador familiar informal',
                                          'empregadores informais')), 
                               na.rm = TRUE)
  ) %>%
  select(categorias, Formalidade.RG.REG, Informalidade.RG.REG) %>%
  ungroup()

agreInformal.RG.REG <- agreInformal.RG.REG %>%
  column_to_rownames(var = "categorias")

# Adding column with year and quarter to the data frame-------------------------

agreInformal.RG.REG$Ano <- i
agreInformal.RG.REG$Quarter <- q
agreInformal.RG.REG$Day <- 01
agreInformal.RG.REG$Month <- ifelse(agreInformal.RG.REG$Quarter==1,1,
                                    ifelse(agreInformal.RG.REG$Quarter==2,4,
                                           ifelse(agreInformal.RG.REG$Quarter==3,7,10)))
agreInformal.RG.REG$Trimestre<-as.yearqtr(with(agreInformal.RG.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
agreInformal.RG.REG$Trimestre<- as.character(agreInformal.RG.REG$Trimestre)

agreInformal.RG.REG <- agreInformal.RG.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')


# To-do: Include code chunk to historic series----------------------------------


# Informality only by region----------------------------------------------------

informal.REG <- svyby(formula = ~ VD4009, # situation of occupation
                      by = ~ interaction(UF), #UF
                      design = pnad, 
                      svymean, # Median
                      na.rm = T) # Drop NA's 


row.names(informal.REG) <- regiaobrasil

informal.REG <- informal.REG %>%
  select(-'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8',-'se9',
         -'interaction(UF)')%>%
  rename('carteira assinada'=VD40090, 'sem carteira assinada'=VD40091,
         'setor público'=VD40092,'empregadores formais'=VD40093,
         'autônomo contribuinte'=VD40094,'autônomo informal'=VD40095,
         'trabalhador familiar contribuinte'=VD40096,
         'trabalhador familiar informal'=VD40097, 
         'empregadores informais'=VD40098) 

# Adding column with year and quarter to the data frame-------------------------

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

# Historic series---------------------------------------------------------------

#informal.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
#
#informal.REG.sh <- rbind(informal.REG.sh,informal.REG)
#
#informal.REG <- informal.REG %>%
#  select(-'Trimestre')

#Not applicable. New methodology

# Aggregating data--------------------------------------------------------------

agreInformal.REG <- informal.REG %>%
  mutate(regiao = rownames(informal.REG)) %>%
  rowwise() %>%  # Apply functions row-wise
  mutate(
    Formalidade.REG = sum(c_across(c('carteira assinada', 'setor público', 
                                     'autônomo contribuinte', 'trabalhador familiar contribuinte',
                                     'empregadores formais')), 
                          na.rm = TRUE),
    Informalidade.REG = sum(c_across(c('sem carteira assinada', 
                                       'autônomo informal', 'trabalhador familiar informal',
                                       'empregadores informais')), 
                            na.rm = TRUE)
  ) %>%
  select(regiao, Formalidade.REG, Informalidade.REG) %>%  # Keep only relevant columns
  ungroup()  # Remove rowwise grouping

agreInformal.REG <- agreInformal.REG %>%
  column_to_rownames(var = "regiao")

# Adding column with year and quarter to the data frame-------------------------

agreInformal.REG$Ano <- i
agreInformal.REG$Quarter <- q
agreInformal.REG$Day <- 01
agreInformal.REG$Month <- ifelse(agreInformal.REG$Quarter==1,1,
                                 ifelse(agreInformal.REG$Quarter==2,4,
                                        ifelse(agreInformal.REG$Quarter==3,7,10)))
agreInformal.REG$Trimestre<-as.yearqtr(with(agreInformal.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
agreInformal.REG$Trimestre<- as.character(agreInformal.REG$Trimestre)

agreInformal.REG <- agreInformal.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')


# Previous methodology to calculate informality---------------------------------
#### Carteira assinada
#
## Taxa de informalidade media  (VD4009)
#
#informalidadeMedia <- svymean(x=~VD4009, 
#                              design = pnad, 
#                              na.rm = T) # Remover valores faltantes 
#
#
#informalidadeMedia <- as.data.frame(informalidadeMedia[2])
#colnames(informalidadeMedia)[colnames(informalidadeMedia) == "informalidadeMedia[2]"] ="Taxa média de informalidade"
#
#informalidadeMedia$Ano <- i
#informalidadeMedia$Quarter <- q
#informalidadeMedia$Day <- 01
#informalidadeMedia$Month <- ifelse(informalidadeMedia$Quarter==1,1,
#                                   ifelse(informalidadeMedia$Quarter==2,4,
#                                          ifelse(informalidadeMedia$Quarter==3,7,10)))
#informalidadeMedia$Trimestre<-as.yearqtr(with(informalidadeMedia,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#informalidadeMedia$Trimestre<- as.character(informalidadeMedia$Trimestre)
#
#informalidadeMedia <- informalidadeMedia %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#informalidadeMedia.sh<- rbind(informalidadeMedia.sh,informalidadeMedia)
#
## TAXA DE INFORMALIDADE POR GRUPO 
#
#informal.RG<- svyby(formula = ~ VD4009, # contribui para o INSS
#                    by = ~ interaction(V2010, V2007), #Raca, Genero
#                    design = pnad, 
#                    svymean, # Fun??o para gerar estat?stica de interesse
#                    na.rm = T) # Remover valores faltantes 
#
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
#
#informal.RG <- informal.RG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#informal.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")
#
#
#informal.RG.sh <- rbind(informal.RG.sh,informal.RG)
#
#informal.RG <- informal.RG %>%
#  select(-'Trimestre')
#
## TAXA DE INFORMALIDADE por grupo e por regiao
#
#informal.RG.REG <- svyby(formula = ~ VD4009, # contribui para o INSS
#                         by = ~ interaction(V2010, V2007, UF), #Raca, Genero
#                         design = pnad, 
#                         svymean, # Fun??o para gerar estat?stica de interesse
#                         na.rm = T) # Remover valores faltantes 
#
#row.names(informal.RG.REG) <- categoriasregiao
#informal.RG.REG <- informal.RG.REG %>%
#  select(-'se1', -'interaction(V2010, V2007, UF)', -'se2')%>%
#  rename('contribuinte INSS'=VD40121, 'não contribuinte INSS'=VD40122) 
#
#informal.RG.REG$Ano <- i
#informal.RG.REG$Quarter <- q
#informal.RG.REG$Day <- 01
#informal.RG.REG$Month <- ifelse(informal.RG.REG$Quarter==1,1,
#                                ifelse(informal.RG.REG$Quarter==2,4,
#                                       ifelse(informal.RG.REG$Quarter==3,7,10)))
#informal.RG.REG$Trimestre<-as.yearqtr(with(informal.RG.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#informal.RG.REG$Trimestre<- as.character(informal.RG.REG$Trimestre)
#
#informal.RG.REG <- informal.RG.REG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#informal.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
#                                   "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
#                                   "Homem Branco Nordeste","Homem Negro Nordeste",
#                                   "Mulher Branca Nordeste", "Mulher Negra Nordeste",
#                                   "Homem Branco Norte", "Homem Negro Norte",
#                                   "Mulher Branca Norte", "Mulher Negra Norte",
#                                   "Homem Branco Sudeste", "Homem Negro Sudeste",
#                                   "Mulher Branca Sudeste", "Mulher Negra Sudeste",
#                                   "Homem Branco Sul", "Homem Negro Sul",
#                                   "Mulher Branca Sul", "Mulher Negra Sul")
#
#informal.RG.REG.sh <- rbind(informal.RG.REG.sh,informal.RG.REG)
#
#informal.RG.REG <- informal.RG.REG %>%
#  select(-'Trimestre')
#
## TAXA DE INFORMALIDADE somente por regiao
#
#informal.REG<- svyby(formula = ~ VD4012, # contribui para o INSS
#                     by = ~ interaction(UF), #Raca, Genero
#                     design = pnad, 
#                     svymean, # Fun??o para gerar estat?stica de interesse
#                     na.rm = T) # Remover valores faltantes 
#
#row.names(informal.REG) <- regiaobrasil
#informal.REG <- informal.REG %>%
#  select(-'se1', -'interaction(UF)', -'se2')%>%
#  rename('contribuinte INSS'=VD40121, 'não contribuinte INSS'=VD40122) 
#
#informal.REG$Ano <- i
#informal.REG$Quarter <- q
#informal.REG$Day <- 01
#informal.REG$Month <- ifelse(informal.REG$Quarter==1,1,
#                             ifelse(informal.REG$Quarter==2,4,
#                                    ifelse(informal.REG$Quarter==3,7,10)))
#informal.REG$Trimestre<-as.yearqtr(with(informal.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
#informal.REG$Trimestre<- as.character(informal.REG$Trimestre)
#
#informal.REG <- informal.REG %>%
#  select(-'Ano', -'Quarter', -'Day',-'Month')
#
#informal.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")
#
#informal.REG.sh <- rbind(informal.REG.sh,informal.REG)
#
#informal.REG <- informal.REG %>%
#  select(-'Trimestre')

# Underoccupation by race and gender--------------------------------------------
# Variable: VD4004A ("Subocupação por insuficiência de horas")

SUBOCUPHORAS.RG <- svyby(formula = ~ VD4004A, # "Subocupação por insuficiência de horas"
                         by = ~ interaction(V2010, V2007), #Race (V2010) and gender (V2007)
                         design = pnad, 
                         svymean, # Generates statistic of interest
                         na.rm = T) # Dropping NA 

#Naming rows
row.names(SUBOCUPHORAS.RG ) <- categorias

# Removing extra columns
SUBOCUPHORAS.RG <- SUBOCUPHORAS.RG %>%
  select(-'se1', -'interaction(V2010, V2007)', -'se2', -VD4004A0)%>%
  rename('Subocupado' = VD4004A1) 

SUBOCUPHORAS.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

# Underoccupation by region--------r--------------------------------------------
# Variable: VD4004A ("Subocupação por insuficiência de horas")

SUBOCUPHORAS.REG <- svyby(formula = ~ VD4004A, # "Subocupação por insuficiência de horas"
                          by = ~ interaction(UF), 
                          design = pnad, 
                          svymean, # Generates statistic of interest
                          na.rm = T) # Dropping NA

# Naming rows
row.names(SUBOCUPHORAS.REG ) <- regiaobrasil

# Removing extra columns
SUBOCUPHORAS.REG <- SUBOCUPHORAS.REG %>%
  select(-'se1', -'interaction(UF)', -'se2', -VD4004A0)%>%
  rename('Subocupado' = VD4004A1) 

SUBOCUPHORAS.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

# Work by sector----------------------------------------------------------------

# Main job by sector by gender and race-----------------------------------------
# Variable: VD4010 ("Grupamentos de atividade principal")

trabalhosetor.RG <- svyby(formula = ~ VD4010, 
                         by = ~ interaction(V2010, V2007), #Race (V2010) and gender (V2007)
                         design = pnad, 
                         svymean, # Generates statistic of interest
                         na.rm = T) # Dropping NA

# Naming rows
row.names(trabalhosetor.RG) <- categorias                               

trabalhosetor.RG <- trabalhosetor.RG %>%
  select(-'interaction(V2010, V2007)', -'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9', -'se10', -'se11', -'se12')%>%
  rename('agropecuária'=VD401001, 'indústria'=VD401002, 'construção'=VD401003, 'comércio'=VD401004, 'transporte'=VD401005, 'alimentação'=VD401006, 'informação'=VD401007, 'administraçãopública'=VD401008, 'educação'=VD401009, 'outrosserviços'=VD401010, 'serviçosdomésticos'=VD401011, 'maldefinido'=VD401012) 

# Adding column with year and quarter to the data frame-------------------------
trabalhosetor.RG$Ano <- i
trabalhosetor.RG$Quarter <- q
trabalhosetor.RG$Day <- 01
trabalhosetor.RG$Month <- ifelse(trabalhosetor.RG$Quarter==1,1,
                                 ifelse(trabalhosetor.RG$Quarter==2,4,
                                        ifelse(trabalhosetor.RG$Quarter==3,7,10)))
# Creating column "Trimestre"
trabalhosetor.RG$Trimestre<-as.yearqtr(with(trabalhosetor.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
trabalhosetor.RG$Trimestre<- as.character(trabalhosetor.RG$Trimestre)

# Binding to historic series
trabalhosetor.RG <- trabalhosetor.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

trabalhosetor.RG["Categoria"] <- c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

trabalhosetor.RG <- trabalhosetor.RG %>%
  select(Categoria,Trimestre,agropecuária,indústria,construção,comércio,transporte,alimentação,
         informação,administraçãopública,educação,outrosserviços,serviçosdomésticos,maldefinido)

trabalhosetor.RG <- trabalhosetor.RG %>%
  rename('Agropecuária'='agropecuária', 'Indústria'='indústria', 'Construção'='construção', 'Comércio'='comércio', 'Transporte'='transporte', 'Alimentação'='alimentação', 'Informação'='informação', 'Administração Pública'='administraçãopública', 'Educação'='educação', 'Outros serviços'='outrosserviços', 'Serviços domésticos'='serviçosdomésticos', 'Mal definido'='maldefinido') 

trabalhosetor.RG.sh <- rbind(trabalhosetor.RG.sh,trabalhosetor.RG)

# Main job by sector by gender, race and region---------------------------------
# Variable: VD4010 ("Grupamentos de atividade principal")

trabalhosetor.RG.REG <- svyby(formula = ~ VD4010, 
                             by = ~ interaction(V2010, V2007, UF), #Race (V2010) and gender (V2007)
                             design = pnad, 
                             svymean, # Generates statistic of interest
                             na.rm = T) # Dropping NA

# Naming rows
row.names(trabalhosetor.RG.REG) <- categoriasregiao                               

# Removing columns
trabalhosetor.RG.REG <- trabalhosetor.RG.REG %>%
  select(-'interaction(V2010, V2007, UF)', -'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9', -'se10', -'se11', -'se12')%>%
  rename('Agropecuária'=VD401001, 'Indústria'=VD401002, 'Construção'=VD401003, 'Comércio'=VD401004, 'Transporte'=VD401005, 'Alimentação'=VD401006, 'Informação'=VD401007, 'Administração Pública'=VD401008, 'Educação'=VD401009, 'Outros serviços'=VD401010, 'Serviços domésticos'=VD401011, 'Mal definido'=VD401012) 

trabalhosetor.RG.REG$Categoria_regiao=c("Homem Branco Centro-Oeste", "Homem Negro Centro-Oeste",
                                        "Mulher Branca Centro-Oeste", "Mulher Negra Centro-Oeste",
                                        "Homem Branco Nordeste","Homem Negro Nordeste",
                                        "Mulher Branca Nordeste", "Mulher Negra Nordeste",
                                        "Homem Branco Norte", "Homem Negro Norte",
                                        "Mulher Branca Norte", "Mulher Negra Norte",
                                        "Homem Branco Sudeste", "Homem Negro Sudeste",
                                        "Mulher Branca Sudeste", "Mulher Negra Sudeste",
                                        "Homem Branco Sul", "Homem Negro Sul",
                                        "Mulher Branca Sul", "Mulher Negra Sul")

# Main job by sector by region--------------------------------------------------
# Variable: VD4010 ("Grupamentos de atividade principal")


trabalhosetor.REG<- svyby(formula = ~ VD4010, 
                          by = ~ interaction(UF), 
                          design = pnad, 
                          svymean, # Generates statistic of interest
                          na.rm = T) # Dropping NA

# Naming rows
row.names(trabalhosetor.REG) <- regiaobrasil                               

trabalhosetor.REG <- trabalhosetor.REG %>%
  select(-'interaction(UF)', -'se1', -'se2', -'se3', -'se4', -'se5', -'se6', -'se7', -'se8', -'se9', -'se10', -'se11', -'se12')%>%
  rename('agropecuária'=VD401001, 'indústria'=VD401002, 'construção'=VD401003, 'comércio'=VD401004, 'transporte'=VD401005, 'alimentação'=VD401006, 'informação'=VD401007, 'administraçãopública'=VD401008, 'educação'=VD401009, 'outrosserviços'=VD401010, 'serviçosdomésticos'=VD401011, 'maldefinido'=VD401012) 

# Adding column with year and quarter to the data frame-------------------------
trabalhosetor.REG$Ano <- i
trabalhosetor.REG$Quarter <- q
trabalhosetor.REG$Day <- 01
trabalhosetor.REG$Month <- ifelse(trabalhosetor.REG$Quarter==1,1,
                                  ifelse(trabalhosetor.REG$Quarter==2,4,
                                         ifelse(trabalhosetor.REG$Quarter==3,7,10)))

# Creating column "Trimestre"
trabalhosetor.REG$Trimestre<-as.yearqtr(with(trabalhosetor.REG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
trabalhosetor.REG$Trimestre<- as.character(trabalhosetor.REG$Trimestre)

# Remoing columns
trabalhosetor.REG <- trabalhosetor.REG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

trabalhosetor.REG$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

trabalhosetor.REG <- trabalhosetor.REG %>%
  select(Regiao,Trimestre,agropecuária,indústria,construção,comércio,transporte,alimentação,
         informação,administraçãopública,educação,outrosserviços,serviçosdomésticos,maldefinido)

trabalhosetor.REG <- trabalhosetor.REG %>%
  rename('Agropecuária'='agropecuária', 'Indústria'='indústria', 'Construção'='construção', 'Comércio'='comércio', 'Transporte'='transporte', 'Alimentação'='alimentação', 'Informação'='informação', 'Administração Pública'='administraçãopública', 'Educação'='educação', 'Outros serviços'='outrosserviços', 'Serviços domésticos'='serviçosdomésticos', 'Mal definido'='maldefinido') 

# Binding to historic series
trabalhosetor.REG.sh <- rbind(trabalhosetor.REG.sh,trabalhosetor.REG)

# Education level of the labor force--------------------------------------------
# "Escolaridade da força de trabalho"

# subset occupied workers
pnad.ocup <- subset(pnad, pnad$variables$VD4001 %in% c("1"))

# Education level by gender and race--------------------------------------------
# Variable: V3009A ("Curso frequentado mais elevado")

ESCOLARIDADE.RG <- svyby(formula = ~ V3009A, #"Curso mais elevado"
                         by = ~ interaction(V2010, V2007), #Race (V2010) and gender (V2007)
                         design = pnad.ocup, 
                         svymean,   
                         na.rm = T) # Dropping NA 

# Naming rows
row.names(ESCOLARIDADE.RG) <- categorias

# Removing extra columns
ESCOLARIDADE.RG <- ESCOLARIDADE.RG %>%
  select(-'interaction(V2010, V2007)',-'se1',-'se2',-'se3',-'se4',-'se5',-'se6')%>%
  rename('Pré-fundamental'=V3009A1,'Ensino fundamental'=V3009A2,'Ensino médio'=V3009A3, 'Ensino superior'= V3009A4,'Especializacao/pós-graduação'= V3009A5, 'Mestrado/doutorado'= V3009A6)

# Education level by gender, race and region------------------------------------
# Variable: V3009A ("Curso frequentado mais elevado")

ESCOLARIDADE.RG.REG <- svyby(formula = ~ V3009A, # "Curso mais elevado"
                             by = ~ interaction(V2010, V2007, UF), #Race (V2010), gender (V2007) and region
                             design = pnad.ocup, 
                             svymean, # Generates statistic of interest
                             na.rm = T) # Dropping NA 

# Naming rows
row.names(ESCOLARIDADE.RG.REG) <- categoriasregiao

# Removing columns
ESCOLARIDADE.RG.REG <- ESCOLARIDADE.RG.REG %>%
  select(-'interaction(V2010, V2007, UF)',-'se1',-'se2',-'se3',-'se4',-'se5',-'se6')%>%
  rename('Pré-fundamental'=V3009A1,'Ensino fundamental'=V3009A2,'Ensino médio'=V3009A3, 'Ensino superior'= V3009A4,'Especializacao/pós-graduação'= V3009A5, 'Mestrado/doutorado'= V3009A6)

# Education level by region-----------------------------------------------------
# Variable: V3009A ("Curso frequentado mais elevado") 

ESCOLARIDADE.REG <- svyby(formula = ~ V3009A, # "Curso mais elevado"
                          by = ~ interaction(UF), 
                          design = pnad.ocup, 
                          svymean, # Generates statistic of interest
                          na.rm = T) # Dropping NA

# Naming rows
row.names(ESCOLARIDADE.REG) <- regiaobrasil

# Removing columns
ESCOLARIDADE.REG <- ESCOLARIDADE.REG %>%
  select(-'interaction(UF)',-'se1',-'se2',-'se3',-'se4',-'se5',-'se6')%>%
  rename('Pré-fundamental'=V3009A1,'Ensino fundamental'=V3009A2,'Ensino médio'=V3009A3, 'Ensino superior'= V3009A4,'Especializacao/pós-graduação'= V3009A5, 'Mestrado/doutorado'= V3009A6)

# Income -----------------------------------------------------------------------
# Average Actual (rendimento efetivo) and Usual ("rendimento habitual") Income--

# Average usual income----------------------------------------------------------
# Variable: VD4017 ("Rendimento mensal efetivo")

RendaEfetivaMedia <- svymean(x=~EfetivaDef22, 
                             design = pnad, 
                             na.rm = T) # Dropping NA 

# Data frame
RendaEfetivaMedia <- as.data.frame(RendaEfetivaMedia[1])

colnames(RendaEfetivaMedia)[colnames(RendaEfetivaMedia) == "RendaEfetivaMedia[1]"] ="RendaEfetivaMedia"

# Adding column with year and quarter to the data frame-------------------------
RendaEfetivaMedia$Ano <- i
RendaEfetivaMedia$Quarter <- q
RendaEfetivaMedia$Day <- 01
RendaEfetivaMedia$Month <- ifelse(RendaEfetivaMedia$Quarter==1,1,
                                  ifelse(RendaEfetivaMedia$Quarter==2,4,
                                         ifelse(RendaEfetivaMedia$Quarter==3,7,10)))
RendaEfetivaMedia$Trimestre<-as.yearqtr(with(RendaEfetivaMedia,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
RendaEfetivaMedia$Trimestre<- as.character(RendaEfetivaMedia$Trimestre)

RendaEfetivaMedia <- RendaEfetivaMedia %>%
  select('Trimestre','RendaEfetivaMedia',-'Ano', -'Quarter', -'Day',-'Month')

RendaEfetivaMedia.sh<- rbind(RendaEfetivaMedia.sh,RendaEfetivaMedia)

# Actual income by gender and race---------------------------------------------- 
# Variable: VD4017 ("Rendimento mensal efetivo")

RendaEfet.RG <- svyby(formula = ~ EfetivaDef22, # Income
                      by = ~ interaction(V2010, V2007), #Race (V2010) and gender (V2007)
                      design = pnad, 
                      svymean, # Generates statistic of interest
                      na.rm = T) # Dropping NA

# Naming rows
row.names(RendaEfet.RG) <-categorias

# Removing columns
RendaEfet.RG <- RendaEfet.RG %>%
  select(-'interaction(V2010, V2007)', -'se')%>%
  rename('Renda_Efetiva_RG' = EfetivaDef22)

# Adding column with year and quarter to the data frame-------------------------
RendaEfet.RG$Ano <- i
RendaEfet.RG$Quarter <- q
RendaEfet.RG$Day <- 01
RendaEfet.RG$Month <- ifelse(RendaEfet.RG$Quarter==1,1,
                             ifelse(RendaEfet.RG$Quarter==2,4,
                                    ifelse(RendaEfet.RG$Quarter==3,7,10)))
RendaEfet.RG$Trimestre<-as.yearqtr(with(RendaEfet.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
RendaEfet.RG$Trimestre<- as.character(RendaEfet.RG$Trimestre)

RendaEfet.RG <- RendaEfet.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

RendaEfet.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

RendaEfet.RG <- RendaEfet.RG %>%
  select('Categoria','Trimestre', 'Renda_Efetiva_RG')

RendaEfet.RG.sh <- rbind(RendaEfet.RG.sh,RendaEfet.RG)

# Average usual income---------------------------------------------------------- 
# Variable: VD4016 ("Rendimento mensal habitual")


RendaHabitMedia <- svymean(x=~HabitualDef22, 
                           design = pnad, 
                           na.rm = T) # Dropping NA 

# Data frame
RendaHabitMedia <- as.data.frame(RendaHabitMedia[1])

# Naming columns
colnames(RendaHabitMedia)[colnames(RendaHabitMedia) == "RendaHabitMedia[1]"] ="RendaHabitMedia"

# Adding column with year and quarter to the data frame-------------------------
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

RendaHabitMedia.sh <- rbind(RendaHabitMedia.sh,RendaHabitMedia)

# Usual income by gender and race 
# Variable: VD4016 ("Rendimento mensal habitual")

RendaHabit.RG <- svyby(formula = ~ HabitualDef22, # Usual income ("Renda habitual")
                       by = ~ interaction(V2010, V2007), #Race (V2010) and gender (V2007)
                       design = pnad, 
                       svymean, # Generates statistic of interest
                       na.rm = T) # Dropping NA 

# Naming rows
row.names(RendaHabit.RG ) <- categorias

# Selecting columns of interest
RendaHabit.RG <- RendaHabit.RG %>%
  select(-'interaction(V2010, V2007)', -'se')%>%
  rename('Renda_Habitual_RG' = HabitualDef22)

# Adding column with year and quarter to the data frame-------------------------
RendaHabit.RG$Ano <- i
RendaHabit.RG$Quarter <- q
RendaHabit.RG$Day <- 01
RendaHabit.RG$Month <- ifelse(RendaHabit.RG$Quarter==1,1,
                              ifelse(RendaHabit.RG$Quarter==2,4,
                                     ifelse(RendaHabit.RG$Quarter==3,7,10)))

# Adding column "Trimestre"
RendaHabit.RG$Trimestre<-as.yearqtr(with(RendaHabit.RG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
RendaHabit.RG$Trimestre<- as.character(RendaHabit.RG$Trimestre)

RendaHabit.RG <- RendaHabit.RG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

RendaHabit.RG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

RendaHabit.RG <- RendaHabit.RG %>%
  select("Categoria", "Trimestre", "Renda_Habitual_RG")

RendaHabit.RG.sh <- rbind(RendaHabit.RG.sh,RendaHabit.RG)

# Poverty-----------------------------------------------------------------------

# Poverty Coefficient-----------------------------------------------------------

#Pessoas_na_pobreza <- svymean(x=~Linha_pobreza,
#                              design = pnad, 
#                              na.rm = T) # Dropping NA
#
# Poverty lines-----------------------------------------------------------------
 
# As per defined by the Brazilian Institute of Geography and Statistics (IBGE) 
# Poverty: USD 6.85/day 
# Extreme poverty: USD 2.15/day

# Getting current exchange rate-------------------------------------------------
#
#install.packages("quantmod") # package quantmod gives the current exchange rate BRL X USD
#library(quantmod)
#
## Creating object
#exchange_rate <- getQuote("USDBRL=X")$Last
#
#print(exchange_rate)
#
## Creating objects for (extreme) poverty lines----------------------------------
#
#poverty_line_usd <- 6.85 # as per defined by IBGE
#extrem_pov_line_usd <- 2.15 # as per defined by IBGE
#
## (Extreme) poverty thresholds converted by nominal excahnge rate---------------
#
#poverty_line_brl <- poverty_line_usd*exchange_rate*30
#extrem_pov_line_brl <- extrem_pov_line_usd*exchange_rate*30
#
#print(poverty_line_brl)
#print(extrem_pov_line_brl)
#
## Using ppp adjusted exchange rate 2.44 (World Bank, 2023)----------------------
#
#ppp_poverty_line_brl <- poverty_line_usd*2.44*30
#ppp_extrem_pov_line_brl <- extrem_pov_line_usd*2.44*30
#
#print(ppp_poverty_line_brl)
#print(ppp_extrem_pov_line_brl)
#
#
#
## Filtering PNADc data for individuals below (extreme) poverty thresholds
## Variable: VD4020 ("Rendimento mensal efetivo")
#
#below_poverty <- pnad$variables %>%
#  filter(VD4020 <= poverty_line_brl)
#
#below_extrem_poverty <- pnad$variables %>%
#  filter(VD4020 <= extrem_pov_line_brl)
#
#
## Create a survey design object
#pnad_design <- svydesign(id = ~UPA, strata = ~Estrato, weights = ~V1028, data = pnad$variables, nest = TRUE)
#
#
## Subset the design for those below the poverty line
#below_poverty_design <- subset(pnad_design, VD4020 <= poverty_line_brl)
#
## Calculate the total number of people below the poverty line
#num_below_poverty <- svytotal(~1, below_poverty_design)
#
## Print the result
#print(num_below_poverty)
#
#
## Create a survey design object
#pnad_design <- svydesign(id = ~UPA, strata = ~Estrato, weights = ~V1028, data = pnad$variables, nest = TRUE)
#
## Create a subset of the data for those below the poverty line
#below_poverty_data <- pnad$variables %>%
#  filter(VD4020 <= poverty_line_brl)
#
## Create a new survey design object for the subset data
#below_poverty_design <- svydesign(id = ~UPA, strata = ~Estrato, weights = ~V1028, data = below_poverty_data, nest = TRUE)
#
## Calculate the total number of people below the poverty line
#num_below_poverty <- svytotal(~1, below_poverty_design)
#
## Print the result
#print(num_below_poverty)
#
#
#
#Pessoas_na_pobreza <- svymean(x=~belo,
#                              design = pnad, 
#                              na.rm = T) # Dropping NA
#
#
#taxa_pobreza <- ((pnad$variables %>% 
#                    filter(VD4020 <= poverty_line_brl) %>% 
#                    pull(V1028) %>% 
#                    sum())/(pnad$variables %>% 
#                              pull(V1028) %>% 
#                              sum()))*100
#
#
#taxa_extrem_pobreza <- ((pnad$variables %>% 
#                    filter(VD4020 <= extrem_pov_line_brl) %>% 
#                    pull(V1028) %>% 
#                    sum())/(pnad$variables %>% 
#                              pull(V1028) %>% 
#                              sum()))*100
#
## Count the number of individuals below the poverty line
#num_below_poverty <- pnad$variables %>%
#  filter(VD4020 <= poverty_line_brl) %>%
#  nrow()
#
## Print the result
#print(num_below_poverty)
#
#Retirar pensionistas e empregados
 #Removing data related to pensioners, domestic employees, and their relatives
 #Variable: V2005 ("Condição no domicílio")
#
#
#
#
#
#
#pnad <- pnad %>% 
#  filter(!V2005 %in% c('Pensionista',
#                       'Empregado(a) doméstico(a)',
#                       'Parente do(a) empregado(a) doméstico(a)'))
#
## Aggregating data by household
#
#pnad_domicilio <- pnad %>% 
#  group_by(domicilio_id) %>% 
#  summarise(peso = max(peso), renda_total = sum(renda_total), 
#            renda_pbf = sum(renda_pbf), renda_total_sem_pbf=sum(renda_total_sem_pbf), 
#            pessoas = n()) %>% ungroup()
#
## Calculating Per-Capita Income
#
#pnad_domicilio$renda_total_sem_pbf_pcp <- 
#  pnad_domicilio$renda_total_sem_pbf/pnad_domicilio$pessoas
#
## Calculating Final Weight
#pnad_domicilio$renda_total_pcp <- 
#  pnad_domicilio$renda_total/pnad_domicilio$pessoas
#
#pnad_domicilio$peso_final <- 
#  pnad_domicilio$peso * pnad_domicilio$pessoas
#
#Calculating Poverty Rate
#
#
#
## Recalculating Poverty Rate with Excluded Income
#taxa_pobreza <- ((pnad_domicilio %>% 
#                    filter(renda_total_sem_pbf_pcp<=linha_pobreza) %>% 
#                    pull(peso_final) %>% sum())/(pnad_domicilio %>% 
#                                                   pull(peso_final) %>% sum()))*100


# Gini index--------------------------------------------------------------------

# Gini index by region----------------------------------------------------------

pnad <- pnad %>%
  convey::convey_prep() # used for the estimation of inequality measures

giniUF <- svyby(formula=~VD4020, 
                by=~UF, 
                design=pnad, 
                FUN=svygini, 
                na.rm=TRUE)

giniUF <- giniUF %>%
  select(-se.VD4020, -UF) %>%
  rename("gini" = VD4020)

# Adding column "Regiao"
giniUF$Regiao=c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul")

# Adding column with year and quarter to the data frame-------------------------
giniUF$Ano <- i
giniUF$Quarter <- q
giniUF$Day <- 01
giniUF$Month <- ifelse(giniUF$Quarter==1,1,
                       ifelse(giniUF$Quarter==2,4,
                              ifelse(giniUF$Quarter==3,7,10)))

# Adding column "Trimestre"
giniUF$Trimestre<-as.yearqtr(with(giniUF,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
giniUF$Trimestre<- as.character(giniUF$Trimestre)

giniUF <- giniUF %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

giniUF.sh <- rbind(giniUF.sh,giniUF)

giniUF <- giniUF %>%
  select(-'Trimestre')

#Gini index (Brazil)------------------------------------------------------------

#pnad <- pnad %>%
#convey::convey_prep()

giniBR <- svygini(formula=~VD4020, 
                  design=pnad, 
                  na.rm=TRUE)

giniBR <- data.frame(giniBR)

# Adding column with year and quarter to the data frame-------------------------
giniBR$Ano <- i
giniBR$Quarter <- q
giniBR$Day <- 01
giniBR$Month <- ifelse(giniBR$Quarter==1,1,
                       ifelse(giniBR$Quarter==2,4,
                              ifelse(giniBR$Quarter==3,7,10)))

# Adding column "Trimestre"
giniBR$Trimestre<-as.yearqtr(with(giniBR,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
giniBR$Trimestre<- as.character(giniBR$Trimestre)

giniBR <- giniBR %>%
  select(-'Ano', -'Quarter', -'Day',-'Month',-'SE')

colnames(giniBR.sh)[colnames(giniBR.sh) == "Gini Brasil"] ="gini"

giniBR.sh <- rbind(giniBR.sh,giniBR)

giniBR <- giniBR %>%
  select(-'Trimestre')

# Gini index by gender and race-------------------------------------------------

#pnad <- pnad %>%
#convey::convey_prep()

giniRG <- svyby(formula=~VD4020, 
                by=~ interaction(V2010, V2007), 
                design=pnad, 
                FUN=svygini, 
                na.rm=TRUE)

# Naming rows
row.names(giniRG) <- categorias 

giniRG <- giniRG %>%
  select(-'interaction(V2010, V2007)', -'se.VD4020')%>%
  rename('gini' = VD4020)

giniRG$Categoria=c("Homem Branco", "Homem Negro", "Mulher Branca", "Mulher Negra")

# Adding column with year and quarter to the data frame-------------------------
giniRG$Ano <- i
giniRG$Quarter <- q
giniRG$Day <- 01
giniRG$Month <- ifelse(giniRG$Quarter==1,1,
                       ifelse(giniRG$Quarter==2,4,
                              ifelse(giniRG$Quarter==3,7,10)))
giniRG$Trimestre<-as.yearqtr(with(giniRG,paste(Ano,Month,Day,sep="-")),"%Y-%m-%d")
giniRG$Trimestre<- as.character(giniRG$Trimestre)

giniRG <- giniRG %>%
  select(-'Ano', -'Quarter', -'Day',-'Month')

colnames(giniRG.sh)[colnames(giniRG.sh) == "Gini Brasil"] ="gini"

# Binding to historic series
giniRG.sh <- rbind(giniRG.sh,giniRG)

giniRG <- giniRG %>%
  select(-'Trimestre')

# Income percentiles------------------------------------------------------------

# apropriação da renda
# obtendo cada um dos decis de renda e o último percentil (ou seja, o valor equivalente ao 1% mais rico da população)

pnad$variables$EfetivaDef23todos <- pnad$variables$Efetivo * pnad$variables$VD4020
quanti2303 <- svyquantile(x = ~ EfetivaDef23todos,
                          design = pnad,
                          quantiles = c(seq(from = .1, to = .9, by = .1), .99),
                          na.rm = T)

quanti2303

#obtendo renda total

rentot2303 <- svytotal(x = ~ EfetivaDef23todos,
                       design = pnad,
                       na.rm = T)

rentot2303

sum(pnad$variables$EfetivaDef23todos, na.rm = T) - rentot2303[1]

#apropriação da renda


quantis <- c(0, quanti2303$EfetivaDef23todos[,1], max(pnad$variables$EfetivaDef23todos, na.rm = T))

names(quantis) <- c("0", rownames(quanti2303$EfetivaDef23todos), "Max")

for (quant in 2:length(quantis)) {
  
  rotulo <- paste(names(quantis)[c(quant - 1, quant)], collapse = " - ")
  
  pnad$variables$Quant[pnad$variables$EfetivaDef23todos >= quantis[quant - 1] &
                         pnad$variables$EfetivaDef23todos <= quantis[quant]] <- rotulo
  
}

pnad.Quant0 <- subset(pnad, Quant == "0 - 0.1")

unique(pnad.Quant0$prob[pnad.Quant0$variables$EfetivaDef23todos >= 17000])

cat <- setdiff(unique(pnad$variables$Quant), NA) #Obter os decis
cat
ordem_final <- c("0 - 0.1","0.1 - 0.2","0.2 - 0.3","0.3 - 0.4", 
                 "0.4 - 0.5", "0.5 - 0.6",  "0.6 - 0.7" , "0.7 - 0.8",
                 "0.8 - 0.9", "0.9 - 0.99", "0.99 - Max") #estabelecer vetor de ordem dos quantis
ordem_real <- c()

for (value in ordem_final){
  for (i in 1:length(cat)){
    if(cat[[i]]==value){
      ordem_real <- append(ordem_real, i)
    }
  }
}
cat <- cat[ordem_real] #Ordenar os decis automaticamente

totquant2303 <- c()

for (quant in cat) {
  
  total <- svytotal(x = ~ EfetivaDef23todos,
                    design = subset(pnad, Quant == quant),
                    na.rm = T)
  
  totquant2303 <- c(totquant2303, total)
  
}


names(totquant2303) <- c(paste(seq(0, .9, .1)), "0.99")

quantis_de_renda23_04 <- as.data.frame(totquant2303/rentot2303 * 100)
quantis_de_renda23_04 <- quantis_de_renda23_04 %>% 
  rename('Percentual da Renda Apropriada' = `totquant2303/rentot2303 * 100`)

percentis <- totquant2303/rentot2303 * 100

percentis <- as.data.frame(percentis)

colnames(percentis)[colnames(percentis) == "percentis"] ="% da renda apropriado"


## Curva de Lorenz
# Carrega o pacote ggplot2
library(ggplot2)

# Dados
x <- seq(0, 1, 0.1)
y <- c(cumsum(totquant2303[-11]/rentot2303), 1)

# Cria o gráfico com ggplot2
grafico <- ggplot(data.frame(x, y), aes(x, y)) +
  geom_line(color = "#45ff66", size=1.5) +
  labs(x = "Quantil", y = "% da Renda", title = "Curva de Lorenz") 

# Adiciona a linha de 45 graus
grafico + geom_abline(intercept = 0, slope = 1, color = "#eb52ff",size=0.7)
curva_de_lorenz <- grafico + geom_abline(intercept = 0, slope = 1, color = "#eb52ff",size=0.7)
ggsave("curva_de_lorenz_2023_03.png", plot = curva_de_lorenz)



# Saving data-------------------------------------------------------------------

# Removing specific objects
rm(pnad,pnad.Quant0,pnad.ocup)

# Saving RData------------------------------------------------------------------
# load("C:/Users/USER/Documents/PNAD/PNAD_dados_relatorio_04_2023.RData")
save.image("C:/Users/lauro/Desktop/PNADc/v3/PNAD_dados_relatorio_01_2024.RData")

# Removing all objects but historic series

rm (EMPREGO.REG, EMPREGO.RG, EMPREGO.RG.REG, giniBR, giniRG, giniUF, informal.REG, 
    informal.RG, informal.RG.REG, informalidadeMedia, PEA.REG, PEA.RG, PEA.RG.REG,
    RendaEfet.RG, RendaEfetivaMedia, RendaHabit.RG, trabalhosetor.REG, trabalhosetor.RG, 
    trabalhosetor.RG.REG, Tx_desemprego, Tx_emprego,  i, q, SUBOCUPHORAS.REG, 
    SUBOCUPHORAS.RG, percentis, curva_de_lorenz) #tudo que não tem SH 

# Did not include agreInformal.REG, agreInformal.RG, agreInformal.RG.REG, 
# As there are no historic series for these variables

# Saving RData------------------------------------------------------------------
save.image("C:/Users/lauro/Desktop/PNADc/v3/dados_serie_historica_012024.RData")


# Transforming to Excel---------------------------------------------------------
library(dplyr)
library(tidyverse)

# Setting work directory--------------------------------------------------------

setwd("C:/Users/lauro/Desktop/PNADc/v3/tabelas")

# Listing variables by gender and race------------------------------------------
lista.RG = list(EMPREGO.RG.sh,
                giniRG.sh,
                informal.RG.sh,
                PEA.RG.sh,
                RendaEfet.RG.sh,
                RendaHabit.RG.sh, 
                trabalhosetor.RG.sh)

final.RG <- lista.RG %>% 
  reduce (inner_join, by =c('Categoria','Trimestre'))

# Listing variables by region---------------------------------------------------
lista.REG <- list(EMPREGO.REG.sh,
                  giniUF.sh,
                  informal.REG.sh,
                  PEA.REG.sh, 
                  trabalhosetor.REG.sh)

final.REG <- lista.REG %>% 
  reduce (inner_join, by =c('Regiao','Trimestre'))

# Listing variables by race, gender and region----------------------------------
lista.RG.REG <- list(EMPREGO.RG.REG.sh, 
                     informal.RG.REG.sh, 
                     PEA.RG.REG.sh)

final.RG.REG <- lista.RG.REG %>% 
  reduce (inner_join, by =c('Categoria_regiao','Trimestre'))

# Other variables---------------------------------------------------------------
lista.outras <- list(giniBR.sh,
                     informalidadeMedia.sh, 
                     RendaEfetivaMedia.sh, 
                     RendaHabitMedia.sh, 
                     Tx_desemprego.sh,Tx_emprego.sh)

final.outras <- lista.outras %>% 
  reduce (inner_join, by ='Trimestre')

lista.novas.RG <- list(carteira.RG, pobreza.RG, extrema.pobreza.RG)
final.novas.RG <- lista.novas.RG %>% reduce (inner_join, by =c('Regiao','Trimestre'))

ista.novas.RG.REG

lista.novas.REG

#Transforming to CSV------------------------------------------------------------
write.csv(quantis_de_renda23_04, "RelatorioPNAD_Dados_Renda_2304.csv")  #Gráfico de perecentis de renda
write.csv(final.RG, file = "RelatorioPNAD_Dados_RG_2304.csv")
write.csv(final.REG, file = "RelatorioPNAD_Dados_REG_2304.csv")
write.csv(final.RG.REG, file = "RelatorioPNAD_Dados_RG_REG_2304.csv")
write.csv(final.outras, file ="RelatorioPNAD_Dados_outras_sh_2304.csv")


