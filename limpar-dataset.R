#Limpar dataset
install.packages("dplyr")
install.packages("tidyr")
library(tidyr)
library(dplyr)
options(digits=15)

#data<-read.csv2('C:\\Users\\Bruno\\Documents\\UFRPE\\Computação para Análise de Dados\\Códigos\\projeto\\MEGASENA-03-07-2018.txt', sep='\t', dec=',', header=T, na.strings='', strip.white=T)
data<-read.csv2('/Users/air/Documents/cpadd/MEGASENA-03-07-2018.txt', sep='\t', dec=',', header=T, na.strings='', strip.white=T, stringsAsFactors = F)

View(data)

#remover linhas NAs
posicaoNA<- which(is.na(data))
data<-data[-posicaoNA,]
nrow(data)
#fim remover linhas NAs

#converte data formatada para date
data<- data %>% separate(col='Data_Sorteio', into=c('Dia_Sorteio','Mes_Sorteio','Ano_Sorteio', sep='/'))
data<-data[,-5]
#data$Data_Sorteio<- as.Date(data$Data_Sorteio, format='%d/%m/%Y')
#fim converte data formatada para date

#converte moeda formatada para decimal
data$Rateio_Sena<- gsub('[.]', '', data$Rateio_Sena)
data$Rateio_Sena<- sub(',', '', data$Rateio_Sena)
data$Rateio_Sena<- as.numeric(data$Rateio_Sena)

data$Rateio_Quina<- gsub('[.]', '', data$Rateio_Quina)
data$Rateio_Quina<- sub(',', '', data$Rateio_Quina)
data$Rateio_Quina<- as.numeric(data$Rateio_Quina)/100

data$Rateio_Quadra<- gsub('[.]', '', data$Rateio_Quadra)
data$Rateio_Quadra<- sub(',', '', data$Rateio_Quadra)
data$Rateio_Quadra<- as.numeric(data$Rateio_Quadra)/100

data$Valor_Acumulado<- gsub('[.]', '', data$Valor_Acumulado)
data$Valor_Acumulado<- sub(',', '', data$Valor_Acumulado)
data$Valor_Acumulado<- as.numeric(data$Valor_Acumulado)/100

data$Estimativa_Premio<- gsub('[.]', '', data$Estimativa_Premio)
data$Estimativa_Premio<- sub(',', '', data$Estimativa_Premio)
data$Estimativa_Premio<- as.numeric(data$Estimativa_Premio)/100

data$Acumulado_Mega_da_Virada<- gsub('[.]', '', data$Acumulado_Mega_da_Virada)
data$Acumulado_Mega_da_Virada<- sub(',', '', data$Acumulado_Mega_da_Virada)
data$Acumulado_Mega_da_Virada<- as.numeric(data$Acumulado_Mega_da_Virada)/100

data$Arrecadacao_Total<- gsub('[.]', '', data$Arrecadacao_Total)
data$Arrecadacao_Total<- sub(',', '', data$Arrecadacao_Total)
data$Arrecadacao_Total<- as.numeric(data$Arrecadacao_Total)/100
#fim converte moeda formatada para decimal

#altera valores de SIM e NAO para TRUE e FALSE
levels(data$Acumulado)<- c(FALSE, TRUE)
data$Acumulado<- as.logical(data$Acumulado)
#fim altera valores de SIM e NAO para TRUE e 

#cria dataset organizada para as dezenas
g<-select(data,Concurso,X1_Dezena,X2_Dezena,X3_Dezena,X4_Dezena,X5_Dezena,X6_Dezena)
data.dezenas<-gather(g, key='Ordem_Sorteio', value='Dezena', -Concurso)
data.dezenas$Ordem_Sorteio<- factor(data.dezenas$Ordem_Sorteio)
levels(data.dezenas$Ordem_Sorteio)<-c(1,2,3,4,5,6)
View(data.dezenas)

data<- select(data,-X1_Dezena,-X2_Dezena,-X3_Dezena,-X4_Dezena,-X5_Dezena,-X6_Dezena)
View(data)
#fim cria dataset organizada para as dezenas

#cria dataset organizado para os estados ganhadores
g<-select(data,Concurso,UF)
g<-filter(g, is.na(g$UF)==FALSE)
View(g)
str(g)
g$UF<-as.character(g$UF)
separate(g$UF, sep=',', into='UF')
g<- g %>%
  transform(UF = strsplit(UF, ",")) %>%
  unnest(UF)
#fim cria dataset organizado para os estados ganhadores

#TODO criar dataset organizado para as regioes ganhadores

Regioes<-c(rep("Norte",7), rep("Nordeste",9), rep("CentroOeste",4), rep("Sul",3), rep("Sudeste",4))
UF<-c("AM","RR","AP","PA","TO","RO","AC","AL","BA","CE","MA","PB","PE","PI","RN","SE",
           "DF","GO","MT","MS","PR","SC","RS","MG","RJ","SP","ES")
data.regioes<- data.frame(Regioes, UF)

 
#
#fim criar dataset organizado para as regioes ganhadores

#grava no arquivo
getwd()
write.table(data, 'clean-data.csv', sep=';', row.names=FALSE)
write.table(data.dezenas, 'clean-data-dezenas.csv', sep=';', row.names=FALSE)
write.table(g, 'clean-data-estados.csv', sep=';', row.names=FALSE)


