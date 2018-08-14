#https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/kNN
install.packages('DMwR')
library(tidyr)
library(dplyr)
library(DMwR) #pacote com o KNN

projeto<-paste(getwd(), '/UFRPE/Computação para Análise de Dados/Códigos/projeto/', sep='')
#projeto<-paste(getwd(), '...')

data<-read.csv2(paste(projeto,'clean-data.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.estados<-read.csv2(paste(projeto,'clean-data-estados.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)

data<-select(data, Concurso, Dia_Sorteio, Mes_Sorteio)
data<-merge(data.estados, data, by='Concurso', all=TRUE)

levels(data$UF)<-c(levels(data$UF), 'NENHUM')
data$UF[is.na(data$UF)]<-'NENHUM'

train<-select(data, UF, Dia_Sorteio, Mes_Sorteio)

#mes 1 pra PE e 3 pra SP e PR
dia<-c(seq(1,31,1))
mes<-c(rep(1,31))
test<-data.frame(UF=NA, Dia_Sorteio=dia, Mes_Sorteio=mes)
print(dia)
kNN(UF ~ ., train, test, norm=TRUE, k=3)
