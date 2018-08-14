#https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/kNN
install.packages('DMwR')
library(tidyr)
library(dplyr)
library(DMwR) #pacote com o KNN

projeto<-paste(getwd(), '/UFRPE/Computação para Análise de Dados/Códigos/projeto/', sep='')
#projeto<-paste(getwd(), '...')

data<-read.csv2(paste(projeto,'clean-data.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.dezenas<-read.csv2(paste(projeto,'clean-data-dezenas.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.estados<-read.csv2(paste(projeto,'clean-data-estados.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)

#knn para prever o estado ganhador a partir de uma data
data.knn<-select(data, Concurso, Dia_Sorteio, Mes_Sorteio)
data.knn<-merge(data.estados, data.knn, by='Concurso', all=TRUE)

levels(data.knn$UF)<-c(levels(data.knn$UF), 'NENHUM')
data.knn$UF[is.na(data.knn$UF)]<-'NENHUM'

train<-select(data.knn, -Concurso)

#mes 1 pra PE e 3 pra SP e PR
dia<-c(seq(1,31,1))
mes<-c(rep(1,31))
test<-data.frame(UF=NA, Dia_Sorteio=dia, Mes_Sorteio=mes)
print(dia)
kNN(UF ~ ., train, test, norm=TRUE, k=3)



#knn pra prever se um jogo ganharia ou nao
data.dezenas<-spread(data.dezenas, key='Ordem_Sorteio', value='Dezena')
data.knn<-select(data, Concurso, Acumulado)
data.knn<-merge(data.dezenas, data.knn, by='Concurso')

train<-select(data.knn, -Concurso)
acumula<-TRUE
#while (acumula) {
dezenas<-sort(sample(1:60, 6)) # gera jogo aleatorio
test<-data.frame('1'=dezenas[1], '2'=dezenas[2], '3'=dezenas[3], '4'=dezenas[4], '5'=dezenas[5], '6'=dezenas[6], Acumulado=NA)
acumula<-as.logical(kNN(Acumulado ~ ., train, test, norm=TRUE, k=3))
#}
print(acumula)
print(dezenas)
