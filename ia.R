library(yarrr)
library(tidyr)
library(dplyr)

projeto<-paste(getwd(), '/UFRPE/Computação para Análise de Dados/Códigos/projeto/', sep='')
#projeto<-paste(getwd(), '...')

data<-read.csv2(paste(projeto,'clean-data.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.dezenas<-read.csv2(paste(projeto,'clean-data-dezenas.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.estados<-read.csv2(paste(projeto,'clean-data-estados.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)

#Verifica a correlação das variaveis Dezena e Concurso.
cor.test(formula = ~ Dezena + Concurso, data = data.dezenas)
#Cor: -0.01164096 ou seja, baixa ou nenhuma associacao.

#Verifica a correlação das variaveis Estado e Concurso.
cor.test(formula = ~ as.numeric(data.estados$UF) + Concurso, data = data.estados)
#Cor: -0.0336750 ou seja, baixa ou nenhuma associacao.

df<-select(data, Concurso, Mes_Sorteio)
df<-merge(data.dezenas, df, by='Concurso')
#Verifica a correlação das variaveis Dezena e Mes.
cor.test(formula = ~ Dezena + Mes_Sorteio, data=df)
#Cor: -0.0336750 ou seja, baixa ou nenhuma associacao.