#graficos
library(dplyr)
library(tidyr)

data<-read.csv2('C:\\Users\\Bruno\\Documents\\UFRPE\\Computação para Análise de Dados\\Códigos\\projeto\\clean-data.csv', sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.dezenas<-read.csv2('C:\\Users\\Bruno\\Documents\\UFRPE\\Computação para Análise de Dados\\Códigos\\projeto\\clean-data-dezenas.csv', sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.estados<-read.csv2('C:\\Users\\Bruno\\Documents\\UFRPE\\Computação para Análise de Dados\\Códigos\\projeto\\clean-data-estados.csv', sep=';', dec='.', header=T, na.strings='', strip.white=T)

View(data)

#ocorrencia das dezenas
ocorrencia<-data.dezenas$Dezena
hist(ocorrencia, main='Histograma', ylab='Ocorrencia', xlab='Dezena', col='green', breaks=3*90, freq=TRUE)

#premio por ano
premioAno<-select(data, Ano_Sorteio, Rateio_Sena, Ganhadores_Sena)
premioAno<-mutate(premioAno, Premio_Ano=(Ganhadores_Sena*Rateio_Sena))
premioAno<-select(premioAno, Ano_Sorteio, Premio_Ano)
premioAno<-filter(premioAno, Ano_Sorteio<2018) #o ano atual ainda esta sendo computado
premioAno<-group_by(premioAno, Premio_Ano, Ano_Sorteio)
premioAno<-aggregate(Premio_Ano ~ Ano_Sorteio, FUN=sum, data=premioAno)
plot(x=premioAno$Ano_Sorteio, y=premioAno$Premio_Ano, type='o', pch='$', col='green', 
     main='Premio por ano', xlab='Ano', ylab='Premio')

#ganhadores por ano
ganhadoresAno<-select(data, Ano_Sorteio, Ganhadores_Sena)
ganhadoresAno<-filter(ganhadoresAno, Ano_Sorteio<2018) #o ano atual ainda esta sendo computado
ganhadoresAno<-group_by(ganhadoresAno, Ganhadores_Sena, Ano_Sorteio)
ganhadoresAno<-aggregate(Ganhadores_Sena ~ Ano_Sorteio, FUN=sum, data=ganhadoresAno)
plot(x=ganhadoresAno$Ano_Sorteio, y=ganhadoresAno$Ganhadores_Sena, type='o', pch='$', col='blue', 
     main='Ganhadores por ano', xlab='Ano', ylab='Ganhadores')

#ganhadores por estado
estados<-data.estados
estados<-aggregate(Concurso ~ UF, FUN=length, estados)
barplot(estados$Concurso, main='Ganhadores por estado', ylab='Ganhadores', xlab='UF', names.arg=estados$UF, col=rainbow(26))
