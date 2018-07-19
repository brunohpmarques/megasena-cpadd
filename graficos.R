#graficos
install.packages(c('maps','mapdata','rworldmap','maptools','mapproj','ggmap','rgdal'))
library(dplyr)
library(tidyr)
library(maps) #mapas simples, eixos, escala, cidades 
library(mapdata) #base de dados WorldHires e rios
library(rworldmap) #outra base de dados de mapas do mundo
library(maptools) #Ler ESRI shapefiles 
library(mapproj) #Projeções e grids
library(ggmap) #Gmaps, OSM + mapas baseados em ggplot2
library(rgdal)

projeto<-paste(getwd(), '/UFRPE/Computação para Análise de Dados/Códigos/projeto/', sep='')
#projeto<-paste(getwd(), '...')

#https://rstudio-pubs-static.s3.amazonaws.com/176768_ec7fb4801e3a4772886d61e65885fbdd.html
par(mar=c(1,1,1,1))
shape.estados<-readOGR(paste(projeto, "shapes/estados_2010.shp", sep=''))
plot(shape.estados, main='Ganhadores por Estado', col='white')

#https://plot.ly/r/choropleth-maps/
#https://www.r-graph-gallery.com/175-choropleth-map-cartography-pkg/

data<-read.csv2(paste(projeto,'clean-data.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.dezenas<-read.csv2(paste(projeto,'clean-data-dezenas.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.estados<-read.csv2(paste(projeto,'clean-data-estados.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)

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

#pizza dos mais ganhadores - agrupar os menores 'outros'

#ganhadores por quantidade de concursos
ganhadoresConcurso<-data$Acumulado
classes<-c('Ganhadores','Acumulado')
x<-c(sum(!ganhadoresConcurso[TRUE]), sum(ganhadoresConcurso[TRUE]))
ptc<-paste(round(x/sum(x)*100), '%', sep='')
pie(x, ptc, main='Ganhadores x Acumulado', col=c('green','blue'))
legend(x='topleft', legend=classes, fill=c('green','blue'), cex = 0.70)


