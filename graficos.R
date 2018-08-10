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

data<-read.csv2(paste(projeto,'clean-data.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.dezenas<-read.csv2(paste(projeto,'clean-data-dezenas.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.estados<-read.csv2(paste(projeto,'clean-data-estados.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)

View(data)
par(mfrow=c(1,1))
dev.off()
col.green<-'#00904E'
col.yellow<-'#F7CE2D'
col.blue<-'#0073B7'
gradient_green<-colorRampPalette(c(col.green, col.yellow))

#ocorrencia das dezenas
ocorrencia<-data.dezenas$Dezena
hist(ocorrencia, main='Histograma', ylab='Ocorrencia', xlab='Dezena', col=col.green, breaks=3*90, freq=F)
ocorrencia<-density(ocorrencia)
lines(ocorrencia)

#premio por ano
premioAno<-select(data, Ano_Sorteio, Rateio_Sena, Ganhadores_Sena)
premioAno<-mutate(premioAno, Premio_Ano=(Ganhadores_Sena*Rateio_Sena))
premioAno<-select(premioAno, Ano_Sorteio, Premio_Ano)
premioAno<-filter(premioAno, Ano_Sorteio<2018) #o ano atual ainda esta sendo computado
premioAno<-group_by(premioAno, Premio_Ano, Ano_Sorteio)
premioAno<-aggregate(Premio_Ano ~ Ano_Sorteio, FUN=sum, data=premioAno)
premioAno$Premio_Ano<-round(premioAno$Premio_Ano/1000000) #por milhoes de reais
plot(x=premioAno$Ano_Sorteio, y=premioAno$Premio_Ano, type='o', pch='$', col=col.green, 
     main='Premio por ano', xlab='Ano', ylab='Premio (Mi)', axes=F)
axis(side=1, at=premioAno$Ano_Sorteio,
     labels=premioAno$Ano_Sorteio,
     cex.axis=0.7)
axis(side=2, at=premioAno$Premio_Ano,
     labels=premioAno$Premio_Ano,
     cex.axis=0.7)

#ganhadores por ano
ganhadoresAno<-select(data, Ano_Sorteio, Ganhadores_Sena)
ganhadoresAno<-filter(ganhadoresAno, Ano_Sorteio<2018) #o ano atual ainda esta sendo computado
ganhadoresAno<-group_by(ganhadoresAno, Ganhadores_Sena, Ano_Sorteio)
ganhadoresAno<-aggregate(Ganhadores_Sena ~ Ano_Sorteio, FUN=sum, data=ganhadoresAno)
plot(x=ganhadoresAno$Ano_Sorteio, y=ganhadoresAno$Ganhadores_Sena, type='o', pch='$', col=col.blue, 
     main='Ganhadores por ano', xlab='Ano', ylab='Ganhadores', cex.axis='0.7', axes=F)
axis(side=1, at=ganhadoresAno$Ano_Sorteio,
     labels=ganhadoresAno$Ano_Sorteio,
     cex.axis=0.7)
axis(side=2, at=ganhadoresAno$Ganhadores_Sena,
     labels=ganhadoresAno$Ganhadores_Sena,
     cex.axis=0.7)

#ganhadores por quantidade de concursos
ganhadoresConcurso<-data$Acumulado
classes<-c('Ganhadores','Acumulado')
x<-c(sum(!ganhadoresConcurso[TRUE]), sum(ganhadoresConcurso[TRUE]))
ptc<-paste(round(x/sum(x)*100), '%', sep='')
pie(x, ptc, main='Ganhadores x Acumulado', col=c(col.green, col.yellow))
legend(x='bottomright', legend=classes, fill=c(col.green, col.yellow))

#ganhadores por estado
estados<-data.estados
estados<-aggregate(Concurso ~ UF, FUN=length, estados)

#pizza dos mais ganhadores - agrupar os menores 'outros'
estados<-estados[order(estados$Concurso, decreasing = TRUE), ]
estPie<-data.frame(estados=c(head(as.character(estados$UF)),'Outros'), qnt=c(head(estados$Concurso),sum(estados$Concurso)-sum(head(estados$Concurso))))
pct<-round(estPie$qnt/sum(estPie$qnt)*100)
pct<-paste(pct, '%)', sep='')
pct<-paste('(', pct, sep='')
labels<-paste(estPie$qnt, pct)
pie(estPie$qnt, labels, main="Ganhadores por estado", col=gradient_green(7))
legend("bottomright",
       legend=estPie$estados,
       fill=gradient_green(7))

#https://rstudio-pubs-static.s3.amazonaws.com/176768_ec7fb4801e3a4772886d61e65885fbdd.html
par(mar=c(1,1,1,1))
map("world","Brazil")
shape.estados<-readOGR(paste(projeto, "shapes/estados_2010.shp", sep=''))
levels(estados$UF)<-c(levels(estados$UF), 'AP')
estados<-data.frame(UF=c(as.character(estados$UF), 'AP'), Concurso=c(as.numeric(estados$Concurso), 0))
estados<-estados[order(estados$UF),]

shape.estados<-shape.estados[order(shape.estados$sigla),]
shape.estados$valor<-estados$Concurso
shape.estados<-shape.estados[order(shape.estados$valor, decreasing = TRUE),]
spplot(shape.estados, 'valor', col.regions=gradient_green(27), par.settings=list(axis.line=list(col= "transparent")), main='Ganhadores por Estado', col='#004E2A')


