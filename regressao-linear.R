projeto<-paste(getwd(), '/UFRPE/Computação para Análise de Dados/Códigos/projeto/', sep='')
projeto<-paste(getwd(), '/', sep='')

data<-read.csv2(paste(projeto,'clean-data.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.dezenas<-read.csv2(paste(projeto,'clean-data-dezenas.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)
data.estados<-read.csv2(paste(projeto,'clean-data-estados.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)

#------- Estimativa de Premios ---------
# Para confirmar a correlação entre as variáveis usaremos o Coeficiente de Correlação Linear de Pearson.
cor.test(data$Concurso, data$Estimativa_Premio)
# a correlação foi 0.45 então ela possui um grau fraco associado

dataEstimativa<-data
dataEstimativa$Estimativa_Premio<-round(dataEstimativa$Estimativa_Premio/1000000) #por milhoes de reais
r.EstimativaPremio<- lm(Estimativa_Premio~Concurso, data=dataEstimativa)
summary(r.EstimativaPremio)

# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Estimativa_Premio = -6719279.04 + 18574.99*Concurso

# grafico da Regressão
plot (Estimativa_Premio ~ Concurso, pch=16 ,data=dataEstimativa, main='Dispersão Concurso x Estimativa de prêmio', ylab='Estimativa de Prêmio (Mi)', cex.axis=0.7)
abline(r.EstimativaPremio, col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Estimativa_Premio). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.EstimativaPremio)

#Como Concurso Pr(>|t|) = 2.22e-16 < 0.05 então eles tem relação estatísticas.
# R =  0.2031   o modelo de regressão não explicou grande parte da variabilidadeno resultado

#Analise dos Pressuposto 
# normalidade
normalidade<-shapiro.test(residuals(r.EstimativaPremio))

if(normalidade$p.value<=0.05){
  print("Hipotese Nula Rejeitada:os resíduos não são provenientes de uma distribuição Normal.")
}else{
  print("Hipotese Nula Aceita: os resíduos são provenientes de uma distribuição Normal.")
}

# Variavel Homogenea
plot(r.EstimativaPremio,1)

previsao<-data.frame(Concurso=c(2001,2020,2068))
predict(r.EstimativaPremio, previsao)

#Previsão:
#Em 2001 o valor está estimado em: 30449279.0395198
#Em 2020 o valor está estimado em: 30802203.8788510
#Em 2068 o valor está estimado em: 31693803.4729509 

#------- Estimativa de Valor Acumulado ---------
#Para confirmar a correlação entre as variáveis usaremos o Coeficiente de Correlação Linear de Pearson.
cor.test(data$Concurso, data$Valor_Acumulado)
# a correlação foi 0.28 então ela possui um grau fraco de associacao

View(data)
dataEstimativa<-data
dataEstimativa$Valor_Acumulado<-round(dataEstimativa$Valor_Acumulado/1000000) #por milhoes de reais
r.EstimativaValorAcumulado<- lm(Valor_Acumulado~Concurso, data=dataEstimativa)
summary(r.EstimativaValorAcumulado)

# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Valor_Acumulado = 3837015.91 + 8304.91*Concurso

# grafico da Regressão
plot (Valor_Acumulado ~ Concurso, pch=16, data=dataEstimativa, main='Dispersão Concurso x Acumulado', ylab='Acumulado (Mi)', cex.axis=0.7)
abline(r.EstimativaValorAcumulado, col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Valor_Acumulado). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.EstimativaValorAcumulado)

#Como Concurso Pr(>|t|) = 2.22e-16 < 0.05 então eles tem relação estatísticas
# R =  0.077   o modelo de regressão não explicou grande parte da variabilidade no resultado

#Analise dos Pressuposto 
# normalidade
normalidade<-shapiro.test(residuals(r.EstimativaValorAcumulado))

if(normalidade$p.value<=0.05){
  print("Hipotese Nula Rejeitada:os resíduos não são provenientes de uma distribuição Normal.")
}else{
  print("Hipotese Nula Aceita: os resíduos são provenientes de uma distribuição Normal.")
}

# Variavel Homogenea
plot(r.EstimativaValorAcumulado,1)

previsao<-data.frame(Concurso=c(2001,2020,2068))
predict(r.EstimativaValorAcumulado, previsao)

#Previsão:
#Em 2001 o valor acumulado está estimado em: 20455133.6680038
#Em 2020 o valor acumulado está estimado em: 20612926.8900945
#Em 2068 o valor acumulado está estimado em: 21011562.3985342 

#------- Estimativa de Valor Acumulado MegaVirada ---------
#Para confirmar a correlação entre as variáveis usaremos o Coeficiente de Correlação Linear de Pearson.
cor.test(data$Concurso, data$Acumulado_Mega_da_Virada)
# a correlação foi 0.70 então ela possui um grau moderado ou bom associado

dataEstimativa<-data
dataEstimativa$Acumulado_Mega_da_Virada<-round(dataEstimativa$Acumulado_Mega_da_Virada/1000000) #por milhoes de reais
r.Acumulado_Mega_da_Virada<- lm(Acumulado_Mega_da_Virada~Concurso, data=dataEstimativa)
summary(r.Acumulado_Mega_da_Virada)
# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Acumulado_Mega_da_Virada = -10838241.63 +  26518.27*Concurso

# grafico da Regressão
plot (Acumulado_Mega_da_Virada ~ Concurso, pch=16, data=dataEstimativa, main='Dispersão Concurso x Acumulado Mega da Virada', ylab='Acumulado Mega da Virada (Mi)', cex.axis=0.7)
abline(r.Acumulado_Mega_da_Virada, col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Acumulado_Mega_da_Virada). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.Acumulado_Mega_da_Virada)

#Como Concurso Pr(>|t|) = 2.22e-16 < 0.05 então eles tem relação estatísticas
# R =  0.49   o modelo de regressão não explicou grande parte da variabilidade no resultado

#Analise dos Pressuposto 
# normalidade

normalidade<-shapiro.test(residuals(r.Acumulado_Mega_da_Virada))

if(normalidade$p.value<=0.05){
  print("Hipotese Nula Rejeitada:os resíduos não são provenientes de uma distribuição Normal.")
}else{
  print("Hipotese Nula Aceita: os resíduos são provenientes de uma distribuição Normal.")
}

# Variavel Homogenea
plot(r.Acumulado_Mega_da_Virada,1)

previsao<-data.frame(Concurso=c(2001,2020,2068))
predict(r.Acumulado_Mega_da_Virada, previsao)

#Previsão:
#Em 2001 o valor Acumulado da Mega da Virada está estimado em: 42224816.6541231
#Em 2020 o valor Acumulado da Mega da Virada está estimado em: 42728663.7842579
#Em 2068 o valor Acumulado da Mega da Virada está estimado em: 44001540.7445984 

