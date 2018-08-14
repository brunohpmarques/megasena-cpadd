
#transformar os estados em números para poder fazer o teste de regressão linear
Estados.Numericos<-data.estados
levels(Estados.Numericos$UF)
Estados.Numericos$UF<-as.factor(Estados.Numericos$UF)
levels(Estados.Numericos$UF)<-1:26
Estados.Numericos$UF<- as.numeric(Estados.Numericos$UF)
View(Estados.Numericos)

#----ESTIMATIVA DE ESTADOS GANHADORES----
r.estadosGanhadores<-lm(UF ~ Concurso, data= Estados.Numericos)
summary(r.estadosGanhadores)

# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: UF = 17.37 -0.0004*Concurso

# grafico da Regressão
plot (UF ~ Concurso, pch=16 ,data =Estados.Numericos)
abline(r.estadosGanhadores,col="red")
?pch
#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (UF). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.estadosGanhadores)

#Como Concurso Pr(>|t|) = 0.36457 > 0.05 então eles tem relação estatísticas
# R =  0.00125  o modelo de regressão não explicou grande parte da variabilidade no resultado.

#Analise dos Pressuposto 
# normalidade

normalidade<-shapiro.test(residuals(r.estadosGanhadores))

if(normalidade$p.value<=0.05){
  print("Hipotese Nula Rejeitada:os resíduos não são provenientes de uma distribuição Normal.")
}else{
  print("Hipotese Nula Aceita: os resíduos são provenientes de uma distribuição Normal.")
}

# Variavel Homogenea
plot(r.estadosGanhadores,1)

previsao<-data.frame(Concurso=c(2001,2030,2062))
predict(r.estadosGanhadores, previsao)

#------- Estimativa de Premios ---------

View(data)
r.EstimativaPremio<- lm(Estimativa_Premio~Concurso, data=data)
summary(r.EstimativaPremio)

# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Estimativa_Premio = -6719279.04 + 18574.99*Concurso

# grafico da Regressão
plot (Estimativa_Premio ~ Concurso, pch=16 ,data =data)
abline(r.EstimativaPremio,col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Estimativa_Premio). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.EstimativaPremio)

#Como Concurso Pr(>|t|) = 2.22e-16 < 0.05 então eles tem relação estatísticas.
# R =  0.2031  o modelo de regressão explicou grande parte da variabilidade no resultado.

#Analise dos Pressuposto 
# normalidade
normalidade<-shapiro.test(residuals(r.EstimativaPremio))

if(normalidade$p.value<=0.05){
  print("Hipotese Nula Rejeitada:os resíduos não são provenientes de uma distribuição Normal.")
}else{
  print("Hipotese Nula Aceita: os resíduos são provenientes de uma distribuição Normal.")
}

# Variavel Homogenea
plot(RegressaoEstimativaPremio,1)

previsao<-data.frame(Concurso=c(2001,2020,2068))
predict(r.EstimativaPremio, previsao)

#Previsão:
#Em 2001 o valor está estimado em: 30449279.0395198
#Em 2020 o valor está estimado em: 30802203.8788510
#Em 2068 o valor está estimado em: 31693803.4729509 

#------- Estimativa de Valor Acumulado ---------

View(data)
r.EstimativaValorAcumulado<- lm(Valor_Acumulado~Concurso, data=data)
summary(r.EstimativaValorAcumulado)

# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Valor_Acumulado = 3837015.91 + 8304.91*Concurso

# grafico da Regressão

plot (Valor_Acumulado ~ Concurso, pch=16 ,data =data)
abline(r.EstimativaValorAcumulado,col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Valor_Acumulado). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.EstimativaValorAcumulado)

#Como Concurso Pr(>|t|) = 2.22e-16 < 0.05 então eles tem relação estatísticas
# R =  0.077  o modelo de regressão explicou grande parte da variabilidade no resultado.

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


r.Acumulado_Mega_da_Virada<- lm(Acumulado_Mega_da_Virada~Concurso, data=data)
summary(r.Acumulado_Mega_da_Virada)
# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Acumulado_Mega_da_Virada = -10838241.63 +  26518.27*Concurso

# grafico da Regressão
plot (Acumulado_Mega_da_Virada ~ Concurso, pch=16 ,data =data)
abline(r.Acumulado_Mega_da_Virada,col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Acumulado_Mega_da_Virada). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.Acumulado_Mega_da_Virada)

#Como Concurso Pr(>|t|) = 2.22e-16 < 0.05 então eles tem relação estatísticas
# R =  0.49  o modelo de regressão explicou grande parte da variabilidade no resultado.

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

#------- Estimativa do o numero de ganhadores pra algum concurso futuro ---------

View(data)
r.NumeroGanhadores<- lm(Ganhadores_Sena~Concurso, data=data)
summary(r.NumeroGanhadores)
# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Concurso = -964.2 +  0.0054*Ganhadores_Quadra - 0.088*Ganhadores_Quina + 18.87*Ganhadores_Sena


# grafico da Regressão
plot (Ganhadores_Sena~Concurso, pch=16 ,data =data)
abline(r.NumeroGanhadores,col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Acumulado_Mega_da_Virada). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.NumeroGanhadores)

#Como Concurso Pr(>|t|) = 0.0045 < 0.05 então eles tem relação estatísticas
# R =  0.0065 o modelo de regressão explicou grande parte da variabilidade no resultado.

#Analise dos Pressuposto 
# normalidade

normalidade<-shapiro.test(residuals(r.NumeroGanhadores))

if(normalidade$p.value<=0.05){
  print("Hipotese Nula Rejeitada:os resíduos não são provenientes de uma distribuição Normal.")
}else{
  print("Hipotese Nula Aceita: os resíduos são provenientes de uma distribuição Normal.")
}

# Variavel Homogenea
plot(r.NumeroGanhadores,1)

previsao<-data.frame(Concurso=c(1,529,2068))
predict(r.NumeroGanhadores, previsao)

#Previsão:
#COLOCAR AS PREVISÕES
#------- Estimar o quando o estado de SP ele vai ganhar-----
Estados.Numericos
r.GanhadoresSP<- lm(UF~Concurso, data=Estados.Numericos)
r.GanhadoresSP<- lm(Concurso~UF, data=Estados.Numericos)
summary(r.GanhadoresSP)
# A equação da linha de regressão estimada pode ser escrita 
# da seguinte forma: Concurso = -964.2 +  0.0054*Ganhadores_Quadra - 0.088*Ganhadores_Quina + 18.87*Ganhadores_Sena


# grafico da Regressão
plot (UF~Concurso, pch=16 ,data =Estados.Numericos)
abline(r.GanhadoresSP,col="red")

#avaliar Modelo

#Estamos interessados em saber se X (Concurso) pode ou não ser utilizada
#para predizer Y (Acumulado_Mega_da_Virada). Se o valor de p associado ao coeficiente for igual ou
#menor que o nível de significância adotado, então aceitamos que existe uma
#relação estatísticas entre as variáveis. 

summary(r.GanhadoresSP)

#Como Concurso Pr(>|t|) = 0.0045 < 0.05 então eles tem relação estatísticas
# R =  0.0065 o modelo de regressão explicou grande parte da variabilidade no resultado.

#Analise dos Pressuposto 
# normalidade

normalidade<-shapiro.test(residuals(r.GanhadoresSP))

if(normalidade$p.value<=0.05){
  print("Hipotese Nula Rejeitada:os resíduos não são provenientes de uma distribuição Normal.")
}else{
  print("Hipotese Nula Aceita: os resíduos são provenientes de uma distribuição Normal.")
}

# Variavel Homogenea
plot(r.GanhadoresSP,1)

previsao<-data.frame(UF=c(25))
predict(r.GanhadoresSP, previsao, index=1:2)
View(Estados.Numericos)
#Previsão:
#COLOCAR AS PREVISÕES