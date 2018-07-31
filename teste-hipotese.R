#Sites de noticiários dizem que o estado de São Paulo tem 40% (0.4) de ganhos na MegaSena.
# HO = SP é o estado que mais ganhou na MegaSena
# H1 = O estado que mais ganhou na MegaSena não foi São Paulo.
# ns =0.05

ns<- 0.05 

concursoGanhadores<- count(data.estados)
ganhadoresSP<- count(filter(data.estados, data.estados$UF=="SP"))
proporcao<- mean(data.estados$UF=="SP")

#transformar factor em numeric
estadosH <- data.estados
estadosH$UF<- as.character(V$UF)
estadosH$UF[estadosH$UF=="SP"]<- 1
estadosH$UF[estadosH$UF!="1"]<-0
estadosH$UF <- as.numeric(estadosH$UF)

# Vetor com a quantidade dos estados de Sp que ganharam a MegaSena
x<-aggregate(formula = UF ~ Concurso, FUN = sum, data= estadosH)


# Media 40% (0.4)
media = 0.38 # verificar essa média

Resultado<- t.test(x$UF, alternative = "two.sided", mu= media, conf.level = ns )

if( Resultado$p.value >= 0.05){
  print("Hipotese Nula aceita: São Paulo foi o estado que mais ganhou na MegaSena")
}else{
  print("Hipotese Nula rejeitada: O estado que teve a quantidade maior de ganhadores não foi são Paulo")
}

