#Sites de noticiários dizem que o estado de São Paulo tem 40% (0.4) de ganhos na MegaSena.
# HO = SP é o estado que mais ganhou na MegaSena
# H1 = O estado que mais ganhou na MegaSena não foi São Paulo.
# ns =0.05

projeto<-paste(getwd(), '/UFRPE/Computação para Análise de Dados/Códigos/projeto/', sep='')
#projeto<-paste(getwd(), '...')
data.estados<-read.csv2(paste(projeto,'clean-data-estados.csv', sep=''), sep=';', dec='.', header=T, na.strings='', strip.white=T)

media<-0.4
ns<-0.05

#transformar factor em numeric
estadosH<-data.estados
estadosH$UF<-as.character(estadosH$UF)
estadosH$UF[estadosH$UF!="SP"]<-0
estadosH$UF[estadosH$UF=="SP"]<-1
estadosH$UF<-as.numeric(estadosH$UF)

# Vetor com a quantidade dos estados de SP que ganharam a MegaSena
x<-aggregate(formula = UF ~ Concurso, FUN = sum, data= estadosH)

Resultado<-t.test(x$UF, alternative = "two.sided", mu= media, conf.level = ns )

if(Resultado$p.value >= 0.05){
  print("Hipotese Nula aceita: São Paulo foi o estado que mais ganhou na MegaSena")
}else{
  print("Hipotese Nula rejeitada: O estado que teve a quantidade maior de ganhadores não foi são Paulo")
}


