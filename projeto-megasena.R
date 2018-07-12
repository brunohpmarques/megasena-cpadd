#projeto megasena
#data<-read.csv2('C:\\Users\\Bruno\\Documents\\UFRPE\\Computação para Análise de Dados\\Códigos\\dados\\MEGASENA-03-07-2018.txt', sep='\t', dec=',', header=T, na.strings='', strip.white=T)

data<-read.csv2('/Users/air/Documents/cpadd/MEGASENA-03-07-2018.txt', sep='\t', dec=',', header=T, na.strings='', strip.white=T, stringsAsFactors = F)

View(data)
str(data)

#remover linhas Nac
posicaoNA<- which(is.na(data))
data<-data[-posicaoNA,]
nrow(data)

data$Rateio_Sena<- gsub('[.]', '', data$Rateio_Sena)
data$Rateio_Sena<- sub(",", ".", data$Rateio_Sena)
data$Rateio_Sena<- as.numeric(data$Rateio_Sena)

data$Rateio_Quina<- gsub('[.]', '', data$Rateio_Quina)
data$Rateio_Quina<- sub(",", ".", data$Rateio_Quina)
data$Rateio_Quina<- as.numeric(data$Rateio_Quina)

data$Rateio_Quadra<- gsub('[.]', '', data$Rateio_Quadra)
data$Rateio_Quadra<- sub(",", ".", data$Rateio_Quadra)
data$Rateio_Quadra<- as.numeric(data$Rateio_Quadra)

data$Valor_Acumulado<- gsub('[.]', '', data$Valor_Acumulado)
data$Valor_Acumulado<- sub(",", ".", data$Valor_Acumulado)
data$Valor_Acumulado<- as.numeric(data$Valor_Acumulado)

data$Estimativa_Premio<- gsub('[.]', '', data$Estimativa_Premio)
data$Estimativa_Premio<- sub(",", ".", data$Estimativa_Premio)
data$Estimativa_Premio<- as.numeric(data$Estimativa_Premio)

data$Acumulado_Mega_da_Virada<- gsub('[.]', '', data$Acumulado_Mega_da_Virada)
data$Acumulado_Mega_da_Virada<- sub(",", ".", data$Acumulado_Mega_da_Virada)
data$Acumulado_Mega_da_Virada<- as.numeric(data$Acumulado_Mega_da_Virada)
