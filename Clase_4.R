rm(list = ls())
memory.size(max=T) 
setwd("C:/Salvador Ake/Trabajo/Cursos Poli/Econometrķa_Financiera/Primavera_2021/")
direccion <- "C:/Salvador Ake/Trabajo/Cursos Poli/Econometrķa_Financiera/Primavera_2021"

if (!require( openxlsx)) install.packages('openxlsx')
if (!require(readxl)) install.packages('readxl')
if (!require(lubridate)) install.packages('lubridate')
if (!require(plyr)) install.packages('plyr')
if (!require(TTR)) install.packages('TTR')
if (!require(ursa)) install.packages('ursa')
if (!require(numbers)) install.packages('numbers')
if (!require(xts)) install.packages('xts')
if (!require(fUnitRoots)) install.packages('fUnitRoots')
if (!require(tseries)) install.packages('tseries')
#if (!require(aTSA)) install.packages('aTSA')
# if (!require(tseries)) install.packages('tseries')
# if (!require(tseries)) install.packages('tseries')
# if (!require(tseries)) install.packages('tseries')
# if (!require(tseries)) install.packages('tseries')
# if (!require(tseries)) install.packages('tseries')


library(openxlsx)
library(readxl)
library(lubridate)
library(plyr)
library(TTR)
library(ursa)
library(numbers)
library(xts)
library(fUnitRoots)
library(tseries)
#library(aTSA)
# library(tseries)
# library(tseries)
# library(tseries)
# library(tseries)
# library(tseries)



###############################################################################################################################################
#########################################################Inicia carga de datos#################################################################
###############################################################################################################################################

#BD <- read.xlsx(xlsxFile = paste(direccion,"/Deuda_USA.xlsx",sep=""), sheet = 1, colNames = TRUE, rowNames = FALSE,skipEmptyRows = TRUE)
contador1 <- 1
tot_hojas <- length( excel_sheets( paste(direccion,"/Deuda_USA.xlsx",sep="") ) )

for(contador1 in 1: (tot_hojas-1)){
	assign(paste("BD_",excel_sheets( paste(direccion,"/Deuda_USA.xlsx",sep=""))[contador1],sep=""), read.xlsx(xlsxFile = paste(direccion,"/Deuda_USA.xlsx",sep=""), sheet = contador1, colNames = TRUE, rowNames = FALSE,skipEmptyRows = TRUE))	
}

###############################################################################################################################################
#########################################################Termina carga de datos#################################################################
###############################################################################################################################################

class(BD_CPI[,1])  #Indica el tipo de objeto que se trabaja
BD_CPI <- BD_CPI[ which( mod(month(as.Date(BD_CPI[,1], origin = "1900-01-01")),3) == 1), c(1:2) ] 
BD <- join(BD_Debt, BD_CPI, by = "observation_date")
fact_inf <- ((ROC(BD[,3], n = 1, type = "discrete"))+1)
fact_inf[is.na(fact_inf )] <- 1
fact_inf <- cbind(BD[,1],fact_inf)
fact_inf <- fact_inf[order(fact_inf[,1], decreasing = TRUE),]
fact_inf[,2] <- cumprod(fact_inf[,2])
fact_inf <- fact_inf[order(fact_inf[,1], decreasing = FALSE),]
Debt_Jul_20 <- BD[,2]*fact_inf[,2]
BD <- cbind(BD, Debt_Jul_20)
BD2 <- xts( Debt_Jul_20, order.by = as.Date(BD[,1], origin = "1900-01-01"))
names(BD2) <- c("USA_Debt_Jul_2020")
pdf(paste(direccion,"/Deuda_USA_Jul_2020.pdf",sep =""))
	plot(BD2[,1], main = "Deuda USA a precios de Julio de 2020" )
dev.off()

adf_test <- adfTest(BD2[,1], lags = 5, type = "ct")

if( adf_test@test$p.value > .05){
	print("La serie es explosiva")
	deuda <- diff(BD2[,1], lag =1 )
	plot(deuda)
	adf_deuda <- adfTest(deuda, lags = 1, type = "nc")
	print(adf_deuda)

}else{
	print("La serie es estacionaria")
}

kpss_deuda <- kpss.test(BD2[,1], null = "Trend")

if( kpss_deuda$p.value > .05){
	print("La serie es estacionaria")
	deuda <- BD[,1]
}else{
	print("La serie es explosiva")
	deuda <- diff(BD2[,1], lag =1 )
	plot(deuda)
	kpss_deuda <- kpss.test(deuda, null = "Trend" )
	print(kpss_deuda)
}
#View(deuda)
#deuda <- deuda[-1]
deuda<-deuda[!is.na(deuda)]

pdf(paste(direccion,"/correlograma_1d_deuda_USA.pdf",sep =""))
	par(mfrow = c(2, 1))
	plot(acf(as.ts(deuda), lag.max = 36))
	plot(pacf(as.ts(deuda), lag.max = 36))
dev.off()

a <- Box.test(deuda, lag = 3, type = "Ljung-Box", fitdf = 0)
summary(arma(deuda, order = c(3, 3), include.intercept = TRUE))


#pp_deuda <- pp.test(BD2[,1], type = "Z_rho", lag.short = TRUE, output = TRUE)


