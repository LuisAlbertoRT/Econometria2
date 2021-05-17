#rm(list = ls())
memory.size(max=T) 
setwd("C:/Salvador Ake/Trabajo/Cursos Poli/Econometría_Financiera/Primavera_2021/")
direccion <- "C:/Salvador Ake/Trabajo/Cursos Poli/Econometría_Financiera/Primavera_2021"

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
if (!require(nortest)) install.packages('nortest')
if (!require(forecast)) install.packages('forecast')
if (!require(FinTS)) install.packages('FinTS')
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
library(nortest)
library(forecast)
library(FinTS)
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
summary(arma(deuda, order = c(3, 3), include.intercept = TRUE))




#pp_deuda <- pp.test(BD2[,1], type = "Z_rho", lag.short = TRUE, output = TRUE)

#fijos=c(AR1,AR2,AR3,AR4,AR5,MA1,MA2,MA3,MA4,MA5,c)     #El último es el intercepto

#Los valores que son 0 no son tomados en cuenta, sólo se calculan los que tienen NA, en este caso MA3
#order=c(AR,0,MA)  me da los máximos
#lag=list(ar=c(1,3),ma=1)

rezagos <- list(ar = c(1,3), ma = c(2))
modelo_0 <- arma(deuda, order = c(3, 3), lag = rezagos, include.intercept = FALSE)  
resids_modelo_0 <- modelo_0$ residuals[!is.na(modelo_0$ residuals)]
rezagos <- 33
pdf(paste(direccion,"/correlograma_res_modelo_0.pdf",sep =""))
	par(mfrow = c(2, 1))
	plot(acf(as.ts(resids_modelo_0), lag.max = rezagos))
	plot(pacf(as.ts(resids_modelo_0), lag.max = rezagos))
dev.off()
ciz <- c(-1,1)*(-qnorm((1- .05))/sqrt( rezagos))
cir <- (exp(2*ciz)-1)/(exp(2*ciz)+1 )

acf_resids_0 <- acf(as.ts(resids_modelo_0), lag.max = rezagos)
estr_res <- which ( (acf_resids_0$acf < ciz[2] | acf_resids_0$acf > ciz[1]))
#No hay estructuras lineales residuales
bds.test(resids_modelo_0, m =3) #H0: la serie es iid
ad.test(resids_modelo_0)
ks.test(resids_modelo_0, pnorm)



modelo_1 <- auto.arima(deuda)
resids_modelo_1 <- modelo_1$ residuals[!is.na(modelo_1$ residuals)]
ciz <- c(-1,1)*(-qnorm((1- .05))/sqrt( rezagos))
cir <- (exp(2*ciz)-1)/(exp(2*ciz)+1 )

acf_resids_1 <- acf(as.ts(resids_modelo_1), lag.max = rezagos)
estr_res_acf <- which ( (acf_resids_1$acf < ciz[2] | acf_resids_1$acf > ciz[1]))
pacf_resids_1 <- pacf(as.ts(resids_modelo_1), lag.max = rezagos)
estr_res_pacf <- which ( (pacf_resids_1$acf < ciz[2] | pacf_resids_1$acf > ciz[1]))

pdf(paste(direccion,"/correlograma_res_modelo_0.pdf",sep =""))
	par(mfrow = c(2, 1))
	plot(acf(as.ts(resids_modelo_1), lag.max = rezagos))
	plot(pacf(as.ts(resids_modelo_1), lag.max = rezagos))
dev.off()
bds.test(resids_modelo_1, m =3) #H0: la serie es iid
ad.test(resids_modelo_1)
ks.test(resids_modelo_1, pnorm)
kurtosis(resids_modelo_1)


#No hay estructuras lineales residuales


fijos=c(NA,0,NA,0,NA,0,0)
modelo_2 <- arima(deuda,order=c(3,0,3),fixed=fijos, transform.pars=FALSE)

pt_modelo_1 <- pt(modelo_1$coef/(diag((modelo_1$var.coef))^0.5), length(deuda) - 2, lower.tail = TRUE)

modelos <- 2
contador1 <- 1
a<- c()
for(contador1 in 1: modelos){
	print( paste("El modelo", contador1, "tiene un valor de Akaike de", 
		BIC(get(paste("modelo_",contador1,sep=""))) ,sep=" ") )	
		a<- append(a, BIC(get(paste("modelo_",contador1,sep=""))))
}

print(paste("El mejor modelo es el ",which(a == min(a)),sep=""))
print(summary(get(paste("modelo_",which(a == min(a)),sep=""))))


#BIC(modelo_1)
#AIC(modelo_1)

pdf(paste(direccion,"/correlograma_res_cuad_modelo_1.pdf",sep =""))
	par(mfrow = c(2, 1))
	plot(acf(as.ts(resids_modelo_1 ^2), lag.max = rezagos))
	plot(pacf(as.ts(resids_modelo_1 ^2), lag.max = rezagos))
dev.off()

ArchTest (deuda, lags=12, demean = FALSE) 








