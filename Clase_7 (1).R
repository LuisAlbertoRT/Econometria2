rm(list = ls())    #Limpia memoria
memory.size(max=T) 
setwd("C:/Salvador Ake/Trabajo/Cursos Poli/Econometría_Financiera/Primavera_2021/") # Establece directorio de trabajo
direccion <- "C:/Salvador Ake/Trabajo/Cursos Poli/Econometría_Financiera/Primavera_2021/rendimientos"
if (!require(quantmod)) install.packages('quantmod')
if (!require(plyr)) install.packages('plyr')
if (!require(tidyquant)) install.packages('tidyquant')
if (!require(ismev)) install.packages('ismev')
if (!require(evd)) install.packages('evd')
if (!require(MASS)) install.packages('MASS')
if (!require(metRology)) install.packages('metRology')
if (!require(tseries)) install.packages('tseries')
if (!require(lmtest)) install.packages('lmtest')
if (!require(forecast)) install.packages('forecast')
if (!require(FinTS)) install.packages('FinTS')
if (!require(rugarch)) install.packages('rugarch')


library(quantmod)
library(plyr)
library(tidyquant)
library(ismev)
library(evd)
library(MASS)
library(metRology)
library(tseries)
library(lmtest)
library(forecast)
library(FinTS)
library(rugarch)


#____________________________________________________________________________________________________________
pruebas_Ljung <- function(serie, rezagos) {
	#mat_prob_Ljung<- vector(mode = "numeric", length =(rezagos +1))
	Ljung_Box<- c("Probabilidad")
	contador1 =1
	for (contador1 in 1:rezagos){
		prueba<-Box.test ( serie, lag = contador1, type = "Ljung")
		Ljung_Box<- rbind(Ljung_Box,prueba[3])
	}
	#mat_prob_Ljung<- cbind(mat_prob_Ljung,Ljung_Box)
	Ljung_Box<- as.data.frame(Ljung_Box)
  return(Ljung_Box)
}

#_____________________________________________________________________________________________________________

symbols <- c("AAPL", "TSLA", "IBM")
data_env <- new.env()
getSymbols(Symbols = symbols, env = data_env, from = "2010-07-01", verbose = TRUE, to = "2021-05-12")
Datos <- do.call(merge, eapply(data_env, Cl))
Datos_rend <- apply(Datos, 2, ROC, type = "discrete")
#rm(Datos)
Datos_rend <- as.data.frame(Datos_rend[-1,])
fechas <-  as.Date(rownames(as.data.frame(Datos))[-1], "%Y-%m-%d")
Datos_rend<- as.xts(Datos_rend, order.by= fechas)


plot(Datos_rend, multi.panel = TRUE, main = "Rendimientos diarios", clev = 0, cex = 0.6, 
	cex.axis = 0.9, observation.based = FALSE, ylim = NULL, yaxis.same = TRUE, 
	yaxis.left = TRUE, yaxis.right = TRUE, grid.ticks.on = "months", grid.ticks.lwd = 1,
	grid.ticks.lty = 1,grid.col = "darkgray", labels.col = "#333333", format.labels = TRUE,
	shading = 1, bg.col = "#FFFFFF", grid2 = "#F5F5F5", legend.loc = "bottomleft")

contador1 <- 1
pdf(paste(direccion,"/rendimientos.pdf",sep=""))
for(contador1 in 1: ncol(Datos_rend)){
	print(plot.xts(x = Datos_rend[,contador1], main = paste("Rendimientos diarios de", names(Datos_rend[,contador1]), sep =" ") , auto.legend = TRUE))
}
dev.off()

contador1 <- 1
for(contador1 in 1: ncol(Datos_rend)){
	pdf(paste(direccion,names(Datos_rend[,contador1]),".pdf",sep = "_"))
		print(plot.xts(x = Datos_rend[,contador1], main = paste("Rendimientos diarios de", names(Datos_rend[,contador1]), sep =" ") , auto.legend = TRUE))
	dev.off()
}

contador1 <- 1
for(contador1 in 1: ncol(Datos_rend)){
	png(paste(direccion,names(Datos_rend[,contador1]),".png",sep = "_"))
		print(plot.xts(x = Datos_rend[,contador1], main = paste("Rendimientos diarios de", names(Datos_rend[,contador1]), sep =" ") , auto.legend = TRUE))
	dev.off()
}

contador1 <- 1
for(contador1 in 1:ncol(Datos_rend)){
	prueba_kpss <- kpss.test(Datos_rend[,contador1], null = "Level", lshort = TRUE)
	if( prueba_kpss$p.value > .05){
		#verdadero
		print(paste("La serie", names(Datos_rend[,contador1]), "es estacionaria, con una probabilidad de ", prueba_kpss$p.value,sep=" "))
	}else{
		#falso
		print(paste("La serie", names(Datos_rend[,contador1]), "es no estacionaria (raíz unitaria), con una probabilidad de ", prueba_kpss$p.value,sep=" "))	
	}
}

contador1 =1
pdf(paste(direccion,"/Gráfico_ACF_PACF.pdf",sep=""))
for( contador1 in 1:ncol(Datos_rend)){
	par(mfrow=c(2,1))
	acf(Datos_rend[,contador1],main =names(Datos_rend[,contador1]))
	pacf(Datos_rend[,contador1],main =names(Datos_rend[,contador1]))
}
dev.off()

max_lag = trunc(length(Datos_rend[,1])/3,0)
contador2 =1
mat_prob_Ljung<- vector(mode = "numeric", length =(max_lag +1))
for (contador2 in 1:ncol(Datos_rend)){
	Ljung_Box<- c(paste("Probabilidad",colnames(Datos_rend[,contador2]),sep = "_"))
	contador1 =1
	for (contador1 in 1:max_lag){
		prueba<-Box.test ( Datos_rend[,contador2] , lag = contador1, type = "Ljung")
		Ljung_Box<- rbind(Ljung_Box,prueba[3])
	}
	mat_prob_Ljung <- cbind(mat_prob_Ljung,Ljung_Box)
}
mat_prob_Ljung<-mat_prob_Ljung[,-1]
nombres<- c(mat_prob_Ljung[1,])
colnames(mat_prob_Ljung)<- nombres
mat_prob_Ljung<-mat_prob_Ljung[-1,]

modelo1<-arima(Datos_rend$AAPL.Close, order = c(2,0,2)) 
modelo2 <- auto.arima(Datos_rend$AAPL.Close, seasonal=FALSE)
coeftest(modelo)
coeftest(modelo2)
fijos <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,NA,NA,NA)
modelo3<- arima(Datos_rend$AAPL.Close,order=c(9,0,9),fixed=fijos, transform.pars=FALSE)  #Aquí se pone el orden del modelo
coeftest(modelo3)

contador1 =1
pdf(paste(direccion,"GráficoACP_FACP_res_modelo3.pdf",sep=""))
	par(mfrow=c(2,1))
	acf(modelo3$residuals,main =names(modelo$residuals))
	pacf(modelo3$residuals,main =names(modelo$residuals))
dev.off()

prueba_normalidad <- ks.test(modelo$residuals, "pnorm", mean(modelo$residuals), sd(modelo$residuals))   #Elabora la prueba Kolmogorv - Smirnov para los residuales del modelo, se puede cambiar "pnorm", por otra de las distribuciones paramétricas del R
print(prueba_normalidad)  #Imprime la prueba de normalidad

prueba_independencia<- bds.test(modelo$residuals, m=5)  # Hace una prueba BDS a los residuales, m es el número de dimensiones
print(prueba_independencia)

prueba_LM_GARCH<- ArchTest(modelo$residuals, lag = (k=trunc((length(modelo$residuals)^(1/3)))))  #Realiza un prueba LM-GARCH para la variable 1 del conjunto de datos
print(prueba_LM_GARCH)   #Imprime la prueba LM_GARCH

modelo_residuals2 <- modelo$residuals^2

pdf(paste(direccion,"Correlograma_residuales_2_modelo1.pdf",sep=""))
	par(mfrow=c(2,1))
	acf(modelo_residuals2 ,main =names(modelo$residuals))
	pacf(modelo_residuals2 ,main =names(modelo$residuals))
dev.off()


par(mfrow=c(2,1))
plot(modelo$residuals)
print(plot.xts(x = Datos_rend[,2], main = paste("Rendimientos diarios de", names(Datos_rend[,2]), sep =" ") , auto.legend = TRUE))




spec1 <- ugarchspec(variance.model = list(model = "eGARCH", 
                                         garchOrder = c(1,2), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   mean.model     = list(armaOrder = c(0,0), include.mean = TRUE),
  			distribution.model = "std") 

mi_garch <- ugarchfit(spec=spec1,data=Datos_rend$AAPL.Close,solver.control=list(trace=0))
print(mi_garch)
residuales_garch <- mi_garch@fit$residuals
pruebas_Ljung(residuales_garch, 15)



 

spec1 <- ugarchspec(variance.model = list(model = "eGARCH", 
                                         garchOrder = c(1,3), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   #mean.model     = list(armaOrder = c(0,0,0), 
                   #                     external.regressors = NULL,
                   #                     start.pars = list()),
  			distribution.model = "sstd", fixed.pars = list(beta2 = 0, omega = 0)) 

mi_garch <- ugarchfit(spec=spec1,data=Datos_rend$AAPL.Close,solver.control=list(trace=0))
print(mi_garch)
residuales_garch <- mi_garch@fit$residuals
pruebas_Ljung(residuales_garch, 15)


par(mfrow=c(2,1))
acf(residuales_garch ,main =names(modelo$residuals))
pacf(residuales_garch ,main =names(modelo$residuals))



#include.mean --- TRUE para añadir un modelo arma 
#En este caso, ya se muestran las pruebas t y los estadísticos
#En la parte de model, se mete la clase de modelo a estimar
# distribution.model: “norm”  --- distribución normal
#“snorm” --- normal con sesgo
# “std” --- distribución t de student
# “sstd” --- distribución t student sesgada
# “ged” --- distribución genralizada de errores 
# “sged” --- distribución generalizda de errores sesgada
# “nig” --- distribución normal inversa gaussiana
# “ghyp” --- Distribución Generalizada hiperbólica 
#“jsu” --- distribución de Johnson

#variance.model: “sGARCH”  --- GARCH estándard
# “eGARCH” ---  GARCH exponencial
# “gjrGARCH” ---  Glosten-Jagannathan-Runkle GARCH (GARCH asimétrico)
# “apARCH”   --- asymmetric power GARCH
# “iGARCH”   --- GARCH integrado
# “csGARCH”  --- GARCH por componentes permanentes y transitorios
# “fGARCH” --- family GARCH (mimetiza otros modelos GARCH), requiere un submodelo
	#submodel = “GARCH” --- El tradicional
	#		“TGARCH”--- Threshold GARCH (umbral)
	#		“AVGARCH” --- Absolute Value GARCH (Modelo de volatilidad de valor absoluto) 
	#		“NGARCH” --- Nonlinear GARCH
	#		“NAGARCH” --- Nesting Asymmetric GARCH (GARCH de asimetría anidada) 
	#		“APARCH” --- asymmetric power GARCH
	#		“GJRGARCH” --- Glosten-Jagannathan-Runkle GARCH (GARCH asimétrico)
#


set.seed(1235678)
# AR([1,5])-GARCH(1,1) - use default garch values and subset ar[.]
spec = garchSpec(model = list(mu = 0.001, ar = c(0.5,0,0,0,0.1)))
garch01.sim <- garchSim(spec, n = 100)
plot(garch01.sim,type='l', main='',ylab=expression(r[t]),xlab='t')


