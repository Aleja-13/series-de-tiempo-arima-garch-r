# Librerias: 
install.packages("dplyr")
install.packages("readxl")
install.packages("tseries")
install.packages("funModeling")
install.packages("TSA")
install.packages("urca")
install.packages("gridExtra")
install.packages("lmtest")
install.packages("ggfortify")
install.packages("xts")

library(dplyr) 
library(ggplot2) 
library(readxl) 
library(tseries) 
library(funModeling) 
library(forecast) 
library(TSA) 
library(urca) 
library(gridExtra) 
library(lmtest) 
library(ggfortify) 
library(xts)

install.packages("tseries")

#Cargar librerias
library(tseries)
library(lubridate)
library(tidyverse)
library(car)
library(astsa)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(car)
library(mlogit)
library(dplyr)
library(tidyr)
library(forecast)
library(fpp2)
library(stats)
library(quantmod)

#### Importar la base de datos

datos <- read.csv("datos/usd_pen_cierre.csv")
View(datos)

# SELECCIONAR LAS VARIABLES: Tipo de cambio al cierre

TDC <- as.matrix(datos$Ultimo)

# ESTADISTICOS

summary(TDC)

# CONVERTIR LAS VARIABLES A SERIES DE TIEMPO

TDC.ts <- ts(TDC, frequency = 4, start = c(2005,1))

autoplot(TDC.ts)

# PRIMERO: GRAFICOS 
autoplot(PBI, frequency = 4, xlab = "Tiempo", ylab = "Tipo de cambio PERU", main = "Comportamiento delTipo de cambio mensual en Perú")

# SEGUNDO: AUTOCORRELOGRAMAS

ggtsdisplay(TDC.ts, main = "Tipo de Cambio")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE NO ES ESTACIONARIA

#   GRAFICO DE ESTACIONALIDAD
ggseasonplot(TDC.ts,polar=TRUE,main="Tipo de cambio" )

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(TDC.ts,alternative = "stationary")     #  p-value = 0.01 ... ES ESTACIONARIA

# PHILLIP PERRON

pp.test(TDC.ts,alternative = "stationary")      #  p-value = 0.4143 ... ES ESTACIONARIA


# DIFERENCIAS

# 1RA FIFERENCIA

TDC_1d <- diff(TDC.ts, differences = 1)

autoplot(TDC_1d, frequency =12, xlab = "Tiempo", ylab = "Tipo de cambio en PERU", main = "Comportamiento del Tipo de cambio en PERU")
adf.test(TDC_1d,alternative = "stationary")
pp.test(TDC_1d,alternative = "stationary")

ggtsdisplay(TDC_1d, main = "Tipo de Cambio")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE NO ES ESTACIONARIA


# INTERPRETACION: EN SU PRIMERA DIFERENCIA YA ESTA ESTACIONARIA

# MODELOS ARIMA
propuesta11 <- arima(TDC.ts, order = c(1,1,0), seasonal = list(order = c(1,0,1), period = 4), method = 'ML')
propuesta11
coeftest(propuesta11)
propuesta11$aic 


propuesta22 <- arima(TDC.ts, order = c(2,1,2), seasonal = list(order = c(1,1,0), period = 4), method = 'ML')
propuesta22
coeftest(propuesta22)
propuesta22$aic 



# FUNCION AUTO ARIMA
Modelo1 <- auto.arima(TDC.ts,d=2, trace = TRUE)
Modelo1

### BEST MODEL: ARIMA(1,1,0)

############################## Validación ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(propuesta11)
tsdiag(propuesta11)

############################## NORMALIDAD #############################

install.packages("normtest")
library(normtest)

### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
error1 <- residuals(propuesta11)
hist(error1)
jb.norm.test(error1) # p-value = 2.2e-16 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(propuesta11$residuals);qqline(propuesta11$residuals)
normalidad <-shapiro.test(propuesta11$residuals)
normalidad # p-value = 0.08729 .... los datos se distribuyen de forma normal



########## Heterocedasticidad #############################################################################



# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente



independencia_propuesta11<- Box.test(propuesta11$residuals, type="Ljung-Box")
independencia_propuesta11 # p-value = 0.954 Los datos se distribuyen de forma independiente
# independencia_Propuesta1$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error1 <- residuals(propuesta11)
plot(error1)



######################### MODELO ARIMA ##########################
propuesta11$coef

#Pronostico
prediccion1 <- predict(propuesta11,n.ahead = 12)
upper <- prediccion1$pred + 1.96*prediccion1$se
lower <- prediccion1$pred - 1.96*prediccion1$se
ts.plot(TDC.ts, prediccion1$pred, col=c("black","red"), main='FORECAST Tipo de Cambio')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")

