####Librerias

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
datos <- read_excel("PIB%mensual.xls")
View(datos)


# SELECCIONAR LAS VARIABLES: Tipo de cambio al cierre

PIB <- as.matrix(datos$`Crecimiento del PIB per cápita (% anual)`/100)

# ESTADISTICOS

summary(PIB)

# CONVERTIR LAS VARIABLES A SERIES DE TIEMPO

PIB.ts <- ts(PIB, frequency = 12, start = c(2010,1))

autoplot(PIB.ts)

# PRIMERO: GRAFICOS 
autoplot(PIB.ts, frequency = 4, xlab = "Tiempo", ylab = "Crecimiento del PIB per cápita (% mensual)", main = "Comportamiento Crecimiento del PIB per cápita (% mensual) en Perú")

# SEGUNDO: AUTOCORRELOGRAMAS

ggtsdisplay(PIB.ts, main = "Crecimiento del PIB per cápita (% mensual)")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE PARECE ES ESTACIONARIA

#   GRAFICO DE ESTACIONALIDAD
ggseasonplot(PIB.ts,polar=TRUE,main="Crecimiento del PIB per cápita (% mensual)" )

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(PIB.ts,alternative = "stationary")     #  p-value = 0.01 ... ES ESTACIONARIA

# PHILLIP PERRON

pp.test(PIB.ts,alternative = "stationary")      #  p-value = 0.01 ... ES ESTACIONARIA


# MODELOS ARIMA
propuesta11 <- arima(PIB.ts, order = c(4,2,0), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuesta11
coeftest(propuesta11)
propuesta11$aic 


propuesta22 <- arima(PIB.ts, order = c(3,2,0), seasonal = list(order = c(1,1,0), period = 12), method = 'ML')
propuesta22
coeftest(propuesta22)
propuesta22$aic 

propuesta33 <- arima(PIB.ts, order = c(1,1,0), seasonal = list(order = c(1,1,0), period = 12), method = 'ML')
propuesta33
coeftest(propuesta33)
propuesta33$aic


# FUNCION AUTO ARIMA
Modelo1 <- auto.arima(PIB.ts,d=2, trace = TRUE)
Modelo1

### BEST MODEL: ARIMA(4,2,0)

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
jb.norm.test(error1) # p-value = 0.0135 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(propuesta11$residuals);qqline(propuesta11$residuals)
normalidad <-shapiro.test(propuesta11$residuals)
normalidad # p-value = 0.04806.... los datos se distribuyen de forma normal



########## Heterocedasticidad #############################################################################



# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente



independencia_propuesta11<- Box.test(propuesta11$residuals, type="Ljung-Box")
independencia_propuesta11 # p-value =  0.3413 Los datos se distribuyen de forma independiente
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
ts.plot(PIB.ts, prediccion1$pred, col=c("black","red"), main='FORECAST Tipo de Cambio')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")