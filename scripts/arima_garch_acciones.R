
#Librerias
rm(list = ls())
library(quantmod)
library(tseries)
library(forecast)
library(rugarch)
library(car)
install.packages("normtest")
library(normtest)

#Cargamos	los	datos	con	frecuencia	mensual	de	Nike Inc.,	definiendo	la	serie	de	retornos	del	precio	al	cierre:

getSymbols("NKE", from = "2010-01-01", to = "2021-11-05", periodicity = "monthly")

NKE.ts <- ts(NKE$NKE.Close, frequency = 12, 
             start = c(2010,1))
autoplot(NKE.ts)

#AUTOCORRELOGRAMAS

ggtsdisplay(NKE.ts, main = "precio	al	cierre")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE NO ES ESTACIONARIA

#   GRAFICO DE ESTACIONALIDAD
ggseasonplot(NKE.ts,polar=TRUE,main="precio	al	cierre" )

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(NKE.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA

# PHILLIP PERRON

pp.test(NKE.ts,alternative = "stationary")      #  p-value = 0.99 ... NO ES ESTACIONARIA


#Aplicamos una diferencia

NKE.rtn <- diff(log(NKE.ts))

autoplot(NKE.rtn, frequency =12, xlab = "Tiempo", ylab = "precio	al	cierre", main = "Comportamiento del precio	al	cierre de NIKE")
adf.test(NKE.rtn,alternative = "stationary")
pp.test(NKE.rtn,alternative = "stationary")


#AUTOCORRELOGRAMAS

ggtsdisplay(NKE.rtn, main = "precio	al	cierre")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE NO ES ESTACIONARIA

# TEST ESTACIONARIEDAD
# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(NKE.rtn,alternative = "stationary")     #  p-value = 0.01 ... ES ESTACIONARIA

# PHILLIP PERRON

pp.test(NKE.rtn,alternative = "stationary")      #  p-value =0.01 ... ES ESTACIONARIA


# INTERPRETACION: EN SU PRIMERA DIFERENCIA YA ESTA ESTACIONARIA


#Modelo de ruido blanco
### PROPUESTA 11
propuesta11 <- arima(NKE.ts, order = c(1,1,1), seasonal = list(order = c(1,0,1), period = 4), method = 'ML')
propuesta11
coeftest(propuesta11)
propuesta11$aic 

############################## Validación PROPUESTA11 ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(propuesta11, plot = TRUE)
tsdiag(propuesta11)

# NORMALIDAD 
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
normalidad # p-value = 1.202e-08 .... los datos no se distribuyen de forma normal

####Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_propuesta11<- Box.test(propuesta11$residuals, type="Ljung-Box")
independencia_propuesta11 # p-value = 0.9822 Los datos se distribuyen de forma independiente
independencia_propuesta11$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error11 <- residuals(propuesta11)
plot(error11)

### PROPUESTA 22
propuesta22 <- arima(NKE.ts, order = c(2,1,2), seasonal = list(order = c(1,1,0), period = 4), method = 'ML')
propuesta22
coeftest(propuesta22)
propuesta22$aic 

############################## Validación PROPUESTA22 ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(propuesta22, plot = TRUE)
tsdiag(propuesta22)

# NORMALIDAD 
### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
error2 <- residuals(propuesta22)
hist(error2)
jb.norm.test(error2) # p-value = 2.2e-16 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(propuesta22$residuals);qqline(propuesta22$residuals)
normalidad <-shapiro.test(propuesta22$residuals)
normalidad # p-value = 0.08729 .... los datos se distribuyen de forma normal

####Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_propuesta22<- Box.test(propuesta22$residuals, type="Ljung-Box")
independencia_propuesta22 # p-value = 0.954 Los datos se distribuyen de forma independiente

# Ruido blanco (es parte del test de heterocedasticidad)
error22 <- residuals(propuesta22)
plot(error22)

# FUNCION AUTO ARIMA
Modelo1 <- auto.arima(NKE.ts,d=2, trace = TRUE)
Modelo1

############################## Validación PROPUESTA22 ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(Modelo1, plot = TRUE)
tsdiag(Modelo1)

# NORMALIDAD 
### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
error3 <- residuals(Modelo1)
hist(error3)
jb.norm.test(error3) # p-value = 2.2e-16 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(Modelo1$residuals);qqline(Modelo1$residuals)
normalidad <-shapiro.test(Modelo1$residuals)
normalidad # p-value = 0.08729 .... los datos se distribuyen de forma normal

####Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_Modelo1<- Box.test(Modelo1$residuals, type="Ljung-Box")
independencia_Modelo1 # p-value = 0.954 Los datos se distribuyen de forma independiente


# Ruido blanco (es parte del test de heterocedasticidad)
error33 <- residuals(Modelo1)
plot(error33)

#Pronostico
prediccion1 <- predict(Modelo1,n.ahead = 12)
upper <- prediccion1$pred + 1.96*prediccion1$se
lower <- prediccion1$pred - 1.96*prediccion1$se
ts.plot(NKE.ts, prediccion1$pred, col=c("black","red"), main='FORECAST precio de la accion al cierre')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")
