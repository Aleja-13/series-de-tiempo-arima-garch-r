#### INFLACIÓN de series mensuales de 5 países de la región: Chile, Colombia, Brasil, México y Perú. 

#Librerias
library(ggplot2) 
library(readxl) 
library(tseries) 
library(forecast) 
library(gridExtra) 
library(lmtest) 
library(ggfortify) 
library(xts)
library(normtest)

#### Importar la base de datos
Inflacion <- read.csv2("datos/Inflacion_5paises.csv")

# Creamos los datos por pais

br <- as.matrix(Inflacion$ï..Brasil)
cl <- as.matrix(Inflacion$Chile)
co <- as.matrix(Inflacion$Colombia)
mx <- as.matrix(Inflacion$MÃ.xico)
pe <- as.matrix(Inflacion$PerÃº)

#Gráficos de las series originales 
br.ts <- ts(log(br), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(br.ts, frequency =12, xlab = "Tiempo", ylab = "Inflacion en BRASIL", main = "Comportamiento de la inflacion en BRASIL")

cl.ts <- ts(log(cl), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(cl.ts, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Chile", main = "Comportamiento de la inflacion en Chile")

co.ts <- ts(log(co), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(co.ts, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Colombia", main = "Comportamiento de la inflacion en Colombia")

mx.ts <- ts(log(mx), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(mx.ts, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Mexico", main = "Comportamiento de la inflacion en Mexico")

pe.ts <- ts(log(pe), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(pe.ts, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Peru", main = "Comportamiento de la inflacion en Peru")

#Graficamos las series en conjunto
autoplot(ts.union(pe.ts, mx.ts, co.ts, br.ts, cl.ts), frequency =12, xlab = "Tiempo", ylab = "Inflacion", main = "Comportamiento de la inflacion")


### Gráfico de estacionalidad
ggtsdisplay(br.ts, main = "Inflacion en Brasil")

ggtsdisplay(cl.ts, main = "Inflacion en Chile")

ggtsdisplay(co.ts, main = "Inflacion en Colombia")

ggtsdisplay(mx.ts, main = "Inflacion en México")

ggtsdisplay(pe.ts, main = "Inflacion en Perú")


## Pruebas de estacionariedad (formales e informales)
# DICKEY FULLER
adf.test(br.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA
adf.test(cl.ts,alternative = "stationary")     #  p-value = 0.9359 ... NO ES ESTACIONARIA
adf.test(co.ts,alternative = "stationary")     #  p-value = 0.6389 ... NO ES ESTACIONARIA
adf.test(mx.ts,alternative = "stationary")     #  p-value = 0.6191 ... NO ES ESTACIONARIA
adf.test(pe.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA

# PHILLIP PERRON
pp.test(br.ts,alternative = "stationary")      #  p-value = 0.99 ... ES ESTACIONARIA
pp.test(cl.ts,alternative = "stationary")      #  p-value = 0.99 ... ES ESTACIONARIA
pp.test(co.ts,alternative = "stationary")      #  p-value = 0.7983 ... ES ESTACIONARIA
pp.test(mx.ts,alternative = "stationary")      #  p-value = 0.6992 ... ES ESTACIONARIA
pp.test(pe.ts,alternative = "stationary")      #  p-value = 0.979 ... ES ESTACIONARIA

# DIFERENCIAS
ndiffs(br.ts)
br_d <- diff(br.ts, differences = 2)
ndiffs(cl.ts)
cl_d <- diff(cl.ts, differences = 1)
ndiffs(co.ts)
co_d <- diff(co.ts, differences = 1)
ndiffs(mx.ts)
mx_d <- diff(mx.ts, differences = 1)
ndiffs(pe.ts)
pe_d <- diff(pe.ts, differences = 1)

#Graficamos las series en conjunto
autoplot(ts.union(pe_d, mx_d, co_d, br_d, cl_d), frequency =12, xlab = "Tiempo", ylab = "Inflacion", main = "Comportamiento de la inflacion")


### Gráfico de estacionalidad
ggtsdisplay(br_d, main = "Inflacion en Brasil")

ggtsdisplay(cl_d, main = "Inflacion en Chile")

ggtsdisplay(co_d, main = "Inflacion en Colombia")

ggtsdisplay(mx_d, main = "Inflacion en México")

ggtsdisplay(pe_d, main = "Inflacion en Perú")

#Presentacion del modelo Arima Construido (propio)

propuesta1br <- arima(br.ts, order = c(1,2,1), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuesta1br
coeftest(propuesta1br)
propuesta1br$aic 

propuesta1cl <- arima(cl.ts, order = c(1,1,0), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuesta1cl
coeftest(propuesta1cl)
propuesta1cl$aic 

propuesta1co <- arima(co.ts, order = c(2,1,2), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuesta1co
coeftest(propuesta1co)
propuesta1co$aic 

propuesta1mx <- arima(mx.ts, order = c(0,1,0), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuesta1mx
coeftest(propuesta1mx)
propuesta1mx$aic 

propuesta1pe <- arima(pe.ts, order = c(2,1,2), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuesta1pe
coeftest(propuesta1pe)
propuesta1pe$aic 


#Presentacion del modelo Arima Autoarima

modeloBR <- auto.arima(br.ts,d=2, trace = TRUE)
modeloBR

modeloCL <- auto.arima(cl.ts,d=1, trace = TRUE)
modeloCL

modeloCO <- auto.arima(co.ts,d=1, trace = TRUE)
modeloCO

modeloMX <- auto.arima(mx.ts,d=1, trace = TRUE)
modeloMX

modeloPE <- auto.arima(pe.ts,d=1, trace = TRUE)
modeloPE

#####Validacion de los modelos
## BRASIL
# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(modeloBR)
tsdiag(modeloBR)

#####NORMALIDAD 

### test de Jarque Bera
## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorBR <- residuals(modeloBR)
hist(errorBR)
jb.norm.test(errorBR) # p-value =  0.2965 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(modeloBR$residuals);qqline(modeloBR$residuals)
normalidad <-shapiro.test(modeloBR$residuals)
normalidad # p-value = 0.2993 .... los datos se distribuyen de forma normal

########## Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_modeloBR<- Box.test(modeloBR$residuals, type="Ljung-Box")
independencia_modeloBR # p-value = 0.6659 Los datos se distribuyen de forma independiente
independencia_modeloBR$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error.BR <- residuals(modeloBR)
plot(error.BR)

####MODELO ARIMA
modeloBR$coef

#### CHILE

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(modeloCL)
tsdiag(modeloCL)

#####NORMALIDAD 

### test de Jarque Bera
## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorCL <- residuals(modeloCL)
hist(errorCL)
jb.norm.test(errorCL) # p-value = 0.718 Los datos se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(modeloCL$residuals);qqline(modeloCL$residuals)
normalidad <-shapiro.test(modeloCL$residuals)
normalidad # p-value = 0.4326 .... los datos se distribuyen de forma normal

########## Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_modeloCL<- Box.test(modeloCL$residuals, type="Ljung-Box")
independencia_modeloCL # p-value = 0.9191 Los datos se distribuyen de forma independiente
independencia_modeloCL$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error.CL <- residuals(modeloCL)
plot(error.CL)

####MODELO ARIMA
modeloCL$coef

###COLOMBIA
# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(modeloCO)
tsdiag(modeloCO)

#####NORMALIDAD 

### test de Jarque Bera
## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorCO <- residuals(modeloCO)
hist(errorCO)
jb.norm.test(errorCO) # p-value = 2.2e-16 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(modeloCO$residuals);qqline(modeloCO$residuals)
normalidad <-shapiro.test(modeloCO$residuals)
normalidad # p-value= 9.942e-10 .... los datos no se distribuyen de forma normal

########## Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_modeloCO<- Box.test(modeloCO$residuals, type="Ljung-Box")
independencia_modeloCO # p-value = 0.9198 Los datos se distribuyen de forma independiente
independencia_modeloCO$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error.CO <- residuals(modeloCO)
plot(error.CO)

####MODELO ARIMA
modeloCO$coef

#### MEXICO
# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(propuesta1mx)
tsdiag(propuesta1mx)

#####NORMALIDAD 

### test de Jarque Bera
## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorMX <- residuals(propuesta1mx)
hist(errorMX)
jb.norm.test(errorMX) # p-value = 0.3055 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(propuesta1mx$residuals);qqline(propuesta1mx$residuals)
normalidad <-shapiro.test(propuesta1mx$residuals)
normalidad # p-value = 0.3009 .... los datos se distribuyen de forma normal

########## Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_propuesta1mx<- Box.test(propuesta1mx$residuals, type="Ljung-Box")
independencia_propuesta1mx # p-value = 0.9198 Los datos se distribuyen de forma independiente
independencia_propuesta1mx$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error.MX <- residuals(propuesta1mx)
plot(error.MX)

####MODELO ARIMA
propuesta1mx$coef

####PERU
# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(modeloPE)
tsdiag(modeloPE)

#####NORMALIDAD 

### test de Jarque Bera
## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorPE <- residuals(modeloPE)
hist(errorPE)
jb.norm.test(errorPE) # p-value = 0.3055 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(modeloPE$residuals);qqline(modeloPE$residuals)
normalidad <-shapiro.test(modeloPE$residuals)
normalidad # p-value = 0.3009 .... los datos se distribuyen de forma normal

########## Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_modeloPE<- Box.test(modeloPE$residuals, type="Ljung-Box")
independencia_modeloPE # p-value = 0.9198 Los datos se distribuyen de forma independiente
independencia_modeloPE$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error.PE <- residuals(modeloPE)
plot(error.PE)

####MODELO ARIMA
modeloPE$coef

#pronostico para la inflación para los últimos 6 meses

#Pronostico BRASIL
prediccionBR <- predict(modeloBR,n.ahead = 6)
prediccionBR
upper <- prediccionBR$pred + 1.96*prediccionBR$se
lower <- prediccionBR$pred - 1.96*prediccionBR$se
ts.plot(br.ts, prediccionBR$pred, col=c("black","red"), main='FORECAST Inflacion BR')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")

#Pronostico Chile
prediccionCL <- predict(modeloCL,n.ahead = 6)
prediccionCL
upper <- prediccionCL$pred + 1.96*prediccionCL$se
lower <- prediccionCL$pred - 1.96*prediccionCL$se
ts.plot(cl.ts, prediccionCL$pred, col=c("black","red"), main='FORECAST Inflacion CL')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")

#Pronostico Colombia
prediccionCO <- predict(modeloCO,n.ahead = 6)
prediccionCO
upper <- prediccionCO$pred + 1.96*prediccionCO$se
lower <- prediccionCO$pred - 1.96*prediccionCO$se
ts.plot(co.ts, prediccionCO$pred, col=c("black","red"), main='FORECAST Inflacion CO')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")

#Pronostico Mexico
prediccionMX <- predict(modeloMX,n.ahead = 6)
prediccionMX
upper <- prediccionMX$pred + 1.96*prediccionMX$se
lower <- prediccionMX$pred - 1.96*prediccionMX$se
ts.plot(mx.ts, prediccionMX$pred, col=c("black","red"), main='FORECAST Inflacion MX')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")

#Pronostico Perú
prediccionPE <- predict(propuestaPE,n.ahead = 6)
prediccionPE
upper <- prediccionPE$pred + 1.96*prediccionMX$se
lower <- prediccionPE$pred - 1.96*prediccionMX$se
ts.plot(pe.ts, prediccionPE$pred, col=c("black","red"), main='FORECAST Inflacion PE')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")