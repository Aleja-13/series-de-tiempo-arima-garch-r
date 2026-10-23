#### INFLACIÓN de series mensuales de 5 países de la región: Chile, Colombia, Brasil, México y Perú. 

#Librerias
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

#### Importar la base de datos
Inflacion <- read.csv2("datos/Inflacion_5paises.csv")

# Creamos los datos por pais

br <- as.matrix(Inflacion$ï..Brasil)
cl <- as.matrix(Inflacion$Chile)
co <- as.matrix(Inflacion$Colombia)
mx <- as.matrix(Inflacion$MÃ.xico)
pe <- as.matrix(Inflacion$PerÃº)

#Analizamos las series de tiempo para cada pais
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


#######BRASIL#######

ggtsdisplay(br.ts, main = "Inflacion en Brasil") 

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(br.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA

# PHILLIP PERRON

pp.test(br.ts,alternative = "stationary")      #  p-value = 0.99 ... ES ESTACIONARIA

# DIFERENCIAS
ndiffs(br.ts)

# 2da DIFERENCIA

br_2d <- diff(br.ts, differences = 2)

autoplot(br_2d, frequency =12, xlab = "Tiempo", ylab = "Inflacion en BRASIL", main = "Comportamiento de la inflacion en BRASIL")
adf.test(br_2d,alternative = "stationary")
pp.test(br_2d,alternative = "stationary") 
#p-value = 0.01 es estacionaria

ggtsdisplay(br_2d, main = "Inflacion")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE ES ESTACIONARIA

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(br_2d,alternative = "stationary")     #  p-value = 0.01 ... ES ESTACIONARIA

# PHILLIP PERRON

pp.test(br_2d,alternative = "stationary")      #  p-value = 0.01 ... ES ESTACIONARIA


# INTERPRETACION: EN SU SEGUNDA DIFERENCIA YA ESTA ESTACIONARIA

# MODELOS ARIMA
propuestaBR <- arima(br.ts, order = c(1,2,1), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuestaBR
coeftest(propuestaBR)
propuestaBR$aic 


# FUNCION AUTO ARIMA
ModeloBR <- auto.arima(br.ts,d=2, trace = TRUE)
ModeloBR

### BEST MODEL: ARIMA(1,2,1)

############################## Validación ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(propuestaBR)
tsdiag(propuestaBR)

############################## NORMALIDAD #############################

### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorBR <- residuals(propuestaBR)
par(mfrow=c(1, 1))
hist(errorBR)
jb.norm.test(errorBR) # p-value = 0.038 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(propuestaBR$residuals);qqline(propuestaBR$residuals)
normalidad <-shapiro.test(propuestaBR$residuals)
normalidad # p-value = 0.1656 .... los datos se distribuyen de forma normal

########## Heterocedasticidad 
# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_propuestaBR<- Box.test(propuestaBR$residuals, type="Ljung-Box")
independencia_propuestaBR # p-value = 0.7389574 Los datos se distribuyen de forma independiente
independencia_propuestaBR$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
errorBR <- residuals(propuestaBR)
plot(errorBR)

######################### MODELO ARIMA
propuestaBR$coef

#Pronostico
prediccionBR <- predict(propuestaBR,n.ahead = 6)
prediccionBR
upper <- prediccionBR$pred + 1.96*prediccionBR$se
lower <- prediccionBR$pred - 1.96*prediccionBR$se
ts.plot(br.ts, prediccionBR$pred, col=c("black","red"), main='FORECAST Inflacion')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")

#######CHILE#######
cl.ts <- ts(log(cl), frequency = 12, star = c(2010,7), end = c(2020,12))
ggtsdisplay(cl.ts, main = "Inflacion en Chile")  

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA
##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(cl.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA

# PHILLIP PERRON

pp.test(cl.ts,alternative = "stationary")      #  p-value = 0.99 ... ES ESTACIONARIA


# DIFERENCIAS
ndiffs(cl.ts)

# 1ers DIFERENCIA

cl_1d <- diff(cl.ts, differences = 1)

autoplot(cl_1d, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Chile", main = "Comportamiento de la inflacion en Chile")
adf.test(cl_1d,alternative = "stationary")
pp.test(cl_1d,alternative = "stationary") 
#p-value = 0.01 es estacionaria

ggtsdisplay(cl_1d, main = "Inflacion en Chile")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE ES ESTACIONARIA


# INTERPRETACION: EN SU primera DIFERENCIA YA ESTA ESTACIONARIA

# MODELOS ARIMA
propuestacl <- arima(cl.ts, order = c(1,1,0), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuestacl
coeftest(propuestacl)
propuestacl$aic 

# FUNCION AUTO ARIMA
Modelocl <- auto.arima(cl.ts,d=1, trace = TRUE)
Modelocl

### BEST MODEL: ARIMA(0,1,1)

############################## Validación ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(Modelocl)
tsdiag(Modelocl)

##########NORMALIDAD 

### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
error1 <- residuals(Modelocl)
par(mfrow=c(1, 1))
hist(error1)
jb.norm.test(error1) # p-value = 0.699 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(Modelocl$residuals);qqline(Modelocl$residuals)
normalidad <-shapiro.test(Modelocl$residuals)
normalidad # p-value = 0.4326 .... los datos se distribuyen de forma normal

#### Heterocedasticidad

# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_Modelocl<- Box.test(Modelocl$residuals, type="Ljung-Box")
independencia_Modelocl # p-value = 0.9191 Los datos se distribuyen de forma independiente
independencia_Modelocl$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
errorCL <- residuals(Modelocl)
plot(errorCL)


######################### MODELO ARIMA ##########################
Modelocl$coef

#Pronostico
prediccionCL <- forecast(Modelocl,n.ahead = 6)
prediccionCL

autoplot(forecast(Modelocl), main='FORECAST Inflacion CL')

#######Colombia#######
co.ts <- ts(log(co), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(co.ts)
ggtsdisplay(co.ts, main = "Inflacion en Colombia")  

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(co.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA

# PHILLIP PERRON

pp.test(co.ts,alternative = "stationary")      #  p-value = 0.99 ... ES ESTACIONARIA


# DIFERENCIAS
ndiffs(co.ts)

# 1ers DIFERENCIA

co_1d <- diff(co.ts, differences = 1)

autoplot(co_1d, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Colombia", main = "Comportamiento de la inflacion en Colombia")
adf.test(co_1d,alternative = "stationary")
pp.test(co_1d,alternative = "stationary") 
#p-value = 0.01 es estacionaria

ggtsdisplay(co_1d, main = "Inflacion en Colombia")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE ES ESTACIONARIA


# INTERPRETACION: EN SU SEGUNDA DIFERENCIA YA ESTA ESTACIONARIA

# MODELOS ARIMA
propuestaCO <- arima(co.ts, order = c(1,2,1), seasonal = list(order = c(1,0,1), period = 4), method = 'ML')
propuestaCO
coeftest(propuestaCO)
propuestaCO$aic 

# FUNCION AUTO ARIMA
ModeloCO <- auto.arima(co.ts,d=1, trace = TRUE)
ModeloCO

### BEST MODEL: ARIMA(1,1,0)

############################## Validación ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(ModeloCO)
tsdiag(ModeloCO)

############################## NORMALIDAD #############################

### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorCO <- residuals(propuestaCO)
par(mfrow=c(1, 1))
hist(errorCO)
jb.norm.test(errorCO) # p-value = 0.0765 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(propuestaCO$residuals);qqline(propuestaCO$residuals)
normalidad <-shapiro.test(propuesta11$residuals)
normalidad # p-value = 0.4326 .... los datos se distribuyen de forma normal

#### Heterocedasticidad

# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_propuestaCO<- Box.test(propuestaCO$residuals, type="Ljung-Box")
independencia_propuestaCO# p-value =  0.9856 Los datos se distribuyen de forma independiente
# independencia_Propuesta1$p.value

# Ruido blanco (es parte del test de heterocedasticidad)
error1 <- residuals(propuestaCO)
plot(errorCO)


######################### MODELO ARIMA ##########################
propuestaCO$coef

#Pronostico
prediccionCO <- predict(propuestaCO,n.ahead = 6)
prediccionCO
upper <- prediccionCO$pred + 1.96*prediccionCO$se
lower <- prediccionCO$pred - 1.96*prediccionCO$se
ts.plot(co.ts, prediccionCO$pred, col=c("black","red"), main='FORECAST Inflacion')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")

#######MEXICO#######
mx.ts <- ts(log(mx), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(mx.ts)
ggtsdisplay(mx.ts, main = "Inflacion en México")  

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(co.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA

# PHILLIP PERRON

pp.test(co.ts,alternative = "stationary")      #  p-value = 0.99 ... ES ESTACIONARIA


# DIFERENCIAS
ndiffs(co.ts)

# 1ers DIFERENCIA

co_1d <- diff(co.ts, differences = 1)

autoplot(co_1d, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Mexico", main = "Comportamiento de la inflacion en Mexico")
adf.test(co_1d,alternative = "stationary")
pp.test(co_1d,alternative = "stationary") 
#p-value = 0.01 es estacionaria

ggtsdisplay(co_1d, main = "Inflacion en Mexico")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE ES ESTACIONARIA


# INTERPRETACION: EN SU SEGUNDA DIFERENCIA YA ESTA ESTACIONARIA

# MODELOS ARIMA
propuestaMX <- arima(mx.ts, order = c(2,1,1), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuestaMX
coeftest(propuestaMX)
propuestaMX$aic 

# FUNCION AUTO ARIMA
ModeloMX <- auto.arima(mx.ts,d=1, trace = TRUE)
ModeloMX

### BEST MODEL: ARIMA(1,1,0)

############################## Validación ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(ModeloMX)
tsdiag(ModeloMX)

############################## NORMALIDAD #############################

### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorMX <- residuals(ModeloMX)
par(mfrow=c(1, 1))
hist(errorMX)
jb.norm.test(errorMX) # p-value = 2.2e-16 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(ModeloMX$residuals);qqline(ModeloMX$residuals)
normalidad <-shapiro.test(ModeloMX$residuals)
normalidad # p-value = 2.2e-16 .... los datos se distribuyen de forma normal

#### Heterocedasticidad

# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_ModeloMX<- Box.test(ModeloMX$residuals, type="Ljung-Box")
independencia_ModeloMX # p-value =  0.9166 Los datos se distribuyen de forma independiente

# Ruido blanco (es parte del test de heterocedasticidad)
error1 <- residuals(ModeloMX)
plot(errorMX)


######################### MODELO ARIMA ##########################
ModeloMX$coef

#Pronostico
prediccionMX <- predict(propuestaMX,n.ahead = 6)
prediccionMX
upper <- prediccionMX$pred + 1.96*prediccionMX$se
lower <- prediccionMX$pred - 1.96*prediccionMX$se
ts.plot(mx.ts, prediccionMX$pred, col=c("black","red"), main='FORECAST Inflacion')
lines(upper, col="blue", lty="solid")
lines(lower, col="blue", lty="solid")


#######PERU#######
pe.ts <- ts(log(pe), frequency = 12, star = c(2010,7), end = c(2020,12))
autoplot(pe.ts)
ggtsdisplay(pe.ts, main = "Inflacion en Perú")  

# TEST ESTACIONARIEDAD

# REGLA DE DECISION: P-VALUE < 0.05 ... ES ESTACIONARIA

##Ho : LA SERIE NO ES ESTACIONARIA:  TIENE RAIZ UNITARIA
##H1 : LA SERIE ES ESTACIONARIA:     NO TIENE RAIZ UNITARIA

# DICKEY FULLER

adf.test(pe.ts,alternative = "stationary")     #  p-value = 0.99 ... NO ES ESTACIONARIA

# PHILLIP PERRON

pp.test(pe.ts,alternative = "stationary")      #  p-value = 0.99 ... ES ESTACIONARIA


# DIFERENCIAS
ndiffs(pe.ts)

# 1ers DIFERENCIA

pe_1d <- diff(pe.ts, differences = 1)

autoplot(pe_1d, frequency =12, xlab = "Tiempo", ylab = "Inflacion en Peru", main = "Comportamiento de la inflacion en Peru")
adf.test(pe_1d,alternative = "stationary")
pp.test(pe_1d,alternative = "stationary") 
#p-value = 0.01 es estacionaria

ggtsdisplay(pe_1d, main = "Inflacion en Peru")      # SOBRESALES DE LA BAMDA DER SIGNIFICANCIA, LA SERIE ES ESTACIONARIA


# INTERPRETACION: EN SU SEGUNDA DIFERENCIA YA ESTA ESTACIONARIA

# MODELOS ARIMA
propuestaPE <- arima(pe.ts, order = c(0,1,1), seasonal = list(order = c(1,0,1), period = 12), method = 'ML')
propuestaPE
coeftest(propuestaPE)
propuestaPE$aic 

# FUNCION AUTO ARIMA
ModeloPE <- auto.arima(pe.ts,d=1, trace = TRUE)
ModeloPE

### BEST MODEL: ARIMA(2,1,0)

############################## Validación ################################################################################

# Revision de los residuos (DIAGNOSTICO VISUAL)
checkresiduals(ModeloPE)
tsdiag(ModeloPE)

############################## NORMALIDAD #############################

### test de Jarque Bera

## Criterio de decisión: si p > 0.05 distribución normal
#H0: La muestra proviene de una distribución normal.
#H1: La muestra no proviene de una distribución normal.

# test de Jarque Bera
errorPE <- residuals(ModeloPE)
par(mfrow=c(1, 1))
hist(errorPE)
jb.norm.test(errorPE) # p-value = 2.2e-16 Los datos no se distribuyen de forma normal.

# Test de Shapiro-Wilk

# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente

qqnorm(ModeloPE$residuals);qqline(ModeloPE$residuals)
normalidad <-shapiro.test(ModeloPE$residuals)
normalidad # p-value = 0.5689 .... los datos se distribuyen de forma normal

#### Heterocedasticidad

# Test de Ljung-Box
# Criterio de decisión: si p-value > 0.05 Los datos se distribuyen de forma independiente
#H0: Los residuos se distribuyen de forma independiente
#H1: Los residuos no se distribuyen de forma independiente

independencia_ModeloPE<- Box.test(ModeloPE$residuals, type="Ljung-Box")
independencia_ModeloPE # p-value =  0.9166 Los datos se distribuyen de forma independiente

# Ruido blanco (es parte del test de heterocedasticidad)
errorPE <- residuals(ModeloPE)
plot(errorPE)


######################### MODELO ARIMA ##########################
ModeloPE$coef

#Pronostico
prediccionPE <- forecast(ModeloPE, h=6)
prediccionPE
autoplot(prediccionPE)
