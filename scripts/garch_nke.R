#Librerias
rm(list = ls())
library(quantmod)
library(tseries)
library(forecast)
library(rugarch)
library(car)

#cargamos	los	datos	con	frecuencia	mensual	de	Intel	Corporation,	definiendo	la	serie	de	retornos	del	precio	al	cierre:

getSymbols("NKE", from = "2010-01-01", to = "2019-12-31", periodicity = "monthly")

NKE.ts <- ts(NKE$NKE.Close, frequency = 12, 
              start = c(2010,1))
autoplot(NKE.ts)

#Aplicamos una diferencia
NKE.rtn <- diff(log(NKE.ts))
autoplot(NKE.rtn)

#Modelo de ruido blanco
### Arima (0,0)

NKE.fit <- Arima(NKE.rtn, order = c(0,0,0))
NKE.fit

#### Diagnostico
checkresiduals(NKE.fit, plot = FALSE)

#Efecto ARCH
Box.test(residuals(NKE.fit)^2, lag=3)$p.value

Box.test(residuals(NKE.fit)^2, lag=6)$p.value

Box.test(residuals(NKE.fit)^2, lag=9)$p.value

Box.test(residuals(NKE.fit)^2, lag=12)$p.value

#Distribución de las innovaciones
shapiro.test(residuals(NKE.fit))

qqPlot(residuals(NKE.fit), distribution = "t", df = 5,
       line = "robust")

hist(residuals(NKE.fit))

#Rubro 2. Modelación ARCH(m)
pacf(residuals(NKE.fit)^2)
# m=1

#Estimación del modelo ARCH(3)

modelo.arch1 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
                           mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                           distribution.model = "sstd")
NKE.arch1.fit <- ugarchfit(spec = modelo.arch1, data = NKE.rtn)
NKE.arch1.fit

#Usando	errores	robustos	a	la	heterocedasticidad
round(NKE.arch5.fit@fit$robust.matcoef,6)


#Rubro 3. Estimación del modelo GARCH(1,1)

modelo.garch11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "sstd")
NKE.garch11.fit <- ugarchfit(spec = modelo.garch11, data = NKE.rtn)
NKE.garch11.fit

round(NKE.garch11.fit@fit$robust.matcoef, 5)

#Comparación del ajuste entre ARCH(1), GARCH(1,1)
##Criterios de información
infocriteria(NKE.arch3.fit)

infocriteria(NKE.garch11.fit)

persistence(NKE.garch11.fit)

halflife(NKE.garch11.fit)

plot(NKE.garch11.fit, which = 12)


spec.mgarch =
  ugarchspec(variance.model=list(model="sGARCH"),
             mean.model=list(armaOrder=c(0,0),
                             archm = TRUE),
             distribution.model = "sstd")
NKE.fit.mgarch = ugarchfit(data = NKE.rtn,
                           spec=spec.mgarch)
round(NKE.fit.mgarch@fit$robust.matcoef, 5)

fcast.boot <- ugarchboot(NKE.garch11.fit, method = "Partial",
                         n.ahead = 5,
                         n.bootpred = 100)

fcast.boot


#Modelo EGARCH
spec.egarch = ugarchspec(
  variance.model=list(model="eGARCH"),
  mean.model=list(armaOrder=c(0,0)),
  distribution.model = "sstd")
NKE.fit.egarch = ugarchfit(data = NKE.rtn,
                           spec=spec.egarch)
NKE.fit.egarch

#gráfica	del	impacto	de	innovaciones
plot(NKE.fit.egarch, which = 12)


persistence(NKE.fit.egarch)

halflife(NKE.fit.egarch)

#Vida media
spec.egarch.m = ugarchspec(
  variance.model=list(model="eGARCH"),
  mean.model=list(armaOrder=c(0,0), archm=TRUE),
  distribution.model = "sstd")
NKE.fit.egarch.m = ugarchfit(data = NKE.rtn,
                             spec=spec.egarch.m)
round(NKE.fit.egarch.m@fit$robust.matcoef,5)

#Apendice
### Modelo ARCH
NKE.arch5.fit
### Modelo GARCH
NKE.garch11.fit
###Pronóstico	GARCH(1,1)
fcast.boot <- ugarchboot(NKE.garch11.fit, method = "Partial",
                         n.ahead = 5,
                         n.bootpred = 100)
fcast.boot
### Modelo EGARCH(1,1)
NKE.fit.egarch
