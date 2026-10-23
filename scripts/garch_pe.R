#Librerias
rm(list = ls())
library(quantmod)
library(tseries)
library(forecast)
library(rugarch)
library(car)

#cargamos	los	datos	con	frecuencia	mensual
getSymbols("HON", from = "2010-01-01", to = "2019-12-31", periodicity = "monthly")

hon.ts<-ts(HON$HON.Close, start = 2010, frequency = 12)
autoplot(hon.ts)

#Eliminamos la tendencia
HON.rtn <- diff(log(hon.ts))
autoplot(HON.rtn)

#Modelo de ruido blanco
### Arima (0,0)

HON.fit <- Arima(HON.rtn, order = c(0,0,0))
HON.fit

#### Diagnostico
checkresiduals(HON.fit, plot = FALSE)

#Efecto ARCH
Box.test(residuals(HON.fit)^2, lag=3)$p.value

Box.test(residuals(HON.fit)^2, lag=6)$p.value

Box.test(residuals(HON.fit)^2, lag=9)$p.value

Box.test(residuals(HON.fit)^2, lag=12)$p.value

#Distribución de las innovaciones
shapiro.test(residuals(HON.fit))

qqPlot(residuals(HON.fit), distribution = "t", df = 5,
       line = "robust")

hist(residuals(HON.fit))

#Rubro 2. Modelación ARCH(m)
pacf(residuals(HON.fit)^2)
# m=5

#Estimación del modelo ARCH(5)

modelo.arch5 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(5, 0)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "sstd")
Hon.arch5.fit <- ugarchfit(spec = modelo.arch5, data = HON.rtn)
Hon.arch5.fit

#Usando	errores	robustos	a	la	heterocedasticidad
round(Hon.arch5.fit@fit$robust.matcoef,6)


#Rubro 3. Estimación del modelo GARCH(1,1)

modelo.garch11 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "sstd")
HON.garch11.fit <- ugarchfit(spec = modelo.garch11, data = HON.rtn)
HON.garch11.fit

round(HON.garch11.fit@fit$robust.matcoef, 5)

#Comparación del ajuste entre ARCH(5), GARCH(1,1)
##Criterios de información
infocriteria(Hon.arch5.fit)

infocriteria(HON.garch11.fit)

persistence(HON.garch11.fit)

halflife(HON.garch11.fit)

plot(HON.garch11.fit, which = 12)


spec.mgarch =
  ugarchspec(variance.model=list(model="sGARCH"),
             mean.model=list(armaOrder=c(0,0),
                             archm = TRUE),
             distribution.model = "sstd")
HON.fit.mgarch = ugarchfit(data = HON.rtn,
                            spec=spec.mgarch)
round(HON.fit.mgarch@fit$robust.matcoef, 5)

fcast.boot <- ugarchboot(HON.garch11.fit, method = "Partial",
                         n.ahead = 5,
                         n.bootpred = 100)

fcast.boot


#Modelo EGARCH
spec.egarch = ugarchspec(
  variance.model=list(model="eGARCH"),
  mean.model=list(armaOrder=c(0,0)),
  distribution.model = "sstd")
HON.fit.egarch = ugarchfit(data = HON.rtn,
                            spec=spec.egarch)
HON.fit.egarch

#gráfica	del	impacto	de	innovaciones
plot(HON.fit.egarch, which = 12)


persistence(HON.fit.egarch)

halflife(HON.fit.egarch)

#Vida media
spec.egarch.m = ugarchspec(
  variance.model=list(model="eGARCH"),
  mean.model=list(armaOrder=c(0,0), archm=TRUE),
  distribution.model = "sstd")
HON.fit.egarch.m = ugarchfit(data = HON.rtn,
                              spec=spec.egarch.m)
round(HON.fit.egarch.m@fit$robust.matcoef,5)

#Apendice
### Modelo ARCH
Hon.arch5.fit
### Modelo GARCH
HON.garch11.fit
###Pronóstico	GARCH(1,1)
fcast.boot <- ugarchboot(HON.garch11.fit, method = "Partial",
                         n.ahead = 5,
                         n.bootpred = 100)
fcast.boot
### Modelo EGARCH(1,1)
HON.fit.egarch
