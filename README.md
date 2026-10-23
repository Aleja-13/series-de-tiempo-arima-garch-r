# Series de tiempo con ARIMA y GARCH en R

Modelado y pronóstico de series económicas y financieras con la metodología Box-Jenkins: pruebas de estacionariedad, ajuste ARIMA, diagnóstico de residuos y, en los casos financieros, volatilidad con GARCH.

## Qué encontrarás aquí

| Archivo | Qué hace |
|---|---|
| `scripts/arima_crecimiento_pib.R` | ARIMA del crecimiento del PIB: prueba de raíz unitaria (ADF), `auto.arima`, normalidad de residuos y prueba Ljung-Box. |
| `scripts/arima_tipo_de_cambio.R` | ARIMA para la variación del tipo de cambio, con la misma secuencia de pruebas y diagnóstico. |
| `scripts/arima_garch_acciones.R` | ARIMA y GARCH sobre precios de acciones descargados con `quantmod`. |
| `scripts/garch_nke.R` | Modelo GARCH (`rugarch`) para el retorno de una acción (NKE), con descarga automática de precios. |
| `scripts/garch_pe.R` | Mismo flujo ARIMA-GARCH aplicado a otra serie bursátil. |
| `scripts/inflacion_5_paises.R` | Comparación de la inflación mensual de Chile, Colombia, Brasil, México y Perú: estacionariedad, ARIMA y pronóstico por país. |
| `scripts/inflacion_5_paises_parte1.R` | Versión por partes del análisis de inflación de los cinco países. |

## Cómo ejecutarlo

1. Descarga o clona este repositorio y abre RStudio.
2. Fija la carpeta del repositorio como directorio de trabajo (`Session > Set Working Directory`).
3. Los scripts leen los datos desde la carpeta `datos/`. Instala los paquetes que falten con `install.packages("nombre")`.

**Paquetes usados:** `MASS`, `TSA`, `astsa`, `broom`, `car`, `dplyr`, `dynlm`, `forecast`, `foreign`, `fpp2`, `funModeling`, `ggfortify`, `ggplot2`, `gridExtra`, `kableExtra`, `knitr`, `lmtest`, `lubridate`, `mFilter`, `mlogit`, `nlme`, `normtest`, `parallel`, `quantmod`, `readxl`, `rugarch`, `stats`, `tidyr`, `tidyverse`, `timsac`, `tseries`, `urca`, `vars`, `xts`.

## Datos

Las bases de PIB mensual, tipo de cambio USD/PEN e inflación no están incluidas todavía. Los scripts de acciones (`NKE`, `PE`, `Modelos ARIMA_1`) descargan los precios por sí solos.

## Autoría

Código y análisis de Alejandra, con fines educativos y de portafolio. Más sobre mi trabajo en analítica de datos: [alejadata.pro](https://alejadata.pro/).
