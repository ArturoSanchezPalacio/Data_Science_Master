### Práctica Final. Gestión del Riesgo Operativo
## Arturo Sánchez Palacio
## 6 de Mayo de 2019

#Las siguientes líneas de código fundamentan el informe adjunto sobre pérdidas de la compañía Copenhagen Reinsurance.

# En primer lugar se cargan las bibliotecas que se usarán para esta práctica:

library(moments)
library(actuar)
library(fitdistrplus)
library(ggplot2)
library(MASS)
library(CASdatasets)
library(car)
library(dplyr)
library(lubridate)
library(OpVaR)
library(QRM)

# Tras esto se procede a la carga de los datos. Visualizamos que los datos se han cargado de manera correcta:

data(danishuni)
head(danishuni)



## ANÁLISIS EXPLORATORIO

## Creación del nuevo dataframe:

datos_agregados <- danishuni %>% group_by(dias = floor_date(danishuni$Date, "day")) %>% summarize(siniestros = n(), Loss = sum(Loss))

#Se calculan las principales medidas de posición y se grafica la distribución:

summary(datos_agregados)
var(datos_agregados$siniestros)
var(datos_agregados$Loss)
quantile(datos_agregados$Loss,seq(0,1, 0.20)) 
quantile(datos_agregados$Loss,seq(0.9,1, 0.01))
hist(datos_agregados$Loss, pch = 5, breaks = 50, prob = TRUE, main = "Pérdidas",
     xlab = " Importe", ylab = "Frecuencia")

### Ajuste de severidad

# Se comprueban tres distribuciones, una gamma, una Pareto, una mixtura de ambas y una Burr. Para ello almacenamos las pérdidas en una variable:

perdidas <- danishuni$Loss
ajuste_gamma <- fitdist(perdidas, "gamma", lower = 0)
ajuste_pareto <- fitdist(perdidas, "pareto", start = list(shape = 2, scale = 2), lower = 0)

# Para ajustar la mixtura es necesario definir la función de densidad y probabilidad:

dmixgampar <- function(x, prob, nu, lambda, alpha, theta)
  prob*dgamma(x, nu, lambda) + (1 - prob)*dpareto(x, alpha, theta)

pmixgampar <- function(q, prob, nu, lambda, alpha, theta)
  prob*pgamma(q, nu, lambda) + (1 - prob)*ppareto(q, alpha, theta)

ajuste_mixtura <- fitdist(perdidas, "mixgampar", start = list(prob = 1/2, nu = 1, lambda = 1, alpha = 2, theta = 2), lower = 0)

cbind(SINGLE = c(NA, ajuste_gamma$estimate, ajuste_pareto$estimate), MIXTURE = ajuste_mixtura$estimate)

ajuste_burr <- fitdist(perdidas, "burr", start = list(shape1 = 2, shape2 = 2, scale = 2), lower = c(0.1,1/2, 0))

ajuste_burr$estimate

# Se puede emplear la función cdfcommp para comparar qué distribución ajusta mejor la severidad:

cdfcomp(list(ajuste_gamma, ajuste_pareto, ajuste_mixtura, ajuste_burr), xlogscale = TRUE, datapch = ".", 
        datacol = "red", fitcol = "black", fitlty = 2:5, legendtext = c("gamma","Pareto","Par-gam","Burr"),
        main = "Comparación de ajustes")

#Se observa que la que mejor ajusta con diferencia es una Burr.

# Visualizamos el PPplot y el QQplot para esta distribución:

ppcomp(list(ajuste_burr), xlogscale = TRUE, ylogscale = TRUE, fitcol = "black", main = "PP-plot Danishuni",
       legendtext = c("Burr"), fitpch = 1)

qqcomp(list(ajuste_burr), xlogscale = TRUE, ylogscale = TRUE, fitcol = "black", main = "QQ-plot Danishuni",
       legendtext = c("Burr"), fitpch = 1)

## Ajuste de frecuencia

# En primer lugar agrupamos el número de sucesos por día:

siniestros_dia <- danishuni %>% group_by(dias = floor_date(danishuni$Date, "day")) %>% summarize(siniestros = n())

frecuencia <- siniestros_dia[,2]
(ajuste_poisson <- fitdist(frecuencia$siniestros, "pois"))
(ajuste_bin <- fitdist(frecuencia$siniestros, "nbinom"))

gofstat(list(ajuste_poisson, ajuste_bin), chisqbreaks = c(0:4, 9), discrete = TRUE,
        fitnames = c("Poisson", "Binomial Negativa"))

# La Poisson ajusta bien (el p-valor es significativo) lo mismo ocurre para la binomial negativa. Se toma la decisión en base a los criterios de información:
# Criterio de Parsimonia

## Función de pérdida agregada

# La frecuencia sigue una distribución de Poisson de parámetro 1.3173 y la severidad una Burr de parámetros 0.1, 14.44 y 1.085:

parsev <- ajuste_burr$estimate
parfreq <- 1.3173
meansev <- mburr(1, parsev[1], parsev[2], parsev[3])
varsev <- mburr(2, shape1 = parsev[1], shape2 = parsev[2], scale = parsev[3]) - meansev^2
varsev <- 85.08001
skewsev <- (mburr(3, parsev[1], parsev[2], parsev[3]) - 3*meansev*varsev - meansev^3)/varsev^(3/2)
skewsev <- 21.09383
meanfreq <- varfreq <- parfreq[1]
skewfreq <- 1/sqrt(parfreq[1])
meanagg <- meanfreq * meansev
varagg <- varfreq * (varsev + meansev^2)
skewagg <- (skewfreq*varfreq^(3/2)*meansev^3 + 3*varfreq*meansev*varsev  + meanfreq*skewsev*varsev^(3/2))/varagg^(3/2)                      
agregada <- aggregateDist("simulation", model.freq = expression(y = rpois(parfreq)), 
                          model.sev = expression(y = rburr(parsev[1], parsev[2], parsev[3])), nb.simul = 1000)                 

Fs.n <- aggregateDist("normal", moments = c(meanagg, varagg))
Fs.np <- aggregateDist("npower", moments = c(meanagg, varagg, skewagg))
plot(seq(0,100), agregada(seq(0,100)), type = "l", xlim = c(0, 100))  
lines(seq(0,100), Fs.n(seq(0,100)), lty = 3)
                     


plot(seq(0,100), Fs.n(seq(0,100)), type = "l", xlim = c(0, 100))  

## Cálculo del VAR

# Empleamos una generalizada de Pareto

cuantil <- 0.99
u <- quantile(danish, cuantil,names = FALSE)
fit.danish <- fit.GPD(danish, threshold = u)
(xi.hat.danish <- fit.danish$par.ses[["xi"]])
(beta.hat.danish <- fit.danish$par.ses[["beta"]])

#Calculamos  el exceso:

loss.excess <- danish[danish > u] - u

n.relative.excess <- length(loss.excess)/length(danish)

#Se calcula el VaR

(VaR.gpd <- u + (beta.hat.danish/xi.hat.danish) * 
    (((1 - cuantil)/n.relative.excess)^(-xi.hat.danish) - 
       1))

# Se calcula el VaR condicional:

(ES.gpd <- (VaR.gpd + beta.hat.danish - 
              xi.hat.danish * u)/(1 - xi.hat.danish))
