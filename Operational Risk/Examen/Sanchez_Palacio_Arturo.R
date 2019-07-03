##### EXAMEN GESTIÓN RIESGO OPERATIVO. MÁSTER DATA SCIENCE CUNEF. 5-VI-2019.

## Arturo Sánchez Palacio

# Las bibliotecas a utilizar serán las siguientes:

library(moments)
library(ggplot2)
library(fitdistrplus)
library(actuar)
# En primer lugar se procede a la carga de datos:

frecuencia <- read.csv("frecuencias_0619.csv")
severidad <- read.csv("cuantias_0619.csv", sep = "\n", dec = ",")

# Se realiza un breve análisis exploratorio a fin de familiarizarnos con los datos:

## ANÁLISIS EXPLORATORIO

## Frecuencia

summary(frecuencia)
var(frecuencia)
table(frecuencia)
sum(frecuencia)

# El número de personas que sobrepasa al mes varía bastante (entre 11 y 147. La varianza se dispara).
# En total durante estos cinco años 3432 personas han excedido la esperanza de vida estimada.

skewness(frecuencia)

# Es positivo luego la asimetría es positiva, es decir,los valores se concentran a la izquierda.

kurtosis(frecuencia)

# Es mayor que 3 luego es leptocúrtica, sus colas son más pesadas que la normal (se acumulan valores extremos)

# Se exploran sus cuantiles:

quantile(frecuencia$x,seq(0,1, 0.20)) 
quantile(frecuencia$x, seq(0.9,1, 0.01))

# Por último se presenta el histograma de la distribución de frecuencia:

hist(frecuencia$x, pch = 20, breaks = 25, prob = TRUE, main = "Histograma frecuencia",
     xlab = "Personas que superan su esperanza de vida", ylab = "Frecuencia")

# Análogamente para severidad.

## Severidad

summary(severidad)
var(severidad)
sum(severidad)

# La cuantía perdida según el mes varía bastante (entre 4576 y 468422. La varianza se dispara).
# En total durante estos cinco años se han perdido 2656305 unidades monetarias.

skewness(severidad)

# Es positivo luego la asimetría es positiva, es decir,los valores se concentran a la izquierda.

kurtosis(severidad)

# Es mayor que 3 luego es leptocúrtica, sus colas son más pesadas que la normal (se acumulan valores extremos)

# Se exploran sus cuantiles:

quantile(severidad$x,seq(0,1, 0.20)) 
quantile(severidad$x, seq(0.9,1, 0.01))

# Por último se presenta la gráfica de la distribución de severidad:

ggplot(severidad) + geom_density(aes(x), fill = 'red') +
  xlab('Severidad') + ylab('Densidad')

## Ajuste de frecuencia:

# A continuación se intenta ajustar la distribución de frecuencia:

# Opción A. Poisson

(fpois <- fitdist(frecuencia$x, "pois"))

# El resultado es malísimo. Es evidente por el tipo de distribución que no se ajusta por una Poisson.

# Opción B. Binomial negativa

(fnbinom <- fitdist(frecuencia$x, "nbinom"))

(ajuste_frec <- gofstat(list(fpois, fnbinom), chisqbreaks = c(0:4, 9), discrete = TRUE,
        fitnames = c("Poisson", "Binomial Negativa")))

# Los criterios de información nos indican que el mejor ajuste entre las dos es una binomial negativa.

plot(fpois)
plot(fnbinom)

# Ninguna de las dos distribuciones ajusta del todo bien la frecuencia pero de elegir nos quedaríamos con la binomial negativa.


## Severidad

# Gamma

fgam <- fitdist(severidad$x, "gamma", lower = 0)
summary(fgam)
plot(fgam)

# Pareto

fpar <- fitdist(severidad$x, "pareto", start = list(shape = 1, scale = 2.2))

summary(fpar)
plot(fpar)

# Burr
fburr <- fitdist(severidad$x, "burr", method = 'mle', start = list(shape1 = 3, shape2 = 2, scale = 1),
                 lower = c(0, 0, 0))
summary(fburr)
plot(fburr)

# Comparamos la bondad de ajuste de las tres distribuciones:

# Pruebas de bondad de ajuste
(ajuste_sev <- gofstat(list(fgam, fpar, fburr), chisqbreaks = c(14:23), discrete = FALSE,
                       fitnames = c("GAMM", "Pareto", "Burr")))


comparacion = cdfcomp(list(fgam, fpar, fburr), xlogscale = TRUE, xlab = "Cuantía",
              ylab = "Probabilidad", datapch = ".",
              datacol = "red", fitcol = c("pink", "green", "blue"), 
              fitlty = 2:5, legendtext = c("Gamma", "Pareto", "Burr"),
              main = "Ajuste severidad", plotstyle = "ggplot")
comparacion

# El mejor ajuste es el obtenido por la Burr.

shapea = fburr$estimate[1]
shapeb = fburr$estimate[2]
scale = fburr$estimate[3]

### Análisis de los valores extremos

#Se emplea el método de Block Máxima. Para estimar el umbral se comprueba el vlaor máximo de cada año:

max(severidad$x[1:12])
max(severidad$x[13:24])
max(severidad$x[25:36])
max(severidad$x[37:48])
max(severidad$x[49:60])

# Se fija el umbral en 118000.

umbral <- 118000

exceso <- severidad[severidad > umbral]
exceso

pasan <- length(exceso)

# Hay cuatro casos que superan el umbral.

prob <- 1 - base::rank(exceso)/(pasan + 1)  #rank ofrece el n de orden
prob

# Se calcula la función de distribución acumulada:

alfa <- -cov(log(exceso), log(prob)) / var(log(exceso))
alfa

x = seq(umbral, max(exceso), length = 100) #divide de u a max() 100 interv.
y = (x / umbral)^(-alfa)
lines(x, y)


prob <- rank(exceso) / (pasan + 1)
plot(exceso, prob, log = "x", xlab = "Excesos", ylab = "Probabilidades de no excesos")
y = 1 - (x / umbral)^(-alfa)
lines(x, y)

# Se procede a la estimación reutilizando el código visto en las prácticas.

#Distribucion valores extremos generalizados (GEV) 
nllik.gev <- function(par, data){
  mu <- par[1]
  sigma <- par[2]
  xi <- par[3]
  if ((sigma <= 0) | (xi <= -1))
    return(1e6)
  n <- length(data)
  if (xi == 0)
    n * log(sigma) + sum((data - mu) / sigma) +
    sum(exp(-(data - mu) / sigma))
  else {
    if (any((1 + xi * (data - mu) / sigma) <= 0))
      return(1e6)
    n * log(sigma) + (1 + 1 / xi) *
      sum(log(1 + xi * (data - mu) / sigma)) +
      sum((1 + xi * (data - mu) / sigma)^(-1/xi))
  }
}

# GEV
sigma.start <- sqrt(6) * sd(exceso) / pi
mu.start <- mean(exceso) + digamma(1) * sigma.start
fit.gev <- nlm(nllik.gev, c(mu.start, sigma.start, 0),
               hessian = TRUE, data = exceso)
fit.gev

fit.gev$estimate 

# Generalizada de Pareto

nllik.gp <- function(par, u, data){
  tau <- par[1]
  xi <- par[2]
  if ((tau <= 0) | (xi < -1))
    return(1e6)
  m <- length(data)
  if (xi == 0)
    m * log(tau) + sum(data - u) / tau
  else {
    if (any((1 + xi * (data - u) / tau) <= 0))
      return(1e6)
    m * log(tau) + (1 + 1 / xi) *
      sum(log(1 + xi * (data - u) / tau))
  }
}

# Obtención de los parámetros PARETO
tau.start <- mean(exceso) - umbral 
fit.gp <- nlm(nllik.gp, c(tau.start, 0), u = umbral, hessian = TRUE,
              data = exceso)
fit.gp
fit.gp$estimate 
sqrt(diag(solve(fit.gp$hessian))) 






qqgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  prob <- 1:m / (m + 1)
  x.hat <- u + tau / xi * ((1 - prob)^-xi - 1)
  ylim <- xlim <- range(x.hat, excess)
  plot(sort(excess), x.hat, xlab = "Cuantiles en la muestra",
       ylab = "Cuantiles ajustados", xlim = xlim, ylim = ylim)
  abline(0, 1, col = "grey")
}

qqgpd(severidad, umbral, fit.gp$estimate[1], fit.gp$estimate[2]) 


#P-P Plot para la Dist. Generalizada de Pareto (DGP)
ppgpd <- function(data, u, tau, xi){
  excess <- data[data > u]
  m <- length(excess)
  emp.prob <- 1:m / (m + 1)
  prob.hat <- 1 - (1 + xi * (sort(excess) - u) / tau)^(-1/xi)
  plot(emp.prob, prob.hat, xlab = "Probabilidades empiricas",
       ylab = "Probabilidades ajustadas", xlim = c(0, 1),
       ylim = c(0, 1))
  abline(0, 1, col = "grey")
}


ppgpd(severidad, umbral, fit.gp$estimate[1], fit.gp$estimate[2]) 


## Pérdidas agregadas y VAR

perdida_agregada <- aggregateDist("simulation", 
                                    model.freq = expression(y = rnbinom(size = fnbinom$estimate[1], mu = fnbinom$estimate[2])),
                                    model.sev = expression(y = rburr(shape1 = fburr$estimate[1], shape2 = fburr$estimate[2],
                                                                     scale = fburr$estimate[3])),
                                    nb.simul = 1000)
perdida_agregada
plot(perdida_agregada)

# Deteminacion del Value at Risk al 90%
quantile(perdida_agregada, 0.9)







