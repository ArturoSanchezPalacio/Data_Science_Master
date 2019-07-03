####LOS COCHES DEL JEFE

## ARTURO SÁNCHEZ PALACIO
## Fecha: 23/XI/18

#Las bibliotecas a usar son:

library(foreign)
library(cluster) #La función daisy está en él.
library(corrplot)
library(factoextra) #La función eclust está en él.
library(dplyr)
library(Rtsne) #Para construir la gráfica de clusters

#Mediante la biblioteca foreign se dispone del método read.spss para la carga de archivos en R. 
dataset <- read.spss("/Users/arturosanchezpalacio/Documents/CUNEF/Técnicas reducción de la dimensión/Prácticas/Los coches del jefe I/tterreno.sav", to.data.frame = TRUE)

#Una vez cargado el archivo se procede a la depuración de los datos:
str(dataset)

#La variable modelo no aporta nada al estudio pero se puede utilizar para ver como se agrupa:
data <- dataset

#La variable acel2 se dicotomiza para facilitar el trabajo:

data$acel2 <- as.character(data$acel2)
data$acel2[data$acel2 == "Mayor a 10 segundos"] <- 0
data$acel2[data$acel2 == "Menor a 10 segundos"] <- 1
data$acel2 <- as.factor(data$acel2)

#Es razonable pensar que hay una gran correlación entre el peso y el número de plazas:

corr_peso_plazas <- lm(data$peso ~ data$plazas, data = data)
anova(corr_peso_plazas)

boxplot(data$peso ~data$plazas, main = "Diagrama de cajas peso - número de plazas", xlab = "Número de plazas", ylab = "Peso")


#Correlación precio y marca

corr_precio_marca <- lm(data$pvp ~ data$marca, data = data)
anova(corr_precio_marca)

boxplot(data$pvp ~ data$marca)

#Correlación aceleración y tiempo de aceleración

corr_acel <- lm(data$acelerac ~ data$acel2, data = data )
anova(corr_acel)

boxplot(data$acelerac ~ data$acel2)

#Respecto a las variables numéricas de motor se calcula la matriz de correlaciones:

motor_numeric <- data[, c(5,  6, 7, 10:14)]
str(motor_numeric)
corr_motor <- cor(motor_numeric, use = "complete.obs")
corrplot(corr_motor)

#Gráfica de la potencia respecto a la media europea:

ggplot(data = data, aes(data$potencia)) + geom_histogram( binwidth = 20) + geom_vline(xintercept = 80, color = "red")

#Gráfica de la cilindrada respecto a la media europea:

ggplot(data = data, aes(data$cc)) + geom_histogram( binwidth = 300) + geom_vline(xintercept = 1532, color = "red") + geom_vline(xintercept = 1587, color = "blue")

#Aquí empieza el análisis Cluster perse:



gower_dist <- daisy(data[, -2],  metric = "gower")

summary(gower_dist)


#A continuación vamos a hacer una prueba de consistencia del trabajo hasta ahora. Consiste en encontrar los elementos más similares y los más dispares.

gower_mat <- as.matrix(gower_dist)

data[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
#Parece muy razonable. (Aunque sería mejorable porque hay elementos repetidos pero estos solo varían en el precio)

#Buscamos los más dispares:

data[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#También parece muy razonable. Son MUY distintos.

#HASTA AQUÍ SE HA CALCULADO DE MANERA CORRECTA LA MATRIZ DE DISTANCIAS

#Calculo el número óptimo de clusters usando PAM:



sil_width <- c(NA)

for (i in 2:10) {
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

#El mayor punto indica el número óptimo de clusters

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


#Esto nos indica que el número óptimo de clusters es 9.

###Agrupo en clusters:

#Se calculan los clusters y un summary:

pam_fit <- pam(gower_dist, diss = TRUE, k = 9)


pam_results <- data %>%
  dplyr::select(-modelo) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#Se presenta un ejemplo paradigmático de cada cluster:

data[pam_fit$medoids, ]
data[pam_fit$medoids, 2 ]


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = data$modelo)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) #Mejorar colores sería interesante

#Asignamos a cada coche un garaje:

Garaje <- pam_fit$clustering
solu <- data.frame(data, Garaje)
(solucion <- solu[ ,c(1,2,16)])


##ANÁLISIS CLUSTER (Versión II)

#Basado en el análisis exploratorio y la investigación realizada previamente se descartan las siguientes variables:
#marca, consumo a 90 km/h y aceleración de 0 a 100 por los resultados explicados en el primer informe:

data <- data[, -c(1,10,14)]

gower_dist <- daisy(data[, -1],  metric = "gower")

summary(gower_dist)


#A continuación vamos a hacer una prueba de consistencia del trabajo hasta ahora. Consiste en encontrar los elementos más similares y los más dispares.

#Similares:

gower_mat <- as.matrix(gower_dist)

data[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Es consistente. Devuelve dos modelos de iguales características.

# Dispares:

data[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#También parece muy razonable. Son MUY distintos.

#HASTA AQUÍ SE HA CALCULADO DE MANERA CORRECTA LA MATRIZ DE DISTANCIAS

#Calculo el número óptimo de clusters usando PAM:


sil_width <- c(NA)

for (i in 2:10) {
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

#El mayor punto indica el número óptimo de clusters

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


#Esto nos indica que el número óptimo de clusters es 2.

###Agrupo en clusters:

#Se calculan los clusters y un summary:

pam_fit <- pam(gower_dist, diss = TRUE, k = 2)


pam_results <- data %>%
  dplyr::select(-modelo) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#Se presenta un ejemplo paradigmático de cada cluster:

data[pam_fit$medoids, ]
data[pam_fit$medoids, 2 ]


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = data$modelo)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) #Mejorar colores sería interesante




#Esta visión no parece muy adecuada así que se decide probar con otras variables:

#De las restantes y desde un punto de vista técnico parece que la menos interesante es la aceleración. Además se elimina el
#número de cilindro porque se dispone de la cilindrada que es más explícita:

data <- data[,-12]
data <- data[,-3]
str(data)

##ANÁLISIS CLUSTER (Versión III)

#Basado en el análisis exploratorio y la investigación realizada previamente se descartan las siguientes variables:
#marca, consumo a 90 km/h y aceleración de 0 a 100 por los resultados explicados en el primer informe:


gower_dist <- daisy(data[, -1],  metric = "gower")

summary(gower_dist)


#A continuación vamos a hacer una prueba de consistencia del trabajo hasta ahora. Consiste en encontrar los elementos más similares y los más dispares.

#Similares:

gower_mat <- as.matrix(gower_dist)

data[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Es consistente. Devuelve dos modelos de iguales características.

# Dispares:

data[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

#También parece muy razonable. Son MUY distintos.

#HASTA AQUÍ SE HA CALCULADO DE MANERA CORRECTA LA MATRIZ DE DISTANCIAS

#Calculo el número óptimo de clusters usando PAM:


sil_width <- c(NA)

for (i in 2:10) {
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

#El mayor punto indica el número óptimo de clusters

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


#Esto nos indica que el número óptimo de clusters es 5.

###Agrupo en clusters:

#Se calculan los clusters y un summary:

pam_fit <- pam(gower_dist, diss = TRUE, k = 5)


pam_results <- data %>%
  dplyr::select(-modelo) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#Se presenta un ejemplo paradigmático de cada cluster:

data[pam_fit$medoids, ]
data[pam_fit$medoids, 2 ]


tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = data$modelo)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) #Mejorar colores sería interesante




#Asignamos a cada coche un garaje:

Garaje <- pam_fit$clustering
solu <- data.frame(dataset, Garaje)
(solucion <- solu[ ,c(1,2,16)])



#Número de coches en cada cluster:



(Cluster_1 <- sum(pam_fit$clustering == 1))
(Cluster_2 <- sum(pam_fit$clustering == 2))
(Cluster_3 <- sum(pam_fit$clustering == 3))
(Cluster_4 <- sum(pam_fit$clustering == 4))
(Cluster_5 <- sum(pam_fit$clustering == 5))




