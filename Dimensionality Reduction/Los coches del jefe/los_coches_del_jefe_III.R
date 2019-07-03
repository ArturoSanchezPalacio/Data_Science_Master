####LOS COCHES DEL JEFE

## ARTURO SÁNCHEZ PALACIO
## Fecha: 22/XII/18

#Las bibliotecas a usar son:

library(foreign)
library(cluster) #La función daisy está en él.
library(corrplot)
library(factoextra) #La función eclust está en él.
library(dplyr)
library(Rtsne) #Para construir la gráfica de clusters
library(tidyverse)
#Mediante la biblioteca foreign se dispone del método read.spss para la carga de archivos en R. 
dataset <- read.spss("/Users/arturosanchezpalacio/Documents/CUNEF/Técnicas reducción de la dimensión/Prácticas/Los coches del jefe/tterreno.sav", to.data.frame = TRUE)

#Una vez cargado el archivo se procede a la depuración de los datos:
str(dataset)

#La variable modelo no aporta nada al estudio pero se puede utilizar para ver como se agrupa:
data <- dataset

#La variable acel2 se dicotomiza para facilitar el trabajo:

data$acel2 <- as.character(data$acel2)
data$acel2[data$acel2 == "Mayor a 10 segundos"] <- 0
data$acel2[data$acel2 == "Menor a 10 segundos"] <- 1
data$acel2 <- as.factor(data$acel2)


############# Actualización 22/XII/18 #########

#En las dos versiones anteriores no había realizado ningún tratamiento para los valores no registrados; a continuación trabajamos con ellos:

# Se comienza realizando una exploración de los NA por columna:

#Identificación de los NAs por columnas
apply(data, 2, function(x) {sum(is.na(x))})

#Las variavles marca, modelo, pvp, cilindro, cc, potencia, rpm, plazas y acel2 no presentan ningún hueco.
#Vamos a rellenar los huecos presentes en las otras variables:

data$peso=replace_na(data$peso, 1850) 

# con el resto
subset(data, is.na(cons90)) 

# En el caso de los Nissan y Ssanyong sustituiremos con los consumos medios de la marca, 

data %>%
  group_by(marca) %>%
  dplyr::summarize(Mean90 = mean(cons90, na.rm=TRUE),
                   Mean120 = mean(cons120, na.rm=TRUE),
                   MeanUrb = mean(consurb, na.rm=TRUE)) 

data$cons90.2 <- ifelse(data$marca %in% c("NISSAN") & is.na(data$cons90), 8.4, data$cons90)
data$cons90.3 <- ifelse(data$marca %in% c("SSANGYONG") & is.na(data$cons90), 8.17, data$cons90.2)

# Para los UAZ, por el consumo medio de los TT de 7 plazas
data %>%
  group_by(plazas) %>%
  dplyr::summarize(Mean90 = mean(cons90, na.rm=TRUE),
                   Mean120 = mean(cons120, na.rm=TRUE),
                   MeanUrb = mean(consurb, na.rm=TRUE)) 

data$cons90.4 <- ifelse(data$marca %in% c("UAZ") & is.na(data$cons90), 9.29, data$cons90.3)

#♥ Finalmente, tenemos cons90.4 con todos los consumos y "pisamos" cons90
data$cons90=data$cons90.4


# Procedemos igual con los cons120 y consurb:
# ASIA: cons120 de los de 4 plazas
data$cons120.2 <- ifelse(data$marca %in% c("ASIA MOTORS") & is.na(data$cons120), 11, data$cons120)

# Jeep  Grand Cherokee Jamb por el 2.5TD 3 ptas (justo encima)
data$cons120.3 <- ifelse(data$marca %in% c("JEEP") & is.na(data$cons120), 10.5, data$cons120.2)

# LADA  por el de los 5 plazas
data$cons120.4 <- ifelse(data$marca %in% c("LADA") & is.na(data$cons120), 12.8, data$cons120.3)

# NISSAN y SSanyong por los consumos medios  de la marca a 120

data$cons120.5 <- ifelse(data$marca %in% c("NISSAN") & is.na(data$cons120), 12.5, data$cons120.4)
data$cons120.6 <- ifelse(data$marca %in% c("SSANGYONG") & is.na(data$cons120), 12.6, data$cons120.5)

#  Por último, los UAZ por el consumo medio de los TT de 7 plazas
data$cons120.7 <- ifelse(data$marca %in% c("UAZ") & is.na(data$cons120), 13.5, data$cons120.6)

##♠ Pisamos cons120 con cons120.7

data$cons120=data$cons120.7

# Eliminamos las sobrantes
data[,c(16:21)]=NULL

# Actuamos del mismo modo para consurb y velocida
data$consurb.1 <- ifelse(data$marca %in% c("JEEP") & is.na(data$consurb), 9.8, data$consurb)
data$consurb.2 <- ifelse(data$marca %in% c("NISSAN") & is.na(data$consurb), 12.2, data$consurb.1)
data$consurb.3 <- ifelse(data$marca %in% c("TOYOTA") & is.na(data$consurb), 10.4, data$consurb.2) # cambiamos por el análogo - justo encima

data$consurb=data$consurb.3

# Eliminamos las sobrantes
data[,c(16:18)]=NULL

data$velocida.1 <- ifelse(data$marca %in% c("SUZUKI") & is.na(data$velocida), 147, data$velocida)
data$velocida.2 <- ifelse(data$marca %in% c("TATA") & is.na(data$velocida), 135, data$velocida.1)

data$velocida=data$velocida.2

## Definimos el DF con las variables que queremos, todas menos rpm, acelerac, acel2

data <- data[, c(1:13)]

# Comprobamos los NA
apply(data, 2, function(x) {sum(is.na(x))})

# Uno las dos 1as columnas, y las elimino
data$mod <- paste(data$marca,"-",data$modelo)
data[,c(1,2)] <- NULL


# Como hay duplicados (debido a versiones distintas no recogidas en el nombre del modelo), y eso nos impide renombrar las filas, los re-codificamos 
data$mod <- with(data, make.unique(as.character(mod)))


# Y pongo por nombre de fila el valor de la columna data

data <- data.frame(data[,-12], row.names = data[,12])

data <- data[, 1:11]

# En esta última versión tras el tratamiento de los NA se elimina el tratamiento como factor de las variables:

library(varhandle)

data$cilindro <- unfactor(data$cilindro)
data$plazas <- unfactor(data$plazas)

# Ahora que todas las variables son numéricas es importante tipificar por las distntas unidades de medida que se emplean (pesetas,cilindro, kilogramos...):
datos <- data
data <- as.data.frame(scale(data))

#Las variables ya están limpias y preparadas para la exploración. Construimos un cluster jerárquico con vínculo completo:

cluster.complete <- hclust(dist(data), method = "complete")


# Se realiza una primera exploración visual del endograma:

plot(cluster.complete, main = "Vínculo completo", xlab = "", sub = "", cex = 0.9)

cutree(cluster.complete, 3)

table(cutree(cluster.complete, 5), rownames(data))


# Añadido

library("NbClust")

set.seed(123)
clus.nb = NbClust(data, distance = "euclidean",
                  min.nc = 2, max.nc = 10, 
                  method = "complete", index ="gap") 
clus.nb # resultados

# Todos los valores del estadístico de corte
clus.nb$All.index
# Número óptimo de clusters
clus.nb$Best.nc
# Mejor partición
clus.nb$Best.partition

# Cálculo de todos los índices, menos los 4 que son muy exigentes operacionalmente (si los quisiéramos, alllong en vez de all)
nb.todos = NbClust(data, distance = "euclidean", min.nc = 2,
                   max.nc = 10, method = "complete", index ="alllong")
nb.todos

#podemos visualizar un resumen
fviz_nbclust(nb.todos) + theme_minimal() +
  labs(x="Número k de clusters", y="Frecuencia")

# A partir de esto parece que los dos mejores valores sugeridos son 2 y 3 siendo el tres ligeramente mejor. En mi opinión
# solo dos clusters aportarían poca especificidad así que se selecciona el 3:


fviz_nbclust(data,  hcut, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  ggtitle("Número óptimo de clusters - jerárquico") +
  labs(x="Número k de clusters",y="Suma total de cuadrados intra grupos")

# Así nos quedamos definitivamente con tres clusters que son representados a continuación:

fviz_dend(cluster.complete, k = 3,  cex = 0.5)
cutree(cluster.complete, 3)




#Asignamos a cada coche un garaje:

Grupo <- cutree(cluster.complete, 3)
solucion <- data.frame(data, Grupo)

solucionn <- data.frame(datos, Grupo)


#Número de coches en cada cluster:



(Cluster_1 <- sum(solucion$Grupo == 1))
(Cluster_2 <- sum(solucion$Grupo == 2))
(Cluster_3 <- sum(solucion$Grupo == 3))

Grupo_2 <- solucionn[solucionn$Grupo == 1,]
Grupo_1 <- solucionn[solucionn$Grupo == 2,]
Grupo_3 <- solucionn[solucionn$Grupo == 3,]

summary(Grupo_1)

summary(Grupo_2)

summary(Grupo_3)
