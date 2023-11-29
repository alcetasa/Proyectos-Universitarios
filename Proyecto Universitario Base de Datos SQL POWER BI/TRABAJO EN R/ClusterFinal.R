
### Modelos de la Data_Seguridad ###

library(xlsx)

# Paquetes
library(pacman)
p_load(cluster,aplpack,fpc,foreign,TeachingDemos,
       factoextra,NbClust,ape,corrplot,DataExplorer,
       funModeling,compareGroups,tidyverse,dendextend,
       igraph,FeatureImpCluster,flexclust,LICORS)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

data <- read.xlsx("Datos_Seguridad.xlsx", sheetIndex = 4)
head(data)



#Elimina todas las filas que contengan algun valor NA

data <- na.omit(data)

data$BANDAS_TOT_2016 <- data$BANDAS_TOT_2016*50
data$BANDAS_TOT_2017 <- data$BANDAS_TOT_2017*50

# Estructura de los datos

str(data)

# Detectando y graficando los % de datos perdidos

plot_missing(data, ggtheme=theme_bw()) + 
  labs(title = "Datos perdidos por variable",y = "Datos perdidos",
       x = "Variables")

#Reduciendo las variables a considerar para perfilar mejor los clusters

datac <- select(data,BANDAS_TOT_2016,BANDAS_TOT_2017,DETENIDOS_TOT_2016,DETENIDOS_TOT_2017)
str(datac)

# Gráfico de variables numéricas

plot_num(datac)
#----------------------------------------

# Cluster Jerárquico Aglomeratiov: AGNES

# Usando la distancia euclidiana
d <- dist(datac, method = "euclidean")

# Cluster Jerarquico usando el método de enlace average
res.hc <- hclust(d, method = "average" )  # calcula el cluster jerarquico

res.hc

str(res.hc)   # se ve la estructura

# Proceso de agrupamiento indicando los individuos
res.hc$merge   # hace un ranking de los individuos mas parecidos

# Criterio de distancias con alturas
alturas <- data.frame(etapa=1:1309,distancia=res.hc$height)
alturas
#Grafica
library(ggplot2)
ggplot(alturas) + aes(x=etapa,y=distancia) + geom_point() + geom_line()+
  scale_x_continuous(breaks=seq(1,20)) + 
  geom_vline(xintercept = 99,col="red",lty=3) +
  geom_vline(xintercept = 98,col="blue",lty=3) +
  theme_bw()
#luego se cuenta los puntos desde la vertical de referencia, hay 3 cluster

# Dividir en 3 clusters
grp <- cutree(res.hc, k = 3)  # funcion que forma cluter y te las da en una columna
grp

# Ploteo de Cluster en poligonos
row.names(datac) <- data$CODIGO_NOMBRE

fviz_cluster(list(data = datac, cluster = grp),
             palette = c("#2E9FDF",  "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = F, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())



