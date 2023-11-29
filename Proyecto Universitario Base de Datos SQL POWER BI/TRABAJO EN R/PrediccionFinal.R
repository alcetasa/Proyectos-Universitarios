
### Modelo de Prediccion ###

library(xlsx)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Lectura de datos

data <- read.xlsx("Datos_Seguridad.xlsx", sheetIndex = 2)
names(data)

data <-data[c(1:1310),]

# Detectando y graficando los % de datos perdidos

plot_missing(data, ggtheme=theme_bw()) + 
  labs(title = "Datos perdidos por variable",y = "Datos perdidos",
       x = "Variables")

# Gráfico de variables numéricas

plot_num(data)

# Elaboracion del modelo predictivo

modelo0 <- lm(DETENIDOS_TOTAL~.,data=data)
summary(modelo0)



  
  
  
  