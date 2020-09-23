#################### Series de Tiempo Multivariantes
####### Caso de Estudio: Índices del mercado financiero europeo
####### PASOS:
#1. Análisis exploratorios de los datos.
#2. Dividir la serie en conjuntos de entrenamiento y prueba.
#3. Prueba de estacionariedad.
#4. Transformar la serie de entrenamiento si es necesario. 
#5. Construir un modelo VAR sobre las series transformadas.
#6. Causalidad de Granger.
#7. Diagnóstico del modelo.
#8. Realizar pronósticos utilizando el modelo finalmente elegido.
#9. Transformación inversa del pronóstico a la escala original.
#10. Realizar una evaluación del pronóstico.

####### Cargando los datos:
# En este caso el dataset ya se encuentra dentro de R con el nombre "EuStockMarkets"
data(EuStockMarkets)
View(EuStockMarkets)
class(EuStockMarkets)
head(EuStockMarkets)

frequency(EuStockMarkets)
start(EuStockMarkets)

# Librería para el test ADF de estacionariedad
library(tseries)


####### Análisis exploratorio

# Con plot:
plot(EuStockMarkets)

# Con autoplot:
library(ggplot2)
library(ggfortify)

autoplot(EuStockMarkets)

## Dividir la serie en conjunto de entrenamiento y de prueba
library(dplyr)
n_obs=30
end=dim(EuStockMarkets)[1]
X_train = EuStockMarkets[1:(end-n_obs),]
X_test = EuStockMarkets[(end-n_obs+1):end,]
dim(X_test)


####### Prueba de estacionariedad
apply(X_train, 2, adf.test) #2 para especificar que lo queremos aplicar por columnas

# Todos los p-valores son > 0.05, hay que diferenciar las series

## Diferenciando todo la mts 
library(MTS)
stnry = diffM(X_train)

# Volviendo a hacer el test:
apply(stnry, 2, adf.test)

#Todos los p-valores son < 0.05, todas ya son estacionarias



####### VAR modeling

plot.ts(stnry)


# Identificación del orden del modelo
library(vars)
VARselect(stnry, type = "none", lag.max = 10)


# Creando el modelo
var.a <- vars::VAR(stnry,
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "none")

summary(var.a)



# Causalidad de Granger 
causality(var.a, cause = c("DAX"))
causality(var.a, cause = c("SMI"))
causality(var.a, cause = c("CAC"))
causality(var.a, cause = c("FTSE"))

# Diagnosis del modelo (Portmanteau test para objetos var)
serial.test(var.a)

# Deseamos obtener un p-valor > 0.05, en este caso no lo obtenemos.
# Posibles soluciones:
# a) Cambiar el orden del modelo.
# b) Cambiar el tipo de modelo.
# c) Añadir otro paso de diferenciación o transformar con logaritmos.

# Estos modelos son a menudo tan complejos que no se puede alcanzar un resultado completamente 
# satisfactorio sin cambiar mucho los datos con logaritmos o varios pasos de diferencias.




## Forecasting usando el modelo VAR (Hallando los pronósticos)

fcast = predict(var.a, n.ahead = 30)
plot(fcast)

# Solo para DAX 
DAX = fcast$fcst[1]; DAX 

# Extrayendo la columna de pronósticos
x = DAX$DAX[,1]; x

# Invirtiendo la diferenciación
tail(X_train)

x = cumsum(x) + 5961.45

plot.ts(x)

# Combinando los datos reales y la predicción en una sola serie de tiempo
start(EuStockMarkets)
frequency(EuStockMarkets)

DAXinv =ts(c(X_train[,1], x),
           start = c(1991,130), frequency = 260)


# Dibujando todo 
plot(DAXinv)
plot.ts(DAXinv[1760:1860])


# Plot avanzado con separación visual entre lo real y lo pronosticado
library(lattice)
library(grid)
library(zoo)

# Objeto zoo
xx = zoo(DAXinv[1760:1860])


# En el parámetro grid.clip ponemos la cantidad de observaciones que son reales dentro de las 
# que hemos elegido. Hemos cogido 101 de las que 30 son pronósticos, así que grid.clip sería 71-1

xyplot(xx, grid=TRUE, panel = function(xx, y, ...){
  
  panel.xyplot(xx, y, col="red", ...)
  
  grid.clip(unit(70, "native"), just=c("right")) 
  
  panel.xyplot(xx, y, col="green", ...) })

# Como vemos si nos vamos demasiado lejos en el futuro se aplana la predicción



