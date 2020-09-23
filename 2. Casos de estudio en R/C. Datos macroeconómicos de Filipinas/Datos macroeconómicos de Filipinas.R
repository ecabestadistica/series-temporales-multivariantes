#################### Series de Tiempo Multivariantes
####### Caso de Estudio: Datos macroeconómicos de Filipinas
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


####### Cargando los datos reales "Datos macroeconómicos de Filipinas"
# Contiene 4 variables de series de tiempo de datos económicos

library(readr)
mp = read_csv("sampleVAR.csv")
class(mp)
head(mp)


# Librería para el test ADF de estacionariedad
library(tseries)


####### Análisis exploratorio
# Convertir a objeto ts las dos series
rgdp <- ts(mp$real_gdp_growth, start = c(1999,1), frequency = 4)
psei <- ts(mp$psei, start = c(1999,1), frequency = 4)
bsp <- ts(mp$bsp_rrp, start = c(1999,1), frequency = 4)
unem <- ts(mp$unem, start = c(1999,1), frequency = 4)

# Gráfico con plot:
dat.mv <- cbind(rgdp, psei, bsp, unem)
plot(dat.mv )

## Dividir la serie en conjunto de entrenamiento y de prueba
library(dplyr)

n_obs=10
end=dim(dat.mv)[1]
X_train = dat.mv [1:(end-n_obs),]
X_test = dat.mv [(end-n_obs+1):end,]
dim(X_test)


####### Prueba de estacionariedad
apply(X_train, 2, adf.test) #2 para especificar que lo queremos aplicar por columnas

# Casi todos los p-valores son > 0.05,  hay que diferenciar las series porque no son estacionarias

# Diferenciamos
library(MTS)
stnry = diffM(X_train)

# Volviendo a hacer el test:
apply(stnry, 2, adf.test)

# Ahora sí, todas son estacionarias


#### MODELO VAR
# Identificación del orden del modelo
library(vars)
VARselect(stnry, type = "none", lag.max = 10)


# Creando el modelo
var.a <- vars::VAR(stnry,
                   
                   lag.max = 10,
                   
                   ic = "AIC",
                   
                   type = "none")

summary(var.a)

## Diagnosis del modelo (Portmanteau test para objetos var)
mv.serial=serial.test(var.a)
mv.serial
plot(mv.serial, names = "rgdp")
plot(mv.serial, names = "psei")
plot(mv.serial, names = "bsp")
plot(mv.serial, names = "unem")


######## Forecasting usando el modelo VAR (Hallando los pronósticos)

fcast = predict(var.a, n.ahead = 10)
plot(fcast)

######### Forecast gold
rgdp_pred = fcast$fcst[1]; rgdp_pred 


# Extrayendo la columna de pronósticos
x = rgdp_pred$rgdp[,1]; x

######### Invirtiendo la diferenciación
tail(X_train)

x = cumsum(x) + 7

plot.ts(x)

# Combinando los datos reales y la predicción en una sola serie de tiempo
rgdpinv =ts(c(X_train[,1], x),
            start = c(1999,1), frequency = 4)


# Dibujando todo 
plot(rgdpinv)
plot.ts(rgdpinv[50:80])


# Plot avanzado con separación visual entre lo real y lo pronosticado
library(lattice)
library(grid)
library(zoo)

# Objeto zoo
xx = zoo(rgdpinv[50:80])


# En el parámetro grid.clip ponemos la cantidad de observaciones que son reales dentro de las 
# que hemos elegido. Hemos cogido 31 de las que 10 son pronósticos, así que grid.clip sería 21-1

xyplot(xx, grid=TRUE, panel = function(xx, y, ...){
  
  panel.xyplot(xx, y, col="red", ...)
  
  grid.clip(unit(20, "native"), just=c("right")) 
  
  panel.xyplot(xx, y, col="green", ...) })

# Como vemos si nos vamos demasiado lejos en el futuro se aplana la predicción

### Evaluacion del modelo
rmse=sqrt(mean((X_test[,1]-x)^2))
rmse

