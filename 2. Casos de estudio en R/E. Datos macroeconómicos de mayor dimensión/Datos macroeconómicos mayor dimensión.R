#################### Series de Tiempo Multivariantes
####### Caso de Estudio: Datos macroeconómicos mayor dimensión
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


####### Cargando los datos reales "Datos macroeconómicos mayor dimensión"
# Contiene 20 variables 
library(readr)
dat= read_csv("Mac_Ric.csv")
View(dat)

plot(ts(dat$LREERS,start=c(1970,1),freq=4),ylab="LREERS") # logarithm of real effective exchange rate
plot(ts(dat$RIRR,start=c(1970,1),freq=4),ylab="RIRR") # real interest rate relative to trading partners
plot(ts(dat$LRGDPPCR,start=c(1970,1),freq=4),ylab="LRGDPPCR") # logarithm of real GDP per capita relative to trading partners
plot(ts(dat$LPR2COMM5,start=c(1970,1),freq=4),ylab="LPR2COMM5") # real commodity prices
plot(ts(dat$OPENY,start=c(1970,1),freq=4),ylab="OPENY") # openness - ratio to GDP of exports and imports
plot(ts(dat$FBYA,start=c(1970,1),freq=4),ylab="FBYA") # ratio of fiscal balance to GDP
plot(ts(dat$NFAOFPY,start=c(1970,1),freq=4),ylab="NFAOFPY") # ratio to GDP of net foreign assets of the banking system

########### Modelo VAR con variables exógenas
#Variables endógenas
dat.VAR <- cbind(dat$LREERS,dat$RIRR,dat$LRGDPPCR,dat$LPR2COMM5,dat$OPENY,dat$FBYA,dat$NFAOFPY)
colnames(dat.VAR) <- c("LREERS","RIRR","LRGDPPCR","LPR2COMM5","OPENY","FBYA","NFAOFPY")

#Variables exógenas
dat.EXO <- cbind(dat$SDUMC1,dat$SDUMC2,dat$SDUMC3,dat$DUMRER1,dat$DUMRER2,dat$DUMFBYA,dat$DUMNFAOFPY)
colnames(dat.EXO) <- c("SDUMC1","SDUMC2","SDUMC3","DUMRER1","DUMRER2","DUMFBYA","NFAOFPY")

# Eliminar las variables de variables con valores solo cero
dat.EXO=dat.EXO[,1:3]

# Crear el modelo VAR con "exog==dat.EXO"
VAR.est <- vars::VAR(dat.VAR,lag.max=10, ic="AIC",type="const",exog=dat.EXO)
summary(VAR.est)

####### Modelo VAR sin variables exógenas:
VAR.est2 <- vars::VAR(dat.VAR,lag.max=10, ic="AIC",type="const")
summary(VAR.est2)

####### Predicciones
predictions <- predict(VAR.est2, n.ahead = 10, ci = 0.95)
plot(predictions, names = "LREERS")

predictions <- predict(VAR.est2, n.ahead = 10, ci = 0.95)
plot(predictions, names = "RIRR")

predictions <- predict(VAR.est2, n.ahead = 10, ci = 0.95)
plot(predictions, names = "LRGDPPCR")

predictions <- predict(VAR.est2, n.ahead = 10, ci = 0.95)
plot(predictions, names = "LPR2COMM5")

predictions <- predict(VAR.est2, n.ahead = 10, ci = 0.95)
plot(predictions, names = "OPENY")

predictions <- predict(VAR.est2, n.ahead = 10, ci = 0.95)
plot(predictions, names = "FBYA")

predictions <- predict(VAR.est2, n.ahead = 10, ci = 0.95)
plot(predictions, names = "NFAOFPY")

