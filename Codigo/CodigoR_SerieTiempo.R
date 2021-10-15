
##################### Paquetes requeridos #########################

# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("forecast")

#################################################
############# Librerias requeridas #######################

library(tseries)
library(ggfortify)
library(forecast)

### Creando la variable gas y almacenando los datos

gas = scan('http://verso.mat.uam.es/~joser.berrendero/datos/gas6677.dat')

## Grafica de los datos
plot(gas)

# Convirtiendo la variable gas en un objeto ts
## Y grafica de la serie 

gas.ts = ts(gas, start = c(1966,1), frequency = 12)
gas.ts
plot(gas.ts, xlab = "Tiempo", ylab = "Consumo", col = "blue")

# Descomponemos la serie con el comando decompose:
des_gas.ts <- decompose(gas.ts)
plot(des_gas.ts, xlab = 'Año')

# Con la siguiente grafica se puede apreciar que la serie no es estacionaria 

plot(acf(gas.ts))

# Necesitamos estabulizar la variabiliddad lo hacemos con el logaritmo
log_gas.ts <- log(gas.ts)
plot(log_gas.ts, col = "blue")


# Necesitamos saber el numero de diferenciacion regulares y estacionales se necesitan
# para que convertir la serie en estacionacionaria. Y lo hacemos mediante las funciones de
# ndiffs y nsdiffs respectivamente.

ndiffs(gas.ts)
nsdiffs(gas.ts)


# Quiere decir que necesitamos una diferenciacion regular y otra estacional
diff.gasts <- diff(log_gas.ts)
plot(diff.gasts, col = "blue")

# ya se ha eliminado la componente tendencia Ahora eliminaremos la componente de 
# estacionalidad lo cual nos da aparentemente una serie estaionaria.
dif2_gas.ts <- diff(diff.gasts, lag = 12)
plot(dif2_gas.ts, col = "blue")


# Las graficas anteriores nos dan idea de que la serie es estacionaria. Y para comprobarlo
# Aplicaremos el Test de Dickey-Fuller. \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05
adf <- adf.test(dif2_gas.ts)
adf$p.value

# El valor p=0.01 del test nos indica que se puede rechazar la hipotesis nula H_{0}.
# Por lo tanto la Serie es estacionaria.


# La ACF nos indica el numero de medias moviles y la PACF nos indica el
# numero de autoregresivos.
# volveremos a graficar la la funcion de autocorrelacion y la funcion de 
# de autocorrelacion parcial
plot(acf(dif2_gas.ts))
plot(pacf(dif2_gas.ts))

# Observando las graficas anteriores de ACF Y PACF podemos plantear varios modelos para el
# analisis de la serie como ser ARIMA(2,0,2)(0,1,2), ARIMA(2,0,2)(2,1,2), ARIMA(2,1,2)(0,1,2),
# o incluso ARIMA(2,1,2)(2,1,2), ETC.


####################### Identificacion del modelo ####################################
### Comparando modelos para la identificacion
arima1 <- Arima(gas.ts, order = c(2,0,2), seasonal = list(order = c(0,1,2), period = 12))
arima2 <- Arima(gas.ts, order = c(2,0,2), seasonal = list(order = c(2,1,2), period = 12))
arima3 <- Arima(gas.ts, order = c(2,1,2), seasonal = list(order = c(0,1,2), period = 12))
arima4 <- Arima(gas.ts, order = c(2,1,2), seasonal = list(order = c(2,1,2), period = 12))


# Clculamos los criterios AIC y BIC
AIC(arima1, arima2, arima3, arima4)
BIC(arima1, arima2, arima3, arima4)

# Podemos apreciar que los ajustes que mejor AIC y BIC tienen dos componentes de medias moviles
# y dos autoregresivas. Siendo ARIMA(2,1,2)(0,1,2) el modelo que los tests arrojan con menor 
# valor y por tanto con mayor consideracion. Una vez estiamos los modelos y elegido el mejor
# de ellos, procedemos a validarlo


#############################   Validacion  ###############################################
# Grafica de los correlogramas de los residuos para comprobar que son ruido blanco
# ARIMA(2,1,2)(0,1,2)
plot(acf(arima3$residuals))
plot(pacf(arima3$residuals))

# En las graficas anteriores podemos apreciar que no hay ningun rezago significaivo (aparte del 0
# que por definicion es 1) que denote ningun tipo de estructura, Por tanto podemos decir que 
# Los residuos son ruido Blanco. Las siguientes graficas son sobre los errores 
ggtsdiag(arima3)


# Haremos la prueba de Ljung-Box para ruido blanco: \alpha = 5 %
# H_{0}:  Ruido blanco si p  > 0.05
# H_{1}: No hay ruido blanco si p < 0.05
lb <- Box.test(arima3$residuals, type = "Ljung-Box")
lb$p.value

## Graficamos el error para corroborar
error <- residuals(arima3)
plot(error, col = "blue")
# El eror tiene media igual a cero, quiere decir que muestro modelo es bueno


########################### Prediccion de la Serie ######################################
pronostico <- forecast::forecast(arima3, h = 10)
pronostico
plot(pronostico, col = "red")





