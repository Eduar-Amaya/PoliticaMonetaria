##################### Paquetes requeridos #########################

# install.packages("ggplot2")
# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("vars")

#################################################
############# Librerias requeridas #######################

library(tseries)
library(ggfortify)
library(forecast)
library(vars)


# Cargamos los datos de la tasa de interes Activa utilizando el promedio ponderado
# Los datos corresponden a Enero de 2013 a Septiembre de 2021

Datos<- read.csv("Datos2BCH.csv")
TIA.ts <- ts(Datos$Tasa_Act, start = c(2013,1), end = c(2021,9), frequency = 12)
TIA.ts
plot(TIA.ts, xlab = "Tiempo", ylab = "Tasas de Interes Activa", col = "blue")

# Tasa interes Pasiva
TIP.ts <- ts(Datos$Tasa_Pas, start = c(2013,1), end = c(2021,9), frequency = 12)
TIP.ts
plot(TIP.ts, xlab = "Tiempo", ylab = "Tasa de Interes Pasiva", col = "brown")

# Cargamos los datos de la tasa de Politica Monetaria del BCH
TPM.ts <- ts(Datos$TPM, start = c(2013,1), end = c(2021,9), frequency = 12)
TPM.ts
plot(TPM.ts, xlab = "Tiempo", ylab = "Tasa de PM", col = "purple")

# Cargamos los datos del Indice de Precios al Consumidor 
IPC.ts <- ts(Datos$IPC, start = c(2013,1), end = c(2021,9), frequency = 12)
plot(IPC.ts, xlab = "Tiempo", ylab = "IPC", col = "orange")

# Graficas de Las series juntas
ts.plot(TIA.ts,TIP.ts,TPM.ts,IPC.ts, xlab = "Tiempo", ylab = "Tasas de Interes",
        col = c("blue","brown","purple","orange"), main = "Representacion de las Series")
legend(2013, 350, legend=c("TIA","TIP","TPM", "IPC"),
       col=c("blue","brown","purple","orange"), lty=1:4, cex=0.8)



############################# Aplicando logaritmo #################

# Aplicando logaritmo a TIA.ts
TIA_log <- log(TIA.ts)


# Aplicando logaritmo a TIP.ts
TIP_log <- log(TIP.ts)

# Aplicando logaritmo a TPM.ts
TPM_log <- log(TPM.ts)

# Aplicando logaritmo a IPC.ts
IPC_log <- log(IPC.ts)

# Grafica del logaritmo de las series
ts.plot(TIA_log,TIP_log,TPM_log,IPC_log, xlab = "Tiempo", ylab = "Logaritmos Tasas",
        col = c("blue","brown","purple","orange"), main = "Logaritmos de las Series")
legend(2013, 5, legend=c("TIA_log","TIP_log","TPM_log","IPC_log"),
       col=c("blue","brown","purple","orange"), lty=1:4, cex=0.8)

############## Test de Dickey-Fuller ##############################

# Aplicaremos el Test a TIA.ts de Dickey-Fuller. \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05
adf <- adf.test(TIA.ts)
adf$p.value
# El p=0.2663158 nos da evidencia aceptar la hipotesis nula, H_{0}, es decir,
#que la serie TIA no es estacionaria, por tanto debemos diferenciar la serie.

# Aplicaremos el Test a TIP.ts de Dickey-Fuller. \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05
TPdf <- adf.test(TIP.ts)
TPdf$p.value
# El p=0.827974 nos da evidencia aceptar la hipotesis nula, H_{0}, es decir,
#que la serie TIP no es estacionaria, por tanto debemos diferenciar la serie.

# Aplicaremos el Test a TPM.ts de Dickey-Fuller. \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05
Tdf <- adf.test(TPM.ts)
Tdf$p.value
# El p=0.7817857 nos da evidencia aceptar la hipotesis nula, H_{0}, es decir, que la 
# serie Tasa de Politica Monetaria no es estacionaria, por tanto debemos diferenciar la serie.

# Test de Dickey-Fuller a IPC.ts \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05

Idf <- adf.test(IPC.ts)
Idf$p.value
# El p=0.7472833 nos da evidencia aceptar la hipotesis nula, H_{0}, es decir, que la 
# serie IPC no es estacionaria, por tanto debemos diferenciar la serie.


############# Diferenciando una vez ################################

# Aplicamos una diferencia las tasas de interes 
dif1 <- diff(TIA_log)
plot(dif1)

TIPdif1 <- diff(TIP_log)
plot(TIPdif1)

#Aplicamos una diferencia a la Tasa de Politica Monetaria
Tdif1 <- diff(TPM_log)
plot(Tdif1)


# Aplicamps una diferencia a IPC
Idif1 <- diff(IPC_log)
plot(Idif1)

# Grafica de las series con una diferencia
ts.plot(dif1,TIPdif1,Tdif1,Idif1, xlab = "Tiempo", ylab = "Tasas de Interes",
        col = c("blue","brown","purple","orange"), main = "Series con una Diferencia")
legend(2013, -0.10, legend=c("TIA","TIP","TPM","IPC"),
       col=c("blue","brown","purple","orange"), lty=1:4, cex=0.8)

############## Test de Dickey-Fuller ##############################

# Aplicaremos el Test a TIA.ts de Dickey-Fuller. \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05
adf1 <- adf.test(dif1)
adf1$p.value
#La Serie es estacionaria

# Aplicaremos el Test a TIP.ts de Dickey-Fuller. \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05
TPdf1 <- adf.test(TIPdif1)
TPdf1$p.value
# La Serie es estacionaria

# Aplicaremos el Test a TPM.ts de Dickey-Fuller. \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05
Tdf1 <- adf.test(Tdif1)
Tdf1$p.value
# El p=0.2281536 nos da evidencia aceptar la hipotesis nula, H_{0}, es decir, que la 
# serie Tasa de Politica Monetaria no es estacionaria, por tanto debemos diferenciar la serie.

# Test de Dickey-Fuller a IPC.ts \alpha = 5%
# H_{0}: La serie no es estacionaria. p > 0 .05
# H_{1}: La serie es estacionaria p < 0.05

Idf1 <- adf.test(Idif1)
Idf1$p.value
# La Serie es estacionaria


############## Aplicando Segunda Diferencias ###########################
# Segunda diferencia a Tasa de interes
dif2 <- diff(dif1)
plot(dif2,  xlab = "Tiempo", ylab = "Dos Dif TIA", col = "blue")

TIPdif2 <- diff(TIPdif1)
plot(dif2,  xlab = "Tiempo", ylab = "Dos Dif TIP", col = "brown")

# Segunda diferencia a TPM
Tdif2 <- diff(Tdif1)
plot(Tdif2,  xlab = "Tiempo", ylab = "Dos Dif TPM", col = "purple")

# Segunda diferencia a IPC
Idif2 <- diff(Idif1)
plot(Idif2, xlab = "Tiempo", ylab = "Dos Dif IPC", col = "orange")

# Grafica de las Series con dos Diferencias 
ts.plot(dif2,TIPdif2,Tdif2,Idif2, xlab = "Tiempo", ylab = "Tasas de Interes",
        col = c("blue","brown","purple","orange"), main = "Series Estacionarias")
legend(2013, 0.2, legend=c("TIA","TIP","TPM","IPC"),
       col=c("blue","brown","purple","orange"), lty=1:4, cex=0.8)



################## Dickey-Fuller #########################################

adf2 <- adf.test(dif2)
adf2$p.value

TIPdf2 <- adf.test(TIPdif2)
TIPdf2$p.value

Tdf2 <- adf.test(Tdif2)
Tdf2$p.value

Idf2 <- adf.test(Idif2)
Idf2$p.value

# El valor p=0.01 del test nos indica que se puede rechazar la hipotesis nula H_{0}.
# Por lo tanto las Series TIA.ts, TIP.TS, IPC.ts y TPM.ts son estacionarias.

######## Test de Causalidad de Granger ######################################
# H_{0}: La TPM no causa en el sentido de Granger a la TIP. Si > 0.05
# H_{1}: La TPM si causa en el sentido de Granger a la TIP. Si < 0.05
grangertest(TIPdif2~Tdif2, order = 1)
# Notamos que la TPM si causa a la TIP

grangertest(dif2~Tdif2, order = 1)
# La TPM si causa en el sentido de Granger a la TIA
# La TPM no causa en el sentido de Granger al IPC

grangertest(Tdif2~Idif2, order = 2)
# Notamos que IPC si causa a la TPM

grangertest(TIPdif2~Idif2, order = 6)
# Notamos que el IPC si causa en el sentido de Granger a la TIP
# El IPC no causa en el sentido de Granger a TIA.

# La TIP no causa en el sentido de Granger ni TPM, IPC ni a TIA

grangertest(Tdif2~dif2, order = 2)
# Notamos que la TIA si causa en el sentido de Granger a la TPM

grangertest(TIPdif2~dif2, order = 4)
# Notamos que TIA si causa en el sentido de Granger a TIP

grangertest(Idif2~dif2, order = 1)
# Notamos que la TIA si causa en el sentido de Granger al IPC

################################################################################
###############################################################################

# Una vez que hemos aplicado el Test de Causalidad de Granger creamos un data frame
# Unicamente con las segundas diferencias, es decir, con las series estacionarias
VTdif2 <- ts(Tdif2, start = c(2013,1), frequency = 12)
VTIPdif2 <- ts(TIPdif2, start = c(2013,1), frequency = 12)
Vdif2 <- ts(dif2, start = c(2013,1), frequency = 12)
VIdif2 <- ts(Idif2, start = c(2013,1), frequency = 12)
VarDatos <- cbind(VTdif2,VTIPdif2,Vdif2,VIdif2)
VarDatos
# Identificacion de nuestro modelo VAR
VARselect(VarDatos, type = "const")


# Seleccionamos nuestro modelo con 4 rezagos
Var2 <- VAR(VarDatos,p=4)
Var2

# Pruebas de especificacion del modelo VAR
summary(Var2)
# Si las raices de los polinomios son menores a 1
# Se satisface la condicion de estabilidad 

plot(Var2)

# Pruebas de Especificacion 
# Prueba de Autocorrelacion Serial en los Residuales
# H_{0}: Los Residuales no estan correlacionados, > 0.05
# H_{1}: Los residuales si estan correlacionados, p < 0.05
serial1 <- serial.test(Var2, lags.pt = 4, type = "PT.asymptotic")
serial1$serial
# Concluimos que si hay una correlacion serial

# ****************************************************#
# Prueba de Normalidad de los Residuales
# H_{0}: Los Residuales se distribuyen normal > 0.05
# H_{1}: Los residuales no se distribuyen normal < 0.05
NorVar1 <- normality.test(Var2)
NorVar1$jb.mul
# ****************************************************#

# Prueba de Homocedasticidad de la Varianza de los residuales
# H_{0}: La Varianza de los residuales es constante >0.05
# H_{1}: L Varianza de los residuales no es constante <0.05
arch1 <- arch.test(Var2, lags.multi = 12)
arch1$arch.mul
# Se concluye que la varianza es constante

# Impulso Respuesta de las tasas de interes acticas ante una innovacion de la TPM, TIP, IPC
Var2_irf <- irf(Var2, response = "Vdif2", n.ahead = 8, boot = TRUE)
Var2_irf

plot(Var2_irf)

# Impulso Respuesta de las tasas de interes pasivas ante una innovacion de la TPM, TIA, IPC
Va3_irf <- irf(Var2, response = "VTIPdif2", n.ahead = 8, boot = TRUE)
Va3_irf
plot(Va3_irf)
