

# 2.1.	Lectura de datos ---------------------------------------------------


ejemplo_data <- read.csv('https://raw.githubusercontent.com/jorgepaguay86/EstadisticaAplicada_TFM_UGR/master/SerieDeTiempo_Amazon_Ago2016Ago2017.csv')

View(ejemplo_data,'SerieDeTiempo_Amazon_Ago2016Ago2017')


# 2.2.	Transformación de los Datos en un Objeto Serie de Tiempo ------------------

ejemplo_data_ts <- ts(ejemplo_data$Close)

View(ejemplo_data_ts)


# 2.3.	Estadística descriptiva de la Serie de Tiempo ----------------------

plot.ts(ejemplo_data_ts,main = 'Serie de Tiempo Amazon Ago2016Ago2017', xlab = 'Tiempo', ylab = 'Precio de Cierre')

summary(ejemplo_data_ts)


# 2.4.	Calculo de Retornos, Retornos al cuadrado y Retornos absolu --------

ret = diff(log(ejemplo_data_ts))*100
 
ret_cua = ret^2

ret_abs = abs(ret)


# 2.5.	Prueba de Independencia de los rezagos -----------------------------

install.packages('xts')

library('xts')

Box.test(coredata(ret),type = 'Ljung-Box', lag =1)

?Box.test()

# 2.6.	Prueba de efectos ARCH ---------------------------------------------

install.packages('FinTS')

library('FinTS')

ArchTest(ret, lag=1)


# 2.6.	Función de Auto-correlaciones --------------------------------------

acf(ret)

# 2.7.	Función de Auto-correlaciones Parciales ----------------------------

pacf(ret)


# 2.8.	Modelos ARIMA(p,d,q) -----------------------------------------------

install.packages('tseries')

library('tseries')

arima001 <- arima(ret,c(0,0,1)) # -->> MA(1)

arima100 <- arima(ret,c(1,0,0)) # -->> AR(1)

arima101 <- arima(ret,c(1,0,1)) # -->> ARMA(1,1)

arima111 <- arima(ret,c(1,1,1)) # -->> ARIMA(1,1,1)

# 2.9.	Modelos GARCH(r,s) -------------------------------------------------

install.packages('fGarch')

library('fGarch')

garch01 <- garch(arima100$residuals,order=c(0,1),trace=F)  # -->> ARCH(1)

garch11 <- garch(arima100$residuals,order=c(1,1),trace=F)  # -->> GARCH(1,1)


# 2.10.	Criterio de elección entre Modelos ----------------------------------

# Para Modelos ARMA(p,d,q)

ModeloMedia <- c('arima001','arima100','arima101','arima111')

ModeloMedia_ValorAIC <- c(arima001$aic,arima100$aic,arima101$aic,arima111$aic)

ModeloMedia_TablaComparacion <- data.frame(ModeloMedia,ModeloMedia_ValorAIC)

subset(ModeloMedia_TablaComparacion,ModeloMedia_ValorAIC == min(ModeloMedia_TablaComparacion$ModeloMedia_ValorAIC))

# Para Modelo  GARCH(r,s)

library('tseries')

library('fGarch')

ModeloVarianza <- c('garch01','garch11')

ModeloVarianza_ValorAIC <- c(AIC(garch01),AIC(garch11))

ModeloVarianza_TablaComparacion <- data.frame(ModeloVarianza,ModeloVarianza_ValorAIC)

subset(ModeloVarianza_TablaComparacion,ModeloVarianza_ValorAIC == min(ModeloVarianza_TablaComparacion$ModeloVarianza_ValorAIC))

