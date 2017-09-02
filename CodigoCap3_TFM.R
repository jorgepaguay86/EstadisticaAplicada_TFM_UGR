

# 3.	Estimación de series de tiempo heterocedásticas, una aplicaci --------

{
install.packages('readr')

library('readr')

data <- read.csv('https://raw.githubusercontent.com/jorgepaguay86/EstadisticaAplicada_TFM_UGR/master/SerieDeTiempo_Google_Ago2004Ago2017Semanal.csv')

data_ts <- ts(data$AdjClose) #ojo

ret = diff(log(data_ts))*100

win.graph(width=4.875, height=2.5,pointsize=8)
par(mfrow=c(2,1))
plot(data_ts,main = 'Precio de Cierre Ajustado Google - Ago2004Ago2017')
plot(ret,main = 'Retornos Google - Ago2004Ago2017')

summary(data_ts)
summary(ret)
}


# 3.1.	Análisis de la serie de rendimientos, prueba de raíces unit --------

install.packages('tseries')

library('tseries')

adf.test(ret) # p-value < 0.05 => no unit-root


# 3.2.	Estimación del modelo ARIMA ----------------------------------------
  
  #1 Correlogramas
{
win.graph(width=4.875, height=2.5,pointsize=8)
par(mfrow=c(1,2))
acf(ret)
pacf(ret)
}
  #2 Modelos Plausibles


{
  ModeloArima001 <- arima(ret, order = c(0,0,1))
  ModeloArima002 <- arima(ret, order = c(0,0,2))
  ModeloArima003 <- arima(ret, order = c(0,0,3))
  ModeloArima004 <- arima(ret, order = c(0,0,4))
  ModeloArima005 <- arima(ret, order = c(0,0,5))
  ModeloArima100 <- arima(ret, order = c(1,0,0))
  ModeloArima200 <- arima(ret, order = c(2,0,0))
  ModeloArima300 <- arima(ret, order = c(3,0,0))
  ModeloArima400 <- arima(ret, order = c(4,0,0))
  ModeloArima500 <- arima(ret, order = c(5,0,0))
  ModeloArima101 <- arima(ret, order = c(1,0,1))
  ModeloArima102 <- arima(ret, order = c(1,0,2))
  ModeloArima103 <- arima(ret, order = c(1,0,3))
  ModeloArima104 <- arima(ret, order = c(1,0,4))
  ModeloArima105 <- arima(ret, order = c(1,0,5))
  ModeloArima201 <- arima(ret, order = c(2,0,1))
  ModeloArima202 <- arima(ret, order = c(2,0,2))
  ModeloArima203 <- arima(ret, order = c(2,0,3))
  ModeloArima204 <- arima(ret, order = c(2,0,4))
  ModeloArima205 <- arima(ret, order = c(2,0,5))
  ModeloArima301 <- arima(ret, order = c(3,0,1))
  ModeloArima302 <- arima(ret, order = c(3,0,2))
  ModeloArima303 <- arima(ret, order = c(3,0,3))
  ModeloArima304 <- arima(ret, order = c(3,0,4))
  ModeloArima305 <- arima(ret, order = c(3,0,5))
  ModeloArima401 <- arima(ret, order = c(4,0,1))
  ModeloArima402 <- arima(ret, order = c(4,0,2))
  ModeloArima403 <- arima(ret, order = c(4,0,3))
  ModeloArima404 <- arima(ret, order = c(4,0,4))
  ModeloArima405 <- arima(ret, order = c(4,0,5))
  ModeloArima501 <- arima(ret, order = c(5,0,1))
  ModeloArima502 <- arima(ret, order = c(5,0,2))
  ModeloArima503 <- arima(ret, order = c(5,0,3))
  ModeloArima504 <- arima(ret, order = c(5,0,4))
  ModeloArima505 <- arima(ret, order = c(5,0,5))
}

  #3 Eleccion del mejor Modelo

{
  TablaValoresAICArima <- AIC(
    ModeloArima001,
    ModeloArima002,
    ModeloArima003,
    ModeloArima004,
    ModeloArima005,
    ModeloArima100,
    ModeloArima200,
    ModeloArima300,
    ModeloArima400,
    ModeloArima500,
    ModeloArima101,
    ModeloArima102,
    ModeloArima103,
    ModeloArima104,
    ModeloArima105,
    ModeloArima201,
    ModeloArima202,
    ModeloArima203,
    ModeloArima204,
    ModeloArima205,
    ModeloArima301,
    ModeloArima302,
    ModeloArima303,
    ModeloArima304,
    ModeloArima305,
    ModeloArima401,
    ModeloArima402,
    ModeloArima403,
    ModeloArima404,
    ModeloArima405,
    ModeloArima501,
    ModeloArima502,
    ModeloArima503,
    ModeloArima504,
    ModeloArima505
      )
}

subset(TablaValoresAICArima, TablaValoresAICArima$AIC == min(TablaValoresAICArima$AIC))

library('lmtest')

coeftest(ModeloArima403)

  #4.	Comprobar el modelo ajustado 

{
win.graph(width=4.875, height=2.5,pointsize=8)
par(mfrow=c(2,2))

plot.ts(residuals(ModeloArima403),main = 'Residuos Arima403')
qqnorm(residuals(ModeloArima403))
qqline(residuals(ModeloArima403))
acf(residuals(ModeloArima403),na.action = na.omit)
pacf(residuals(ModeloArima403),na.action = na.omit)

}


# 3.3.	Análisis de los efectos GARCH --------------------------------------

Box.test(residuals(ModeloArima403)^2,type = 'Ljung-Box',lag=1)

library('FinTS')
ArchTest(residuals(ModeloArima403))

# 3.4.	Estimación del modelo GARCH ----------------------------------------

  # 1. Correlogramas
{
win.graph(width=4.875, height=2.5,pointsize=8)
par(mfrow=c(2,2))

acf(residuals(ModeloArima403),na.action = na.omit)
pacf(residuals(ModeloArima403),na.action = na.omit)

acf(residuals(ModeloArima403)^2,na.action = na.omit)
pacf(residuals(ModeloArima403)^2,na.action = na.omit)
}

  # 2. Modelos Plausibles

{
  ModeloGarch01 <- garch(na.omit(residuals(ModeloArima403)),order = c(0,1),na.action=na.omit)
  ModeloGarch02 <- garch(na.omit(residuals(ModeloArima403)),order = c(0,2),na.action=na.omit)
  ModeloGarch03 <- garch(na.omit(residuals(ModeloArima403)),order = c(0,3),na.action=na.omit)
  ModeloGarch04 <- garch(na.omit(residuals(ModeloArima403)),order = c(0,4),na.action=na.omit)
  ModeloGarch05 <- garch(na.omit(residuals(ModeloArima403)),order = c(0,5),na.action=na.omit)
  ModeloGarch10 <- garch(na.omit(residuals(ModeloArima403)),order = c(1,0),na.action=na.omit)
  ModeloGarch20 <- garch(na.omit(residuals(ModeloArima403)),order = c(2,0),na.action=na.omit)
  ModeloGarch30 <- garch(na.omit(residuals(ModeloArima403)),order = c(3,0),na.action=na.omit)
  ModeloGarch40 <- garch(na.omit(residuals(ModeloArima403)),order = c(4,0),na.action=na.omit)
  ModeloGarch50 <- garch(na.omit(residuals(ModeloArima403)),order = c(5,0),na.action=na.omit)
  ModeloGarch11 <- garch(na.omit(residuals(ModeloArima403)),order = c(1,1),na.action=na.omit)
  ModeloGarch12 <- garch(na.omit(residuals(ModeloArima403)),order = c(1,2),na.action=na.omit)
  ModeloGarch13 <- garch(na.omit(residuals(ModeloArima403)),order = c(1,3),na.action=na.omit)
  ModeloGarch14 <- garch(na.omit(residuals(ModeloArima403)),order = c(1,4),na.action=na.omit)
  ModeloGarch15 <- garch(na.omit(residuals(ModeloArima403)),order = c(1,5),na.action=na.omit)
  ModeloGarch21 <- garch(na.omit(residuals(ModeloArima403)),order = c(2,1),na.action=na.omit)
  ModeloGarch22 <- garch(na.omit(residuals(ModeloArima403)),order = c(2,2),na.action=na.omit)
  ModeloGarch23 <- garch(na.omit(residuals(ModeloArima403)),order = c(2,3),na.action=na.omit)
  ModeloGarch24 <- garch(na.omit(residuals(ModeloArima403)),order = c(2,4),na.action=na.omit)
  ModeloGarch25 <- garch(na.omit(residuals(ModeloArima403)),order = c(2,5),na.action=na.omit)
  ModeloGarch31 <- garch(na.omit(residuals(ModeloArima403)),order = c(3,1),na.action=na.omit)
  ModeloGarch32 <- garch(na.omit(residuals(ModeloArima403)),order = c(3,2),na.action=na.omit)
  ModeloGarch33 <- garch(na.omit(residuals(ModeloArima403)),order = c(3,3),na.action=na.omit)
  ModeloGarch34 <- garch(na.omit(residuals(ModeloArima403)),order = c(3,4),na.action=na.omit)
  ModeloGarch35 <- garch(na.omit(residuals(ModeloArima403)),order = c(3,5),na.action=na.omit)
  ModeloGarch41 <- garch(na.omit(residuals(ModeloArima403)),order = c(4,1),na.action=na.omit)
  ModeloGarch42 <- garch(na.omit(residuals(ModeloArima403)),order = c(4,2),na.action=na.omit)
  ModeloGarch43 <- garch(na.omit(residuals(ModeloArima403)),order = c(4,3),na.action=na.omit)
  ModeloGarch44 <- garch(na.omit(residuals(ModeloArima403)),order = c(4,4),na.action=na.omit)
  ModeloGarch45 <- garch(na.omit(residuals(ModeloArima403)),order = c(4,5),na.action=na.omit)
  ModeloGarch51 <- garch(na.omit(residuals(ModeloArima403)),order = c(5,1),na.action=na.omit)
  ModeloGarch52 <- garch(na.omit(residuals(ModeloArima403)),order = c(5,2),na.action=na.omit)
  ModeloGarch53 <- garch(na.omit(residuals(ModeloArima403)),order = c(5,3),na.action=na.omit)
  ModeloGarch54 <- garch(na.omit(residuals(ModeloArima403)),order = c(5,4),na.action=na.omit)
  ModeloGarch55 <- garch(na.omit(residuals(ModeloArima403)),order = c(5,5),na.action=na.omit)
}

  #3. Seleccion del Mejor Modelo
{
  TablaValoresAICGarch <-
    AIC(
      ModeloGarch01,
      ModeloGarch02,
      ModeloGarch03,
      ModeloGarch04,
      ModeloGarch05,
      ModeloGarch10,
      ModeloGarch20,
      ModeloGarch30,
      ModeloGarch40,
      ModeloGarch50,
      ModeloGarch11,
      ModeloGarch12,
      ModeloGarch13,
      ModeloGarch14,
      ModeloGarch15,
      ModeloGarch21,
      ModeloGarch22,
      ModeloGarch23,
      ModeloGarch24,
      ModeloGarch25,
      ModeloGarch31,
      ModeloGarch32,
      ModeloGarch33,
      ModeloGarch34,
      ModeloGarch35,
      ModeloGarch41,
      ModeloGarch42,
      ModeloGarch43,
      ModeloGarch44,
      ModeloGarch45,
      ModeloGarch51,
      ModeloGarch52,
      ModeloGarch53,
      ModeloGarch54,
      ModeloGarch55
        )
}

subset(TablaValoresAICGarch, TablaValoresAICGarch$AIC == min(TablaValoresAICGarch$AIC))

coeftest(ModeloGarch15)

summary(ModeloGarch15)


  # 4. Comprobacion del modelo ajustado
{
  win.graph(width=4.875, height=2.5,pointsize=8)
  par(mfrow=c(2,2))
  
  plot.ts(residuals(ModeloGarch15),main = 'Residuos Garch15')
  qqnorm(residuals(ModeloGarch15))
  qqline(residuals(ModeloGarch15))
  acf(residuals(ModeloGarch15)^2,na.action = na.omit)
  pacf(residuals(ModeloGarch15)^2,na.action = na.omit)
} 


# 3.5.	Presentación Modelo ARIMA-GARCH estimado ---------------------------
{
fit <- fitted.values(ModeloArima403)
fitgarch <- fitted.values(ModeloGarch15)[,1]
low <- fit - (1.96 * fitgarch)
high <- fit + (1.96 * fitgarch)

win.graph(width=4.875, height=2.5,pointsize=8)
plot(ret, 
      main = 'Google: Retornos VS Ajuste de Retornos con Modelo ARIMA(4,0,3)-GRACH(1,5)', 
      type = 'l')  
lines(low,col = 'purple') 
lines(high,col = 'purple')  
lines(fit,col = 'red')
}
