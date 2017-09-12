

# 3.	Estimación de series de tiempo heterocedásticas, una aplicaci --------

{
#install.packages('readr')

library('readr')

data <- read.csv('https://raw.githubusercontent.com/jorgepaguay86/EstadisticaAplicada_TFM_UGR/master/SerieDeTiempo_Google_Ago2004Ago2017Semanal.csv')

data$Date <- as.Date(data$Date)

#data <- subset(data,data$Date <= '2016-01-01')

data_ts <- ts(data$AdjClose) #ojo

ret = diff(log(data_ts))*100

win.graph(width=5.875, height=3.5,pointsize=8)
plot(data_ts,main = 'Precio de Cierre Ajustado Google - Ago2004Ago2017')

win.graph(width=5.875, height=3.5,pointsize=8)
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
win.graph(width=5.875, height=3.5,pointsize=8)
par(mfrow=c(1,2))
acf(ret)
pacf(ret)
}
  #2 Modelos Plausibles

{
  ModeloArima200 <- arima(ret, order = c(2,0,0))
  ModeloArima002 <- arima(ret, order = c(0,0,2))
  ModeloArima001 <- arima(ret, order = c(0,0,1))
  ModeloArima101 <- arima(ret, order = c(1,0,1))
  ModeloArima102 <- arima(ret, order = c(1,0,2))
  ModeloArima201 <- arima(ret, order = c(2,0,1))
  ModeloArima202 <- arima(ret, order = c(2,0,2))
  ModeloArima111 <- arima(ret, order = c(1,1,1))  
  ModeloArima011 <- arima(ret, order = c(0,1,1)) 
  ModeloArima112 <- arima(ret, order = c(1,1,2))
  
}

  
{
  library('lmtest')
  
  TablaDiagnosticoArima <- 
    rbind.data.frame(
    list('ModeloArima200' , all(coeftest(ModeloArima200)[,4] <= 0.05),AIC(ModeloArima200)),          
    list('ModeloArima002' , all(coeftest(ModeloArima002)[,4] <= 0.05),AIC(ModeloArima002)),  
    list('ModeloArima001' , all(coeftest(ModeloArima001)[,4] <= 0.05),AIC(ModeloArima001)),  
    list('ModeloArima101' , all(coeftest(ModeloArima101)[,4] <= 0.05),AIC(ModeloArima101)),
    list('ModeloArima102' , all(coeftest(ModeloArima102)[,4] <= 0.05),AIC(ModeloArima102)),
    list('ModeloArima201' , all(coeftest(ModeloArima201)[,4] <= 0.05),AIC(ModeloArima201)),
    list('ModeloArima202' , all(coeftest(ModeloArima202)[,4] <= 0.05),AIC(ModeloArima202)),
    list('ModeloArima111' , all(coeftest(ModeloArima111)[,4] <= 0.05),AIC(ModeloArima111)),
    list('ModeloArima011' , all(coeftest(ModeloArima011)[,4] <= 0.05),AIC(ModeloArima011)),
    list('ModeloArima112' , all(coeftest(ModeloArima112)[,4] <= 0.05),AIC(ModeloArima112))
    
            )
  
  names(TablaDiagnosticoArima) <- c('Modelo','AllCoef<0.05?','AIC')
  
  TablaDiagnosticoArima <- TablaDiagnosticoArima[order(-TablaDiagnosticoArima$`AllCoef<0.05`,TablaDiagnosticoArima$AIC),] 
  
  head(TablaDiagnosticoArima)
  
  coeftest(ModeloArima101)
  
  ModeloArimaElegido <- ModeloArima101
  #confint(ModeloArima101, level = 0.9)
  }

#3.	Validacion

{
win.graph(width=5.875, height=3.5,pointsize=8)
par(mfrow=c(2,2))

plot.ts(residuals(ModeloArimaElegido),main = 'Residuos, Modelo en Media Elegido, Arima101')
qqnorm(residuals(ModeloArimaElegido))
qqline(residuals(ModeloArimaElegido))
acf(residuals(ModeloArimaElegido),na.action = na.omit)
pacf(residuals(ModeloArimaElegido),na.action = na.omit)

}

# 3.3.	Análisis de los efectos GARCH --------------------------------------

Box.test(residuals(ModeloArimaElegido)^2,type = 'Ljung-Box',lag = 12)

library('FinTS')
ArchTest(residuals(ModeloArimaElegido),lag = 5)

# 3.4.	Estimación del modelo GARCH ----------------------------------------

  # 1. Correlogramas
{
win.graph(width=5.875, height=3.5,pointsize=8)
par(mfrow=c(2,2))

acf(residuals(ModeloArimaElegido),na.action = na.omit)
pacf(residuals(ModeloArimaElegido),na.action = na.omit)
acf(residuals(ModeloArimaElegido)^2,na.action = na.omit)
pacf(residuals(ModeloArimaElegido)^2,na.action = na.omit)
}

  # 2. Modelos Plausibles

{
  library('fGarch')
  library('forecast')
  library('TSA')
  
  ModeloGarch11 <- garch(na.omit(residuals(ModeloArimaElegido)),order = c(1,1),na.action=na.omit)
  ModeloGarch02 <- garch(na.omit(residuals(ModeloArimaElegido)),order = c(0,2),na.action=na.omit)
  
  ModeloGarch22 <- garch(na.omit(residuals(ModeloArimaElegido)),order = c(2,2),na.action=na.omit)
 
}

  #3. Seleccion del Mejor Modelo

{
  library('lmtest')
  
  TablaDiagnosticoGarch <- 
    rbind.data.frame(

      list('ModeloGarch11' , all(coeftest(ModeloGarch11)[,4] <= 0.05),AIC(ModeloGarch11)),
      list('ModeloGarch02' , all(coeftest(ModeloGarch02)[,4] <= 0.05),AIC(ModeloGarch02)),
      list('ModeloGarch22' , all(coeftest(ModeloGarch22)[,4] <= 0.05),AIC(ModeloGarch22))
          )
  
  names(TablaDiagnosticoGarch) <- c('Modelo','AllCoef<0.05?','AIC')
  
  TablaDiagnosticoGarch <- TablaDiagnosticoGarch[order(-TablaDiagnosticoGarch$`AllCoef<0.05`,TablaDiagnosticoGarch$AIC),] 
  
  head(TablaDiagnosticoGarch)
  coeftest(ModeloGarch11)
  
  ModeloGarchElegido <- ModeloGarch11
}

  # 4. Comprobacion del modelo ajustado
{
  win.graph(width=4.875, height=2.5,pointsize=8)
  par(mfrow=c(2,2))
  
  plot.ts(residuals(ModeloGarchElegido),main = 'Residuos, Modelo en Varianza, Garch(1,1)')
  qqnorm(residuals(ModeloGarchElegido))
  qqline(residuals(ModeloGarchElegido))
  acf(residuals(ModeloGarchElegido)^2,na.action = na.omit)
  pacf(residuals(ModeloGarchElegido)^2,na.action = na.omit)
} 




# 3.5.	Presentación Modelo ARIMA-GARCH estimado ---------------------------

{
  library('forecast')
  
fit <- fitted.values(ModeloArimaElegido)
fitgarch <- fitted.values(ModeloGarchElegido)[,1]
low <- fit - (1.96 * fitgarch)
high <- fit + (1.96 * fitgarch)

win.graph(width=4.875, height=2.5,pointsize=8)
plot(ret, 
      main = 'Google: Retornos VS Ajuste de Retornos con Modelo ARIMA(1,0,1)-GRACH(1,1)', 
      type = 'l')  
lines(low,col = 'purple') 
lines(high,col = 'purple')  
lines(fit,col = 'red')
}




