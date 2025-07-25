#Loading the libraries  

library(forecast) 
library(strucchange) 
library(ggplot2) 
library(tseries) 

#India dataset 

india = scan() 
india = ts(india, start = 2000) 

#plotting the data and modeling 

autoplot(india, ylab = "   ") 
tsdisplay(india) 
india.ARIMA = auto.arima(india, trace = T, stepwise = F, approximation = F) 

#2,1,0 and 1,1,0 

#Final Model 
india.Arima.gdppc = Arima(india, order=c( ,  , ), include.drift = TRUE)  
summary(india.Arima.gdppc) 

#holt modEl 
india.hlt=holt(india,h=5) 
summary(india.hlt) 
autoplot(india.hlt) 

#Diagonastics
checkresiduals(india.ARIMA)
checkresiduals(india.hlt) 
#normality of the residual 
shapiro.test(india.ARIMA$residuals)
jarque.bera.test(india.ARIMA$residuals) 
jarque.bera.test(india.hlt$residuals) 

#hetroscedasticity of sq. residuals 
acf(india.ARIMA$residuals^2) 
pacf(india.ARIMA$residuals^2) 

#forecasting 
arimafore = forecast(india.ARIMA, h=5)
arimafore autoplot(arimafore) 

#accuracy 

ts_train=window(india, start= 2000, end=2015)
ts_test=window(india, start= 2016, end=2023)
ARIMA11=Arima(ts_train, order=c( , , )) 
ARIMA12=Arima(ts_train, order=c(  , , )) 
ARIMAF1=forecast(ARIMA11, h=8) 
ARIMAF2=forecast(ARIMA12, h=8) 
accuracy(ARIMAF1, ts_test) 
accuracy(ARIMAF2, ts_test) 



#structural break 
model1 = Fstats(india~1, from= 0.01)
sctest(model1) 
#H0 There is no structural break in a series. here there are structural change
strucchange::breakpoints(india~1) 


#Bai perron test.
bp.india = breakpoints(india~1, h=3) 
summary(bp.india) 
coefficients(bp.india) 
