library(vars)
library(TSstudio)
library(dplyr)
library(forecast)
library(corrplot)
library(ggplot2)
library(MASS)
library(tseries)
library(glmnet)
library(pls)
library(boot)
library(leaps)
nigeria <- read.csv("C:\\Users\\msaul\\OneDrive\\Documents\\Documents\\nigeria project.csv")
# looking at data
head(nigeria)
# checking variable type
str(nigeria)
# getting rid of data and making data a timeseries
ts.nigeria <- nigeria
ts.nigeria$Year <-NULL
ts.nigeria$Date <- NULL
ts.nigeria$Month <-NULL
tseries <- ts(ts.nigeria, start = c(2004,1),frequency = 12)
summary(tseries)
plot(tseries)
# plotting timeseries all together
ts_plot(tseries, title = "Nigeria", Xtitle = "Date"      )
par(mfrow=c(2,2))
ts_plot(tseries[, "violence"], title = "Violence", Xtitle = "Year", Ytitle = "No of Events")
ts_plot(tseries[, "Food"], title = "Food Inflation", Xtitle = "Year", Ytitle = "Rate")
ts_plot(tseries[, "All.Items"], title = "Gen. Inflation", Xtitle = "Year", Ytitle = "Rate")
ts_plot(tseries[, "USDFX"], title = "Value of Dollor Against Naira", Xtitle = "Year", Ytitle = "Rate")
# visualizing the relationships
pairs(tseries)
# correlation matrix of variables
cor.n <- cor(tseries)
corrplot::corrplot(cor.n)
# lin reg predicting violence
lm.violence <- lm(violence~., data=tseries)
lm.violence
## relationship seen
summary(lm.violence)
# looking at the linearity and outliers present
par(mfrow=c(2,2))
plot(lm.violence)
confint(lm.violence)
poly(tseries)         

# trying to actually do the time series stuff
decom <- decompose(tseries, type = "mult")
forecast::autoplot(decom)
# all of it separated
par(mfrow=c(2,2))
plot(decompose(tseries[, "violence"]))
plot(decompose(tseries[, "All.Items"]))
plot(decompose(tseries[, "Food"]))
plot(decompose(tseries[, "USDFX"]))
# acf : auto correlation
acf(tseries) # ask about this
pacf(tseries)
# box test
Box.test(tseries[,"All.Items"])
Box.test(tseries[,"violence"])
Box.test(tseries[,"Food"])
Box.test(tseries[,"USDFX"]) #none are stationary
#adf test to see if it is stationary
adf.test(tseries[,"All.Items"])
adf.test(tseries[,"violence"])
adf.test(tseries[,"Food"])
adf.test(tseries[,"USDFX"]) # none are stationary
# trying to difference by seeing how many needed
dUSDFX<-diff(tseries[,"USDFX"], differences =2.4, lag = 72) #lagging to 2016
dFood<-diff(tseries[,"Food"], differences =2.4, lag = 72)
dviolence<-diff(tseries[,"violence"], differences =2.4, lag=72)
dInflation<-diff(tseries[,"All.Items"], differences =2.4, lag = 72) #diffed everything twice to make mroe stationary
difftseries <- cbind("violence" = dviolence, "All.Items" = dInflation,
      "Food" = dFood, "USDFX" = dUSDFX)
# trying the arima thing out and forecasting  5 years
acf(difftseries)
#checking for white noise
mymodel <-auto.arima(difftseries[,"violence"])
mymodel2 <-auto.arima(difftseries[,"All.Items"])
mymodel3 <-auto.arima(difftseries[,"USDFX"])
mymodel4 <-auto.arima(difftseries[,"Food"])
forecast(mymodel)
forecast(mymodel2)
forecast(mymodel3)
checkresiduals(mymodel4)
forecast(mymodel)
forecast(mymodel2)
forecast(mymodel3)
forecast(mymodel4)
mymodel
mymodel2
mymodel3
mymodel4
# plotting arima things
par(mfrow=c(2,2))
plot(forecast(mymodel4, h=12), xlab = "ARIMA Food Inf")
plot(forecast(mymodel3,h=12), xlab = "ARIMA USDFX")
plot(forecast(mymodel2,h=12), xlab = "Inflation")
plot(forecast(mymodel,h=12), xlab = "violence")
# looking at arima a little closer
summary(mymodel)
summary(mymodel2)
summary(mymodel3)
summary(mymodel4)
#trying out VAR
lagselect <- VARselect(difftseries, lag.max = 15, type = "const")
lagselect$selection # I CHOSE 2
varmod1 <- VAR(difftseries, p=2, type = "const", season = NULL)
summary(varmod1) #model r and p look pretty good for violence
serial <- serial.test(varmod1)
serial # pval seems high enough to have no autocorrelation i think
arch <- arch.test(varmod1)
arch # I think this shows heteroscedasticity
normal <- normality.test(varmod1)
normal
cause.v <- causality(varmod1, cause = "violence")
cause.v #idk what this means. i guess violence cant forecast other stuff
forecastv <- predict(varmod1, n.ahead = 12, ci=.95) #forcasting 2 years into time
forecastm <- forecast(varmod1, h =12) #12 mo forecast
# individual fancharts
fanchart(forecastv, names = "violence", xlab = "Horizon", colors = "blue")
fanchart(forecastv, names = "Food", xlab = "Horizon")
fanchart(forecastv, names = "All.Items", xlab = "Horizon")
fanchart(forecastv, names = "USDFX", xlab = "Horizon")
#combined fancharts
fanchart(forecastv, plot.type = "multiple", nc=2, colors = "orange") #it looks like everything is increasing
# has dates
autoplot(forecastm)
autoplot(forecastv)
