# Nigerian-Inflation-Project
[Effects of Economic Factors on Rates of Violence in Nigeria; Final STAT 5000 (1).pdf](https://github.com/msaulaw/Nigerian-Inflation-Project/files/14877557/Effects.of.Economic.Factors.on.Rates.of.Violence.in.Nigeria.Final.STAT.5000.1.pdf)
# Introduction

## Background

I chose this topic because I saw a graph of the violence in Nigeria presented by The Economist, which made me realize that the violence might be connected to rising inflation and general food prices.


Inflation is a serious challenge that affects everyone. It may also contribute to the unrest and violence that we witness in some countries, such as Nigeria. Climate change is another urgent issue that cannot be overlooked, especially since it will have disproportionate impacts on the working class of the global south, with global repercussions on the economies of the world, due to globalization. For example, in Nigeria, people spend 97.4% of their income on food. Inflation also has differential effects on various segments of the population, which affects their employment and access to basic needs.

## Purpose of Analysis

I want to see if violence in Nigeria is in any way correlated with the rise in inflation and food prices, as well as the FX rate changes. I also wanted to see if these variables can be used to predict violence in the future.

## Data Description

This data is from the Nigerian government. I cleaned the data in Excel and saved it as a .csv file to then import into RStudio.

I began by checking the data and removing the months, years, and dates to replace them with a time series formula. To avoid dealing with null values, I began the time series in 2004 and ended it in October 2024.

```{r cleaning}
head(nigeria)
# checking variable type
str(nigeria)
ts.nigeria <- nigeria
ts.nigeria$Year <-NULL
ts.nigeria$Date <- NULL
ts.nigeria$Month <-NULL
tseries <- ts(ts.nigeria, start = c(2004,1),frequency = 12)
summary(tseries)
```

I then plotted the variables on a single graph. The variables I looked at were the dollar's value against the Naira, food inflation and general inflation, which are the small lines, and the total number of acts of political violence each month.

```{r plot, echo=FALSE}
ts_plot(tseries, title = "Nigeria", Xtitle = "Date"      )
```

I also plotted each variable separately to better understand the series' patterns. I saw that there was generally an increase in each category.

The value of dollar has had the most steady increase with a very sharp increase corresponding with the recent election and decision to let the currency "float" and be determined soley by market activity.

```{r plots, echo=FALSE}
ts_plot(tseries[, "violence"], title = "Violence", Xtitle = "Year", Ytitle = "No of Events")
ts_plot(tseries[, "Food"], title = "Food Inflation", Xtitle = "Year", Ytitle = "Rate")
ts_plot(tseries[, "All.Items"], title = "Gen. Inflation", Xtitle = "Year", Ytitle = "Rate")
ts_plot(tseries[, "USDFX"], title = "Value of Dollor Against Naira", Xtitle = "Year", Ytitle = "Rate")
```

I then visualized the relationships using `pairs()` and plotted a correlation matrix. From this plot, I can see that there is a strong relationship between food and general inflation which I will ignore since food inflation is a factor of general inflation. After that, USDFX and violence have a strong relationship. From the correlation plot, violence, and food inflation seem to be the least correlated.

```{r pairs}
pairs(tseries)
```

```{r corr, echo=FALSE}
cor.n <- cor(tseries)
corrplot::corrplot(cor.n)
```

# Methods and Analysis

## Linear Regression

I then decided to use a linear regression model to see if inflation, food inflation, USD rates had any effect on the rate of violence that occurred. The overall model had an r^{2} of 77% and a small p-value, which is decent but not the best. From the model, I gathered the by p-value, t-value, and standard error, USD increases had a positive effect on the rate of violence, which is bad, and I hope something changes with the way they are handling the currency and the other variables that are affecting the rate of violence in the country. It was, however, somewhat of a surprise that the model showed a negative relationship with inflation. This is surprising the me since it has been reported that Nigerians spend 96% of their income on food supplies. I don't know how to explain that, but I do know that the value of the dollar is followed closely by merchants and sellers of various items.

```{r lm}
lm.violence <- lm(violence~., data=tseries)
lm.violence
summary(lm.violence)
```

I then plotted the linear regression. The residual v fitted is curved with a significant number of outliers and appears to signify a nonlinear relationship of the predictors. This model doesn't seem to be that good when looking at the q-q plot as it indicates a lack of normality. Therefore, I don't think linear regression is good for modeling. I will now use methods specifically for time series.

```{r plots 2, echo=FALSE}
par(mfrow=c(2,2))
plot(lm.violence)
confint(lm.violence)
```

### Set Up for ARIMA and VAR

I first decomposed the variables together and seperately.

```{r decom}
decom <- decompose(tseries, type = "mult")
```

```{r decom 2, echo=FALSE}
forecast::autoplot(decom)
par(mfrow=c(2,2))
plot(decompose(tseries[, "violence"]))
plot(decompose(tseries[, "All.Items"]))
plot(decompose(tseries[, "Food"]))
plot(decompose(tseries[, "USDFX"]))
```

Then, I viewed the auto correlation present in the data. From the above graphs of the individual data, the variables seem to be nonstationary, so they depend on time. I also performed the box test, and the p-values were very small, so I can conclude that the data is nonstationary. But I don't think there is much seasonality outside of the inflations.

```{r acf, echo=FALSE}
acf(tseries) # ask about this
pacf(tseries)
```

I also performed the Box-Pierce test and Augmented Dickey-Fuller test to test the stationarity of the variables. They were not stationary.

```{r box}
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
```

So, I second order differenced all the variables for a new acf plot that looks more stationary and lagged them by a few years. The diagonal ones are the ones to look at.

```{r diff}
dUSDFX<-diff(tseries[,"USDFX"], differences =2.4, lag = 72) #lagging to 2016
dFood<-diff(tseries[,"Food"], differences =2.4, lag = 72)
dviolence<-diff(tseries[,"violence"], differences =2.4, lag=72)
dInflation<-diff(tseries[,"All.Items"], differences =2.4, lag = 72) #diffed everything twice to make mroe stationary and made new TS
difftseries <- cbind("violence" = dviolence, "All.Items" = dInflation,
      "Food" = dFood, "USDFX" = dUSDFX)
```

```{r d, echo= FALSE}
acf(difftseries)
```

After differencing and lagging, the models I made passed the Ljung-Box test.

## ARIMA Models
I then modeled the variables.

```{r models, echo=TRUE}
mymodel <-auto.arima(difftseries[,"violence"])
mymodel2 <-auto.arima(difftseries[,"All.Items"])
mymodel3 <-auto.arima(difftseries[,"USDFX"])
mymodel4 <-auto.arima(difftseries[,"Food"])
```

```{r ltest}
checkresiduals(mymodel)
checkresiduals(mymodel2)
checkresiduals(mymodel3)
checkresiduals(mymodel4)
```

I then forecasted each variable and plotted them.
```{r fore arima, echo=TRUE}
forecast(mymodel)
forecast(mymodel2)
forecast(mymodel3)
forecast(mymodel4)
```

Afterwards, I plotted them one year forward. This is the final model. It seems like violence will still be on the increase. However, dollar hopefully seems to be coming down a well as some disinflation in the overall food inflation.

```{r plot arima, echo=FALSE}
par(mfrow=c(2,2))
plot(forecast(mymodel4, h=12), xlab = "ARIMA Food Inf")
plot(forecast(mymodel3,h=12), xlab = "ARIMA USDFX")
plot(forecast(mymodel2,h=12), xlab = "Inflation")
plot(forecast(mymodel,h=12), xlab = "violence")
```

## VAR Models
I chose a lag of 2
```{r vlag}
lagselect <- VARselect(difftseries, lag.max = 15, type = "const")
lagselect$selection # I CHOSE 2
```

I then created the VAR model with a lag of 2. The R^{2} and the p-values of each are significant, showing that the model is good.
```{r var sum}
varmod1 <- VAR(difftseries, p=2, type = "const", season = NULL)
summary(varmod1)
```

I performed the Portmanteau test and saw no autocorrelation. The ARCH test shows conditional heteroscedasticity. The normality test showed that the data is not normally distributed, and residuals aren't all in the confidence intervals. 

The Granger test shows that violence does not cause the other variables.

```{r var t}
serial <- serial.test(varmod1)
serial # pval seems high enough to have no autocorrelation i think
arch <- arch.test(varmod1)
arch # I think this shows heteroscedasticity
normal <- normality.test(varmod1)
normal
cause.v <- causality(varmod1, cause = "violence")
cause.v
```

Afterward, I forecasted a year into the future. I tried the `predict()` and `forecast()` functions.

```{r for var}
forecastv <- predict(varmod1, n.ahead = 12, ci=.95) 
forecastm <- forecast(varmod1, h =12) #12 mo forecast
```

Below are the respective forecast plots.
```{r fan and auto, echo=FALSE}
fanchart(forecastv, plot.type = "multiple", nc=2, colors = "orange") #it looks like everything is increasing
autoplot(forecastm)
```

# Conclusion
After reviewing the plots of the ARIMA model, I see that violence will rise, but not very drastically. The value of the Dollar against Naira seems to drop dramatically in the next 12 months, which might help point to the new currency rules actually working. Both inflation variables show an overall constant growth that might be eased if the Dollar actually drops.

The VAR model shows a similar pattern but is not as dramatic and is smoother.

# References
https://www.cbn.gov.ng/rates/inflrates.asp

https://www.cbn.gov.ng/rates/ExchRateByCurrency.asp

https://acleddata.com/africa/

https://neusroom.com/the-struggle-to-eat-how-nigerians-are-spending-almost-all-their-income-on-food/
