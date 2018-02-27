#Midterm 1:
rm(list=ls())

table <- read.table("sea_ice_montly.txt")

table <- as.matrix(table)

year <- table[,1]

n <- length(year)

data <- table[,2:13]

y <- matrix(t(data),ncol=1)

y.ts <- ts(y,start = year[1],frequency = 12)

plot(y.ts,ylab="Artic Sea Index (km^2)")

# 1- Exploratory Data Analysis

# This dataset represents the Arctic Sea Ice Index, which
# is a measure of the sea ice extent, based on satellite imaging. The data are
# monthly, from January 1979 to December 2017, and are in square km.
# The data is reported by the surface of ice extent on the sea
# We definitely observe a strong seasonality in the time series plot of our data
# The strong seasonality is due to natural climate variations during the year
# For example, A small temperature increase at the poles leads to still 
#greater warming over time, making the poles the most sensitive regions to 
#climate change on Earth. According to scientific measurements, both the thickness 
#and extent of summer sea ice in the Arctic have shown a dramatic decline over the past thirty years. 
#This is consisistent with observations of a warming Arctic. The loss of sea ice also has the potential 
#to accelerate global warming trends and to change climate patterns.

#Display seasonal and Annual Trends
data.annual <- rowMeans(data, na.rm = TRUE)

data.monthly <- colMeans(data, na.rm = TRUE)

par(mfrow=c(1,2))

plot(data.monthly,type="n",ylab="Artic Sea Index(Km^2)",xlab="Month")
lines(data.monthly)

plot(year,data.annual,type="n",ylab="Artic Sea Index(km^2)",xlab="Year")
lines(year,data.annual)

# analyzing the trend: there is seasonality, we need the harmonics

h1_sin=sin(2*pi*(0:11)/12)
h1_cos=cos(2*pi*(0:11)/12)

h1_sin_X=c(rep(h1_sin,n))
h1_cos_X=c(rep(h1_cos,n))

h2_sin=sin(2*pi*2*(0:11)/12)
h2_cos=cos(2*pi*2*(0:11)/12)

h2_sin_X=c(rep(h2_sin,n))
h2_cos_X=c(rep(h2_cos,n))

h3_sin=sin(2*pi*3*(0:11)/12)
h3_cos=cos(2*pi*3*(0:11)/12)

h3_sin_X=c(rep(h3_sin,n))
h3_cos_X=c(rep(h3_cos,n))

year.trend.lin=kronecker(year,rep(1,12))
year.trend.quad=kronecker(year^2,rep(1,12))

mod_h=lm(y~year.trend.lin+year.trend.quad+h1_sin_X+h1_cos_X)
mod_h2=lm(y~year.trend.lin+year.trend.quad+h1_sin_X+h1_cos_X+h2_sin_X+h2_cos_X)
mod_h3=lm(y~year.trend.lin+year.trend.quad+h1_sin_X+h1_cos_X+h2_sin_X+h2_cos_X+h3_sin_X+h3_cos_X)


summary(mod_h)
summary(mod_h2)
summary(mod_h3)

# We notice that the linear and quadratic are not significant by looking at p-values
# In fact, p-values for both the linear and quadratic trends are fairly big thus, insignificant
# We also notice that the seasonality is significant for three harmonics, because
# they have very small p-values.
# We choose mod_h3 which seems to be the best based on a very high R^2 and adjusted R^2 and the lowest residual error


#3- ARIMA modelling

#Check Reiduals
res=y-mod_h3$fitted.values
par(mfrow=c(1,2))
plot(y)
lines(mod_h3$fitted.values,lty=1,col="red",lwd=2)
plot(res)


# predicting the trend, for later

library(forecast)

#Fit residuals into ARIMA (p,q) model
res.ts=ts(res,start=year[1],frequency=12)
fit=auto.arima(res.ts)
fit

par(mfrow=c(1,1))
acf(fit$residuals)

# We can clearly see that, the autocorrelation plot of the residuals has a 
#significant lag at 0 and after that there is no more significant lags
# We can conclude that the model is very good because the autocorrelation plot
#shows that the residuals are white noise

#4- Forecast

year.trend.lin=kronecker(2018:2019,rep(1,12))

year.trend.quad=kronecker(c(2018^2,2019^2),rep(1,12))

h1_sin_X=c(rep(h1_sin,2))
h1_cos_X=c(rep(h1_cos,2))
h2_sin_X=c(rep(h2_sin,2))
h2_cos_X=c(rep(h2_cos,2))
h3_sin_X=c(rep(h3_sin,2))
h3_cos_X=c(rep(h3_cos,2))


beta.predict=rbind(year.trend.lin,year.trend.quad,h1_sin_X,h1_cos_X,h2_sin_X,h2_cos_X,h3_sin_X,h3_cos_X)
trend.predict=forecast(mod_h3,newdata=data.frame(t(beta.predict)))

forc=forecast(fit,h=24)

forc$x=forc$x+mod_h3$fitted.values
forc$mean=forc$mean+trend.predict$mean
forc$lower=forc$lower+trend.predict$mean
forc$upper=forc$upper+trend.predict$mean

par(mfrow=c(1,1))
plot(forc,include=100,main="Forecast of Sea Artic Index",ylab="Sea Artic Index (km^2)",xlab="Year")


# Comments about Reliability:
#First of all let's discuss the origin of the data, and also check if the data source
#is reliable.
#For this forecast, we used an 80% and 95% confidence intervals
# The data was aquired using satellites which take monthly images of the sea ice extent
# The artic sea ice extent is almost impossible to accurately measure from Earth's surface
# Therefore the measurement of the data using satellite imaging is quite reliable.
# Finally our model is quite good because it captured the trend and the seasonality
# in the data since the residuals from our model are white noise series.
# I think that our forecast would only be good for up to 1 or 2 years because
# in the long run (5,10,15 years or more) it's hard to predict this type of phenomenon
# climate phenomena are extremely volatile and any sudden event such as 
# climate change can significantly impact the forecast even though the collected data is quite reliable.

