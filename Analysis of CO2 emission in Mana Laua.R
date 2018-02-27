rm(list=ls())

this.dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

table=read.table("Mauna_Loa_CO2.txt")
table=as.matrix(table)
year=table[,1]
n=length(year)
data=table[,2:13]

y=matrix(t(data),ncol=1)
y.ts=ts(y,start=year[1],frequency=12)
plot(y.ts,ylab="CO2 (ppm)")

#eploratory analysis: monthly profiles, yearly averages
data.annual=rowMeans(data, na.rm = TRUE)
data.monthly=colMeans(data, na.rm = TRUE)

par(mfrow=c(1,2))

plot(data.monthly,type="n",ylab="CO2(ppm)",xlab="Month")
lines(data.monthly)

plot(year,data.annual,type="n",ylab="CO2(ppm)",xlab="Year")
lines(year,data.annual)


# analyzing the trend: there is seasonality, we nee the harmonics

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

res=y-mod_h3$fitted.values
par(mfrow=c(1,2))
plot(y)
lines(mod_h3$fitted.values,lty=1,col="red",lwd=2)
plot(res)

# predicting the trend, for later
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


library(forecast)

res.ts=ts(res,start=year[1],frequency=12)
fit=auto.arima(res.ts)
fit

par(mfrow=c(1,1))
acf(fit$residuals)

forc=forecast(fit,h=24)

forc$x=forc$x+mod_h3$fitted.values
forc$mean=forc$mean+trend.predict$mean
forc$lower=forc$lower+trend.predict$mean
forc$upper=forc$upper+trend.predict$mean

par(mfrow=c(1,1))
plot(forc,include=100,main="Forecast of CO2",ylab="CO2 (ppm)",xlab="Year")


