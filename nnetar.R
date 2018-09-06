library(forecast); library(tidyverse); library(prophet)

data(lynx) #'famous' canadian lynx dataset
head(lynx) #annual numbers for lynx trapping in canada
#?lynx

# autoplot(forecast(lynx))  #mega-fail
autoplot(forecast(auto.arima(lynx), h = 20))
#starting to see why its a 'famous' dataset

class(lynx)
attributes(lynx)
ds <- as.numeric(time(lynx))
ds <- as.Date(ds, format = '%Y')

ds <- lubridate::ymd(ds, truncated = 2L)

class(ds)
length(ds)
y <- as.numeric(lynx)
class(y)
length(y)
head(y)

#that dyplot from prophet was pretty cool, can we try that
# would like to use prophet, but need dates in dataframe
lynxDF <- data.frame(ds, y)
head(lynxDF)

plot(y ~ ds, lynxDF, type ='l')

m <- prophet(lynxDF)
future <- make_future_dataframe(m, periods = 20)
forecast <- predict(m, future)

plot(m, forecast)
dyplot.prophet(m, forecast)
#prophet is NOT made for annual measurements, apparently

############################
#https://robjhyndman.com/hyndsight/nnetar-prediction-intervals/
############################

set.seed(2015)
fit <- nnetar(lynx, lambda=0.5)
fit


sim <- ts(matrix(0, nrow=20, ncol=9), start=end(lynx)[1]+1)

for(i in seq(9))
  sim[,i] <- simulate(fit, nsim=20)

autoplot(lynx) + forecast::autolayer(sim)

fcast <- forecast(fit, PI=TRUE, h=20)
autoplot(fcast)
#this looks pretty darn good compared to the others

