# from Kaggle user kailex
# https://www.kaggle.com/kailex/time-series-with-parallel-auto-arima-and-tsclean
# data from https://www.kaggle.com/c/web-traffic-time-series-forecasting 

setwd("~/Projects/parallel_time_series_forecast")

library(data.table)     
library(forecast)
library(foreach)
library(doParallel) 

registerDoParallel(cores=2)

train <- fread("train_1.csv", nrow=1000)
#dim(train) # 1000 ^limit set above, 551 columns = dates

impute.med <- function(x){
  x <- as.double(unlist(x))
  nas <- is.na(x)
  x[nas] <- ifelse(all(nas), 0, median(x, na.rm=T))
  as.list(x)
}

# Impute NAs with medians or 0
dt <- train[, -1][, impute.med(.SD), by=row.names(train)][, row.names:=train$Page]
setnames(dt, names(train))

# Replace outliers and forecast with auto.arima (in parallel)
fc.wiki <- foreach(i=1:nrow(dt), .combine=rbind, .packages="forecast") %dopar% {
  y <- tsclean(as.ts(unlist(dt[i, -1])))
  forecast(auto.arima(y, max.p=2, max.d=2, max.q=1), h=60)$mean
}

colnames(fc.wiki) <- as.character(seq(as.Date("2017-01-01"), as.Date("2017-03-01"), by="day"))
train <- cbind(train[, 1], fc.wiki)
# using the page ID from train[,1]


# Reshape data into long format
dt <- melt(train,
           id.vars="Page",
           measure.vars=names(train[, -1]),
           variable.name="ds",
           value.name="Visits")

dt[, `:=`(Page=paste(Page, ds, sep="_"), ds=NULL)] 

rm(fc.wiki, train); gc()

# Join with keys to create submission
key <- fread("key_1.csv", key="Page")
setkey(dt, Page)
subm <- dt[key]

head(subm)

write.csv(subm[, .(Id, Visits)], file=gzfile("subm_arima.csv.gz"), row.names=F)