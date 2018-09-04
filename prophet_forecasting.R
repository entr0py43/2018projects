#Tutorial ref
#https://towardsdatascience.com/using-open-source-prophet-package-to-make-future-predictions-in-r-ece585b73687

# install.packages('prophet')
pacman::p_load(tidyverse, prophet)

#load dataset, lebron james pageviews
setwd("~/Projects/parallel_time_series_forecast/prophet")
stats <- read.csv('pageviews.csv', header=T, sep = ',') #renamed for simplicity
colnames(stats) <- c('ds', 'y') # required by package, not arbitrary
head(stats) # starts 2015-07-01
tail(stats) # finish 2018-09-02

#note the tutorial is older, and had lebron in every playoff for the previous 6 years
# since we can't reach back that far for the data, it takes a little away from this


stats$y <- log10(stats$y)
summary(stats)

plot(y ~ ds, stats, type ='l') #its ignoring line plot request?

m <- prophet(stats)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

plot(m, forecast)
dyplot.prophet(m, forecast) #very nice


tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
tail(forecast)

# trend lines, weekly + yearly
prophet_plot_components(m, forecast)


playoff_brackets <- data_frame(
  holiday = 'playoffs',
  ds = as.Date(c('2019-04-13' '2018-04-14','2017-04-15','2016-04–16')),
  lower_window = 0,
  upper_window = 45
)

# 
# playoff_finals <- data_frame(
#   holiday = ‘playoff_finals’,
#   ds = as.Date(c('2019-06-01' '2018-05-31','2017-05-14','2016–06–02')),
#   lower_window = 0,
#   upper_window = 20
# )
#lebron wasn't in playoffs for 2017,2018

#what you would do w/two dataframes
# holidays <- bind_rows(playoff_brackets, playoff_finals)
m <- prophet(stats, holidays = playoff_brackets)
forecast <- predict(m, future)
plot(m, forecast)

dyplot.prophet(m, forecast)
#weird... the prediction tries to mimic the spikes in the data much more
#precisely around april-june 2017, compared to other years, not sure why


#removing outliers
outliers <- (as.Date(stats$ds) > as.Date('2014-07-09') &
               as.Date(stats$ds) < as.Date('2014-07-12')))
stats$y[outliers] <- NA
#this will do nothing since we don't have any in that range
# PROPHET WILL IGNORE NA'S pretty useful to know... won't break


# anomaly detection
combined_data <- cbind(head(forecast, nrow(stats)), stats[order(stats$ds),])
combined_data$diff_values <- (combined_data$y - combined_data$yhat)
summary(combined_data$diff_values)

combined_data$diff_values_normalized <-
  (combined_data$y - combined_data$yhat) / combined_data$y

plot(diff_values_normalized ~ ds, combined_data, type = 'l')

#what percent of data points are anomalies
# theshold here, 10%
nrow(combined_data[abs(combined_data$diff_values_normalized) > 0.1
                   & !is.na(combined_data$y),]) / nrow(combined_data)
#.012
# not bad...