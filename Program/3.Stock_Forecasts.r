

#*********************************************
# FORECASTING DAX USING PROPHET
#*********************************************

library("here")
library("dplyr")
library("magrittr")
library("prophet")
library("ggplot2")
library("readr")

# help(package="prophet")
rm(list=ls())

# source scripts: ------------
source("https://raw.githubusercontent.com/YuetongLiu/Prophet_Forecasting/master/Program/1.Clean_Stock_Data.R")  # note that site has been set



# input variables: --------------
# set cutoff date for forecast: 
start.date <- as.Date("2017-11-01")
start.index <- match(start.date, as.Date(df2.stock.prophet$ds))

# set forecast horizon in days: 
horizon = 70 


# fit model using histo data up to cutoff: ---------------
df3.histo <-  df2.stock.prophet[1:start.index, ]
# max(df3.ed.histo$ds)  

# retain actuals to compare: 
df4.actual <- df2.stock.prophet[(start.index + 1):nrow(df2.stock.prophet), ]

# apparently ggplot needs POSIXct: 
df4.actual$ds <- as.POSIXct(df4.ed.actual$ds)


#*******************************************
# fit model: 
#*******************************************
stock.model <- prophet(df3.histo)

# df of forecsat horizon:  ----------
future <- make_future_dataframe(stock.model, 
                                periods = horizon,
                                freq = "day")  

tail(future,horizon)  


# predict with predict( ): -----
fcast <- predict(stock.model, future)

str(fcast); summary(fcast)
tail(fcast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])




#**************************************
# plot the forecast: --------
plot(stock.model, fcast)
# plot(fcast)  # prduces something crazy 

# decompose time series: --------
prophet_plot_components(stock.model, fcast)  
# todo: flat trend since 2016?? Does that make sense? 




#**************************************
# CUSTOM PLOT FOCUSING ON LAST FEW DATA POINTS ------------
#**************************************
latest.fcast <- select(fcast, ds, yhat) %>% 
      rename(fit.histo = yhat) %>%  # fitted values to histo data
      mutate(fcast = fit.histo)  # create copy of fit.histo

# set historical series to NA after cutoff date: 
latest.fcast$fit.histo[(start.index+1):nrow(latest.fcast)] <- NA

# set fcast series to NA before cutoff: 
latest.fcast$fcast[1:start.index] <- NA

# now subset fcast using cutoff, going back 2*horizon: 
latest.fcast <- 
      latest.fcast[(start.index-2*horizon):nrow(latest.fcast), ]
str(latest.fcast)


# finally, plot it: 
p1.fcast <- ggplot() + 
      
      # add historical line: 
      geom_line(data=latest.fcast, 
                aes(x=ds, y=fit.histo)) + 
      
      # now add fcast line: 
      geom_line(data=latest.fcast,
                aes(x=ds, y=fcast), 
                colour="blue") + 
      
      # now add actuals line: 
      geom_line(data=df4.actual, 
                aes(x=ds, y=y), 
                colour="red") + 
      
      # change breaks on x axis: 
      scale_x_datetime(date_breaks = "5 day") + 
      
      theme_classic() + 
      theme(axis.text.x = element_text(angle=90)); p1.fcast


