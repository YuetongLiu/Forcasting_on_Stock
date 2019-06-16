#*****************
# forecast monthly Air passengers 
#*****************

library(here)
library(dplyr)
library(magrittr)
library(prophet)
library(ggplot2)

# help(package="prophet")

# data: -----
str(AirPassengers)  # monthly total passengers 

df <- data.frame(ds = seq(as.Date("1949-01-01"),
                          as.Date("1960-12-31"),
                          by="1 month"), 
                 y = as.numeric(AirPassengers))

str(df)
summary(df)

# examine data: -----
p1.historical <- ggplot(df, aes(x=ds, y=y)) + 
      geom_line(); p1.historical

# fit model: --------
m <- prophet(df)

# create future df: ---------
future <- make_future_dataframe(m, 
                                periods = 240,
                                freq = "month")  # 20 years, in months
tail(future)  


# predict with predict( ): -----
fcast <- predict(m, future)

str(fcast)
tail(fcast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# plot with plot( ): -----
plot(m, fcast)

# decompose series : -----
prophet_plot_components(m, fcast)  
# todo: there shouldn't be any day of year seasonality? 
#     or should there be? 

# compare with histo data from 12 year
df2 <- mutate(df, 
              year = rep(1:12, each=12) %>% as.factor, 
              month = as.factor(rep(1:12, 12)))

p2.historical <- ggplot(df2,
                        aes(x=month, 
                            y=y, 
                            group=year, 
                            col=year)) + 
      geom_line(); p2.historical

# both graphs identify peaks in Mar, Jul, Aug
# seasonality actually seems to be getting more pronounced 


