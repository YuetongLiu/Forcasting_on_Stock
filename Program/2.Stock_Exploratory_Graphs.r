

#*********************************
# EXPLORATORY GRAPHS FOR DATA 
#*********************************

library("here")
library("dplyr")
library("magrittr")
# library("prophet")
library("ggplot2")
library("readr")
library("fpp")

# help(package="forecast")
rm(list = ls())

# source scripts: -----------
source("https://raw.githubusercontent.com/YuetongLiu/Prophet_Forecasting/master/Program/1.Clean_Stock_Data.R")  # note that site has been set

# id first and last years: 
first.date <- df1.stock$StartDate[1]  %>% print 
last.date <- df1.stock$StartDate[nrow(df1.stock)] %>% print 

# boxplot by year: 
p1.boxplot <- 
      ggplot(df1.stock, 
             aes(x=year, y=DAX)) + 
      geom_boxplot() + 
      stat_summary(fun.y = mean, 
                   geom = "point") + 
      labs(title = "DAX by year") + 
      theme_classic(); p1.boxplot


# boxplot by month: 
p2.boxplot.month <- 
      ggplot(df1.stock, 
             aes(x=month, y=DAX)) + 
      facet_wrap(~year) + 
      geom_boxplot() + 
      stat_summary(fun.y = mean, 
                   geom = "point") + 
      labs(title = "DAX by month and year") + 
      theme_classic(); p2.boxplot.month


# seasonal boxplot: 
p3.seasonal.box <- 
      ggplot(
            # get avg per month for each year:  
            group_by(df1.stock, year, month) %>% 
                  summarize(month.avg = mean(DAX)), 
            
            # now add aes: 
             aes(x=month, 
                 y=month.avg)) + 
      geom_boxplot() + 
      labs(title = "DAX by month across years", 
           subtitle = paste(first.date, "to", last.date)) + 
      theme_classic(); p3.seasonal.box


# seasonality graph: 
p4.seasonal.line <- 
      ggplot(
            # get avg per month for each year:  
            group_by(df1.stock, year, month) %>% 
                  summarize(month.avg = mean(DAX)),
            
            # now add aes( ): 
             aes(x=month, 
                 y=month.avg, 
                 group=year, 
                 col=year)) + 
      geom_line() + 
      labs(title = "Avg DAX by month across years", 
           subtitle = paste(first.date, "to", last.date)) +
      theme_classic(); p4.seasonal.line






#************************************
# TIME SERIES PLOTS: -------------------
#************************************

# time series by day: 
p5.time.series.by.day <- 
      ggplot(df1.stock,
            aes(x=StartDate, 
                y=DAX)) + 
      
      geom_line() +  # try geom_point for an alternate view 
      
      labs(title = "DAX", 
           subtitle = paste(first.date, "to", last.date)) +
      
      theme_classic(); p5.time.series.by.day




# time series by week 
p6.time.series.by.week <- 
      ggplot(# summarzie data by week: 
            group_by(df1.stock, week) %>% 
                  summarize(week.mean = mean(DAX)),
            # now add aes
            aes(x=week, 
                y=week.mean, 
                group=1)) + 
      
      geom_line() + 
      
      labs(title = "DAX by week", 
           subtitle = paste(first.date, "to", last.date)) +
      
      theme_classic(); p6.time.series.by.week


# time series by month 
p7.time.series.by.month <- 
      ggplot(
            # summarzie data by month: 
            group_by(df1.stock, month.year) %>% 
                  summarize(month.mean = mean(DAX)) %>% 
                  slice(1:n()),  # change endpoint to slice 
            # now add aes  
            aes(x=month.year, 
                y=month.mean, 
                group=1)) +  # note the use of group =1 
      
      geom_line() + 
      
      labs(title = "Avg DAX by month", 
           subtitle = paste(first.date, "to", last.date)) +
      
      theme_classic() ; p7.time.series.by.month


# decompose using stl( ): 
# stl.fit <- stl(as.ts(df1.stock$DAX, 
#                      start=c(2009, 1), 
#                      frequency=365))
# 


#********************************************
# testing normality: 

# density plot: 
p8.density <- ggplot(df1.stock, 
                  aes(x=DAX)) + 
      geom_density() + 
      # geom_vline(xintercept = mean(df1.stock$DAX)) + 
      facet_wrap(~year) + 
      
      # round( ) to -1 for nearest 10s place:
      scale_x_continuous(limits = c(round(min(df1.stock$DAX), -1),
                                    round(max(df1.stock$DAX), -1)),
                         breaks = seq(round(min(df1.stock$DAX), -1),
                                      round(max(df1.stock$DAX), -1),
                                       20)) +

      labs(title = "Density of DAX", 
           subtitle = paste(first.date, "to", last.date)) + 
      
      theme_classic(); p8.density



# qqnorm (ggplot doesn't do this well): 
# first print a plot, then use recordPlot()

# set display: 
setpar <- par(mfrow=c(3, 4))  # 3 rows 4 cols 

# split data by year: 
df1.split.year <- split(df1.stock$DAX, df1.stock$year)

# qqnorm for each year: 
mapply(
      # define fn: 
      function(df, year){
            qqnorm(df, main=year)
      }, 
      
      # args to fn: 
      df1.split.year, 
      names(table(df1.stock$year))
) 

p9.qqnorm <- recordPlot()  # then use recordPlot( )

# reset display: 
par(setpar)
