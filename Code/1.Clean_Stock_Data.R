#*******************************************
# CLEAN Stock DATA 
#*******************************************

library(here)
library(dplyr)
library(magrittr)
library(prophet)
library(ggplot2)
library(readr)
# help(package="prophet")
rm(list=ls())

# source scripts: ------------
# source(here("src", "ggtheme.R"))
source("https://raw.githubusercontent.com/YuetongLiu/Prophet_Forecasting/master/Program/1.2weeknum_function.r")
source("https://raw.githubusercontent.com/YuetongLiu/Prophet_Forecasting/master/Program/1.1.generate-levels-for-month-year.r")


# read ed data in: --------------------------------
df1.stock <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Prophet_Forecasting/master/Data/Stock.csv")

df1.stock %<>% 
  mutate(year = floor_date(as.Date(StartDate), unit="year") %>% 
           as.character %>% 
           substr(1,4) %>% 
           as.factor, 
         week = floor_date(as.Date(StartDate),unit="week")%>% 
           week.fn %>% 
           as.factor, 
         month = StartDate %>%
           as.character %>%
           substr(6,7) %>% 
           as.factor, 
         month.year = paste(month, 
                            substr(year, 3, 4),
                            sep="-") %>% 
           factor(levels = month.year.levels)) 

# str(df1.stock)
# summary(df1.stock); head(df1.stock)
# as.data.frame(df1.stock)[1:100, ]



# prepare data for prohet:------------
df2.stock.prophet <- select(df1.stock, 
                         StartDate, 
                         DAX) %>% 
  rename(ds=StartDate, 
         y=DAX)

str(df2.stock.prophet)

