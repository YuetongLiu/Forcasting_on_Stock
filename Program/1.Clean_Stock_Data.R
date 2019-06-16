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
source("https://github.com/YuetongLiu/Prophet_Forecasting/blob/master/Program/1.1.generate-levels-for-month-year.r")


# read ed data in: --------------------------------
df1.stock <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Prophet_Forecasting/master/Data/Stock.csv")

# here and lubridate packages don't seem to play well, so we do this later

df1.ed %<>% 
  mutate(year = floor_date(StartDate,
                           unit = "year") %>% 
           as.character %>% 
           substr(1,4) %>% 
           as.factor, 
         week = floor_date(StartDate, 
                           unit = "week") %>% 
           week.fn %>% 
           as.factor, 
         month = floor_date(StartDate, 
                            unit = "month") %>%
           as.character %>%
           substr(6,7) %>% 
           as.factor, 
         month.year = paste(month, 
                            substr(year, 3, 4),
                            sep="-") %>% 
           factor(levels = month.year.levels)) 

# str(df1.ed)
# summary(df1.ed); head(df1.ed)
# as.data.frame(df1.ed)[1:100, ]



# prepare data for prohet:------------
df2.ed.prophet <- select(df1.ed, 
                         StartDate, 
                         numvisits) %>% 
  rename(ds=StartDate, 
         y=numvisits)

str(df2.ed.prophet)



# write outputs: --------------
unloadNamespace("lubridate")
write_csv(df1.ed, 
          paste0(here("output from src"), 
                 "/df1-ed-reformatted.csv")) 

# in xl, change month.year col to text then paste: 
writeClipboard(df1.ed$month.year %>% as.character)
