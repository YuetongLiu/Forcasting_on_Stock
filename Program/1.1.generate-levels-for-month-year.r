
#*************************************
# GENERATE LEVELS FOR month.year var
#*************************************

library("here")
library("dplyr")

# read data: 
df1.stock <- read.csv("https://raw.githubusercontent.com/YuetongLiu/Prophet_Forecasting/master/Data/Stock.csv")

# first find levels of years: 
year.start <- df1.stock$StartDate[1] %>% 
      as.character %>% 
      substr(3, 4) 

year.end <- df1.stock$StartDate[nrow(df1.stock)] %>% 
      as.character %>% 
      substr(3, 4) 

years <- as.integer(year.start): as.integer(year.end)

# now create months: 
months <- rep(1:12, times=length(years))
months.df <- data.frame(month=months, 
                           year = rep(years, each=12))


# combine to find factor levels: 
month.year.levels <- 
      mapply(
            # define function: 
            function(month, year){
            # fn adds zeroes in front if necessary 
            # returns month.year in form "01-18"
                  
                  if(month<10) {
                        month.char <- paste0("0", month)
                  } else {
                        month.char <- as.character(month)
                  }
            
                  if(year<10) {
                        year.char <- paste0("0", year)
                  } else {
                        year.char <- as.character(year)
                  }
                  
            paste(month.char, year.char, sep = "-")
            
            }, 
      
            # args to function: 
            months.df$month, 
            months.df$year)

# examine result: 
# month.year.levels
