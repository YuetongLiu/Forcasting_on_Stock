
#*************************************
# GENERATE LEVELS FOR month.year var
#*************************************

library("here")
library("dplyr")


# read data: 
unloadNamespace("lubridate")
df1.ed <- read_csv(here("data", "2018-01-17_daily-ed-visits-data.csv"))

library("lubridate")

# first find levels of years: 
year.start <- floor_date(df1.ed$StartDate[1], unit="year") %>% 
      as.character %>% 
      substr(3, 4) 

year.end <- floor_date(df1.ed$StartDate[nrow(df1.ed)],
                       unit="year") %>% 
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
