

#*************************
# FN TO GET WEEK NUMBER FROM DATES IN WEEK FORMAT 
#*************************

week.fn <- function(vec){ 
      # takes vect of weeks in floor_date( *, unit = "week") format 
      # returns week nums as integers starting from 0 
      
      first <- vec[1]
      sapply(vec, function(x){
            diff <- x-first
            
            if (diff > 0){
                  return(diff/7)
            } else {
                  return(0)
            }})

}

# test function: -----  
# library("lubridate")
# 
# weeks <- floor_date(seq(as.Date("2017-01-01"), 
#                         as.Date("2017-03-20"), 
#                         by="1 days"), 
#                     unit="weeks") 
# 
# week.fn(weeks)
# week.fn(weeks) %>% table
