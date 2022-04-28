#####Problem 1#####
# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:

# FactorialFunction <- function(inputNumber){
#   ???
#     return(Result)
# } 


Result > 1
b<-5
FactorialFunction <- function(n){
for (i in (1:n)){ 
print(Result<- Result*i)
}
return(Result)
}

#####Problem 1#####

#####Problem 2#####
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector) 

f <- c(1,6,8,4,7,9)
SDFunction <- function(inputVector){ 
  avg = sum(inputVector)/length(inputVector)
  sumDiff <- sum((inputVector - avg)^2) 
  result <- sqrt(sum((inputVector - avg)^2) / length(inputVector)) 
  return(result)
}
SDFunction(f) 

sd(f)
  
#####Problem 2#####

#####Problem 3#####
# Read everything from https://r4ds.had.co.nz/transform.html, 
# in particular chapters 5.6/5.7

#Do all the exercises:
# 5.6.7 Exercises 

library(nycflights13)
library(tidyverse) 

#1)Brainstorm at least 5 different ways to assess the typical delay characteristics of a group of flights. Consider the following scenarios:
  
  #A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time. 

eatly_late <- flights %>% 
  group_by(flight) %>% 
  filter(!is.na(arr_delay)) %>%
  summarise(early.15 = sum(arr_delay <= -15, na.rm = TRUE)/n(),
            late.15 = sum(arr_delay <= 15, na.rm = TRUE)/n(), 
           n =n()) %>% 
  ungroup() %>% 
  filter(early.15 == .5, late.15 == .5) 
                    

  

  #A flight is always 10 minutes late. 
  late <- flights %>% 
    group_by(flight) %>% 
    filter(!is.na(arr_delay))%>% 
    summarise( late10 = sum(arr_delay <= 10)/n()) %>% 
    ungroup() %>% 
    filter(late10 == 1)

 

  #A flight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time. 
   or30or50 <- flights %>% 
     group_by(flight) %>% 
     filter(!is.na(arr_delay))%>% 
     summarise(early_30 = sum(arr_delay <= 30, na.rm = TRUE)/n(),
               late_30 = sum(arr_delay <= -30, na.rm = TRUE)/n(), 
                             n = n())%>% 
     ungroup() %>% 
     filter(early_30 == 0.5, late_30 == 0.5)
   
  #99% of the time a flight is on time. 1% of the time it’s 2 hours late. 
   or99or1 <- flights %>% 
     group_by(flight) %>% 
     filter(!is.na(arr_delay))%>% 
     summarise(ontime = sum(arr_delay <= 0, na.rm = TRUE)/n(), 
               late1 = sum(arr_delay <= 120, na.rm = TRUE)/n(), 
                           n = n())%>% 
    ungroup() %>% 
    filter(ontime == 0.99, late1 == 0.01)

  #Which is more important: arrival delay or departure delay?  
   
   
   
   #Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
  worstdelay<-  flights %>% 
     group_by(carrier) %>% 
     summarise(avg_delay = mean(arr_delay, na.rm = TRUE),
               n = n()) %>% 
     arrange(desc(avg_delay))
#2)Come up with another approach that will give you the same output as not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
  not_cancelled <- flights %>%
    filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
    group_by(dest) %>%
    summarise(n = length(dest)); 


#3) Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. Why? Which is the most important column? 
   delay <- filter(flights, !is.na(dep_delay), is.na(arr_delay)) 
  # We need information about the arrival delay. 
  
#4)Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?
  flights %>% 
    group_by(day) %>% 
    summarise(num = n(),
      cancelled = sum(is.na(arr_delay)),
      avg_delayed = mean(arr_delay, na.rm = TRUE),
     cancelled_perc = cancelled / num) 
  # 5.7.1 Exercises 
 # 1) Refer back to the lists of useful mutate and filtering functions. Describe how each operation changes when you combine it with grouping.
    
  # 2)Which plane (tailnum) has the worst on-time record? 
     
     
  flights %>% 
    group_by(tailnum) %>% 
    summarise(n = n(),
              num_not_delayed = sum(arr_delay <= 0, na.rm = TRUE),
              ontime_rate = num_not_delayed/ n, 
              sum_delayed_time_grt0 = sum(ifelse(arr_delay >= 0, arr_delay, 0), na.rm = TRUE)) %>% 
    filter(n > 100, !is.na(tailnum)) %>%
    arrange(ontime_rate)
  
     
    
    # 3)What time of day should you fly if you want to avoid delays as much as possible? 
     avoid_delays <- flights %>% 
       group_by(hour) %>% 
       summarise(avg_delay = mean(arr_delay, na.rm = TRUE)) %>% 
       ungroup() %>%
     arrange(desc(avg_delay)) %>% 
       slice_head()
     
     
                 
    
    # 4)For each destination, compute the total minutes of delay. For each flight,
     #compute the proportion of the total delay for its destination.
    minutes_delay <- flights %>% 
      group_by(dest, flight)%>% 
      summarise(total_min_delay = sum(arr_delay>0, na.rm = TRUE))%>% 
      mutate(total_delay_dest = sum(total_min_delay), 
             proportion = total_min_delay/total_delay_dest)
  # 5)Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. Using lag(), explore how the delay of a flight is related to the delay of the immediately preceding flight.
    correlation <-flights %>% 
      group_by(origin) %>% 
      mutate(delay_lag = lag(dep_delay, 1),
             diff_lag = dep_delay -delay_lag) %>% 
      ungroup() %>% 
      select(dep_delay, delay_lag) %>% 
      na.omit() %>% 
      cor()
 
    
    #6) Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error). Compute the air time of a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?
    
    standardized_flights <- flights %>%
      filter(!is.na(air_time)) %>%
      group_by(dest, origin) %>%
      mutate(
        air_time_mean = mean(air_time),
        air_time_sd = sd(air_time),
        n = n()
      ) %>%
      ungroup() %>%
      mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1)) 
    
    # 7)Find all destinations that are flown by at least two carriers. Use that information to rank the carriers. 
    
    at_least_two <- flights %>% 
      group_by(dest) %>% 
      summarise(carriers = n_distinct(carrier)) %>% 
      filter(carriers > 1) %>% 
      group_by(carriers) %>%
      summarize(destination = n_distinct(dest)) %>%
      arrange(desc(destination))
  
 #8) For each plane, count the number of flights before the first delay of greater than 1 hour. 
   each_plane <-  flights %>%
      filter(!is.na(dep_delay)) %>%
      arrange(tailnum, year, month, day) %>%
      group_by(tailnum) %>%
      summarise(hour_delays = cumsum(dep_delay > 60)) %>% 
      summarise(total_flights = sum(hour_delays < 1))
    
  
#####Problem 3##### 


#####Problem 4#####
#Find the following:
#4.1 For each carrier what is the most common destination?
      
    
    common_dest1 <- flights %>% 
      group_by(carrier) %>% 
      summarise(destination = max(dest))
      
      
#4.2 For each carrier what is the biggest delay? 
    biggest_delay <- flights %>% 
      arrange(desc(arr_delay)) %>%
      group_by(carrier)%>% 
      slice_head() 
    
#4.3 Which are the three plane which have flown the most/least miles? 
    three_planes_least <- flights %>% 
      arrange(distance) %>% 
      slice(1:3)
      
    
    three_planes_most <- flights %>% 
      arrange(desc(distance)) %>% 
      slice(1:3)
    
    
#4.4 What are the first/last flights for each day in February 2013? 
    first <- flights %>% 
      filter(month==2) %>%
      group_by(day) %>% 
      summarise(departure = first(dep_time)) 
    
    last <- flights %>% 
      filter(month==2) %>% 
      group_by(day) %>% 
      summarise(departure = last(dep_time, na.rm = TRUE)) 
    
#4.5 Which company flew the most miles in March 2013? Which flew the least? 
     
     march_flights_max <- flights %>% 
       filter(month == 3) %>% 
       group_by(carrier) %>% 
       summarise(most_miles = max(distance)) 
     
     march_flights_min <- flights %>% 
       filter(month == 3) %>% 
       group_by(carrier) %>% 
       summarise(most_miles = min(distance))
     
       
#4.6 Which month had the most delays over 60 minutes? 
     most_delays <- flights %>% 
       select(month,dep_delay, arr_delay) %>%
       filter(dep_delay > 60, arr_delay > 60) %>%
       group_by(month) %>% 
       summarise(numb_arr_delay = sum(arr_delay, dep_delay)) %>% 
       arrange(desc(numb_arr_delay)) %>% 
       slice_head()
  
#4.7 What is the average time between two consecutive flights? 
     
     avg_time <- flights %>%
       group_by(flight) %>% 
       summarise(diff = mean(arr_delay - lag(arr_delay), na.rm = TRUE)) 
     
       
     

     
     
#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation 
       
#of the flight delays for each month and for each destination.
#####Problem 4#####
#Upload your homeworks on your own github repo.
#Link to the seminar https://unisofiafaculty.sharepoint.com/:v:/s/AccountingFinanceandDigitalapplicationsSeminargroupI/EfR2uYarKcRFiljWMgRb9U8BL6XsygAzJv_fu7mOCQsYzQ?e=CanPG0

  
  
    

  
  
  

  