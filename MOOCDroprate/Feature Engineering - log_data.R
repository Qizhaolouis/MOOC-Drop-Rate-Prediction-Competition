## read time
library('tidyverse')
log <- read_csv("activity_log.csv")
head(log,10)
names(log)[2] <- "datetime"
## 
log$date <- format(log$datetime, "%D") %>% 
  as.Date(format = "%m/%d/%y")
log$datetime <- log$datetime %>% as.numeric()

### Start and end
start_end <- log %>%
  select(enrollment_id,date) %>% 
  group_by(enrollment_id) %>% 
  slice(c(1,n())) %>% 
  mutate(row = row_number()) %>%
  spread(key = "row",
         value = date) %>% 
  rename( startdate = `1`,
          enddate = `2`)
  

###events count
### for each enrollmentID we count all events
events <- log %>% 
  group_by(enrollment_id) %>% 
  count(event) %>% 
  spread(key = "event", 
         value = n, 
         fill=0,
         convert = TRUE)

## each enrollment ID more than 1?
### how many days?
### Choose the first and the last record of the enrollment_id, difference in days
secs_in_day <- 60*60*24
proccess_period <- log %>% 
  group_by(enrollment_id) %>% 
  summarise(proccess_period = (datetime[length(enrollment_id)]-datetime[1])/secs_in_day)

### how many hours that are effective?
### choose first and last record in the day and find the difference in hours
secs_in_hour <- 60*60
effective_time <- log %>% 
  group_by(enrollment_id,date) %>% 
  summarise(effective_time = (datetime[length(date)]-datetime[1])/secs_in_hour) %>% 
  ungroup() %>% 
  group_by(enrollment_id) %>% 
  summarise(effective_time = sum(effective_time))

## present date
## find all unique dates within each enrollmentID
present_days <- log %>% 
  group_by(enrollment_id) %>% 
  summarise(present_days = length(unique(date)))

## week days
## check for the present days in an enrollment ID  
week_days <- log %>% 
  distinct(enrollment_id,date) %>% 
  mutate(weekday = weekdays(date)) %>% 
  group_by(enrollment_id) %>% 
  count(weekday) %>% 
  spread(key = "weekday", 
         value = n, 
         fill = 0, 
         convert = TRUE)

### is holiday
# New Year Memorial Indepen. Labor day Thanksgiving Christmas 
library(timeDate)
sample(listHolidays(),10)
HOLI <- c(as.Date(Easter(2010:2017)),
          as.Date(USLaborDay(2010:2017)),
          as.Date(USThanksgivingDay(2010:2017)),
          as.Date(USMemorialDay(2010:2017)),
          as.Date(ChristmasDay(2010:2017)),
          as.Date(USIndependenceDay(2010:2017)),  
          as.Date(USMemorialDay(2010:2017)),
          as.Date(USPresidentsDay(2010:2017))
              )
holidays <-  log %>% 
  distinct(enrollment_id,date) %>% 
  mutate(holidays = as.character(is.element(date,HOLI)))%>% 
  group_by(enrollment_id) %>% 
  count(holidays) %>% 
  spread(key = "holidays", 
         value = n, 
         fill = 0, 
         convert = TRUE) %>% 
  rename(holidays = `TRUE`) %>% 
  select(enrollment_id,holidays)

## remove 
remove(log,HOLI,secs_in_day,secs_in_hour)

#### merge all data from activity
Activity <- left_join(start_end,events,
                      by="enrollment_id") %>% 
  left_join(proccess_period,
            by="enrollment_id") %>% 
  left_join(present_days,
            by="enrollment_id") %>% 
  left_join(effective_time,
            by="enrollment_id") %>% 
  left_join(week_days,
            by="enrollment_id") %>% 
  left_join(holidays,
            by="enrollment_id") %>% 
  write_csv("Activity_Cleaned.csv")