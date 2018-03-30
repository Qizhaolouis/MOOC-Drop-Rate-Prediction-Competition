Also read the report in http://rpubs.com/louischoki/Feature-Engineering-with-MOOC-Dataset
------------
# MOOC-Drop-Rate

This is a documentation of the [**MOOC Dropout Prediction**](https://www.kaggle.com/c/mooc-dropout-prediction-17) project, which is a Kaggle competition.

# Data Set

First of all, let's describe the 3 tables of the dataset. 

* In the 'enrollment_list.csv' table, there are 3 columns. First column is the Enrollment ID. Second column is the User ID and the third one is the Course ID. Each User ID can choose multiple courses and thus has multiple Enrollment ID's.

* In the 'activity_log.csv' table, there are also 3 columns: The first column is the Enrollment ID; second one is the date and time of the activity and the third one is the activity. 

* The 'train_label.csv' contains the Enrollment ID and whether a Enrollment ID has dropped a course.

# Feature Engineering

## Activity Table
Obviously, this is not considered tidy data, we have to clean the data and gather the features that we want. We need the package tidyverse which enables us to read, wrangle and write data much faster.
```{r}
library('tidyverse')
log <- read_csv("activity_log.csv")
head(log,5)
names(log)[2] <- "datetime"
```
The tidyverse package automatically read the date as the 'datetime' type. We will get the "date" from the date and time first.
```{r}
log$date <- format(log$datetime, "%D") %>% 
  as.Date(format = "%m/%d/%y")
log$datetime <- log$datetime %>% as.numeric()
```
First of all, we will make each event as a column and record the number of each event within a certain ID.
```{r}
events <- log %>% 
  group_by(enrollment_id) %>% 
  count(event) %>% 
  spread(key = "event", 
         value = n, 
         fill=0,
         convert = TRUE)
head(events, 5)
```
Then we record the start and end date of each ID.
```{r}
start_end <- log %>%
  select(enrollment_id,date) %>% 
  group_by(enrollment_id) %>% 
  slice(c(1,n())) %>% 
  mutate(row = row_number()) %>%
  spread(key = "row",
         value = date) %>% 
  rename( startdate = `1`,
          enddate = `2`)
```

Next we will gather the difference between the time of the first and last record within a enrollment_id. We then convert the seconds to days.
```{r}
secs_in_day <- 60*60*24
proccess_period <- log %>% 
  group_by(enrollment_id) %>% 
  summarise(proccess_period = 
              (datetime[length(enrollment_id)]-
                 datetime[1])/secs_in_day)
head(proccess_period,5)
```

We can do the same to get the effective time of an enrollment ID. 
```{r}
secs_in_hour <- 60*60
effective_time <- log %>% 
  group_by(enrollment_id,date) %>% 
  summarise(effective_time = (datetime[length(date)]-datetime[1])/secs_in_hour) %>% 
  ungroup() %>% 
  group_by(enrollment_id) %>% 
  summarise(effective_time = sum(effective_time))

```
We can also count the days that a student has at least one activity.
```{r}
present_days <- log %>% 
  group_by(enrollment_id) %>% 
  summarise(present_days = length(unique(date)))
```

Besides, we can also count the number of each weekdays.
```{r}
week_days <- log %>% 
  distinct(enrollment_id,date) %>% 
  mutate(weekday = weekdays(date)) %>% 
  group_by(enrollment_id) %>% 
  count(weekday) %>% 
  spread(key = "weekday", 
         value = n, 
         fill = 0, 
         convert = TRUE)

```

Next, I want to gather the number of present days that are also a holiday.

```{r}
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
```

Now we have gathered all the data that we want from the activity table. Let's join all the data and write it to a csv file called "Activity_Cleaned.csv".

```{r}
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
```

## Enrollment List Table
First, let's read the table in R and get the number of students in each course and the number of enrollment of each user. 
```{r}
enrollment <- read_csv("enrollment_list.csv")
course <- count(enrollment, course_id) %>% rename(course_enroll = n)
user <- count(enrollment, user_id) %>% rename(user_enroll = n)
```

Now, let's read in the train label table and calculate the "drop rate" of each course
```{r}
train <- read_csv('train_label.csv') %>% 
  left_join(enrollment, by = "enrollment_id")
course_drop_rate <- train %>% 
  group_by(course_id) %>% 
  summarise(course_drop_rate = sum(dropout_prob)/length(enrollment_id))
```

Finally we merge all the features and get the final dataset from Enrollment List table.
```{r}
CourseInfo <- left_join(enrollment, course, by = 'course_id') %>% 
  left_join(user, by = "user_id") %>% 
  left_join(course_drop_rate, by = 'course_id') %>% 
  select(enrollment_id, course_enroll, 
         user_enroll, course_drop_rate) %>% 
  write_csv("CourseInfo_Cleaned.csv")
```

## Merge Data

We merge the data and split it into train and test set.

```{r}
Activity <- read_csv('Activity_Cleaned.csv')
CourseInfo <- read_csv("CourseInfo_Cleaned.csv")
train_label <- read_csv("train_label.csv")


Activity$start_year <- Activity$startdate %>% 
                       as.character() %>% 
                       substring(1,4) %>% as.numeric()
Activity$start_month <- Activity$startdate %>% 
                        as.character() %>% 
                        substring(6,7) %>% as.numeric()
Activity$end_year <- Activity$enddate %>% 
                      as.character() %>% 
                      substring(1,4) %>% as.numeric()
Activity$end_month <- Activity$enddate %>% 
                      as.character() %>% 
                      substring(6,7) %>% as.numeric()
Activity <- Activity[,-c(2,3)]
```
Before we merge, I created 4 new features because we are going to use a machine learning algorithms which only takes numerical features.
``` {r}
##join all
MOOC <- left_join(Activity, CourseInfo, by = "enrollment_id") %>% 
write_csv("MOOC.csv")
## get train
MOOC_train <- right_join(MOOC, train_label, 
                         by = 'enrollment_id') %>% 
              write_csv("MOOC_train.csv")
## get test
MOOC_test <- MOOC %>% 
             filter(!enrollment_id %in% train_label$enrollment_id) %>% 
             write_csv("MOOC_test.csv")
```

# Exploratory Analysis

First of all let's look at the correlation between columns.
```{r fig.align='center'}
##cor plot
library(ggcorrplot)
COR <- MOOC_train %>% 
  select(proccess_period,present_days,effective_time,start_month,dropout_prob)
COR$events <- apply(MOOC_train[,2:8],1,sum)
corr <- round(cor(COR,method = "pearson"), 2)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_col = "black",
           outline.color = "white",
           colors = c("skyblue",  "white","skyblue"),
           lab_size = 3, 
           method="square", 
           show.legend = TRUE, legend.title = "correlation", 
           title="Correlarion Plot", 
           ggtheme=theme_bw)

```
