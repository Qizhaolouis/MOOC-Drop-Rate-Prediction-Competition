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
  
  
  