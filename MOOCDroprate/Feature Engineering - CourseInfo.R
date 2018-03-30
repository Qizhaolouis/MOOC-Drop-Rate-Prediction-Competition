## read in enrollment list
enrollment <- read_csv("enrollment_list.csv")

## 
course <- count(enrollment, course_id) %>% rename(course_enroll = n)
user <- count(enrollment, user_id) %>% rename(user_enroll = n)

## read training data
train <- read_csv('train_label.csv') %>% 
  left_join(enrollment, by = "enrollment_id")

## find the drop rate of each course
course_drop_rate <- train %>% 
  group_by(course_id) %>% 
  summarise(course_drop_rate = sum(dropout_prob)/length(enrollment_id))

CourseInfo <- left_join(enrollment, course, by = 'course_id') %>% 
  left_join(user, by = "user_id") %>% 
  left_join(course_drop_rate, by = 'course_id') %>% 
  select(enrollment_id, course_enroll, 
         user_enroll, course_drop_rate) %>% 
  write_csv("CourseInfo_Cleaned.csv")