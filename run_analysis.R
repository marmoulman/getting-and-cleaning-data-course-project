# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

library(dplyr)

# 1)  merge training and test sets to create one data set
  dataTest <-  read.table("UCI HAR Dataset\\test\\X_test.txt")
  dataTrain <-  read.table("UCI HAR Dataset\\train\\X_train.txt")
  dataLabels <-  read.table("UCI HAR Dataset\\features.txt")
  subject_Train <- read.table("UCI HAR Dataset\\train\\subject_train.txt") 
  subject_Test <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
  activity_Train <-read.table("UCI HAR Dataset\\train\\y_train.txt")
  activity_Test <-  read.table("UCI HAR Dataset\\test\\y_test.txt")
   
  my_dfTest <-  tbl_df(dataTest)
  my_dfTrain <-  tbl_df(dataTrain)
  my_df = bind_rows(my_dfTrain, my_dfTest)
  subjects <- c(subject_Train[,1], subject_Test[,1])
  activities <- c(activity_Train[,1], activity_Test[,1])
  colnames(my_df) <-  dataLabels[,2]
  
# 2)  extract only the measurements on the mean and standard deviation for each measurement
  
  my_df2 <- my_df[, unique(colnames(my_df))]
  
  #^^^ There were duplicate column names in the original dataset- bandsEnergy duplicated itself for each category. Since these do not affect the mean or sd, they are removed before any further steps are made
  
  my_df2 <- select(my_df2, matches("mean|std")) 
  
# 3)  Use descriptive activiy nbames to name the activities in the data set
  
  
  my_df3 <- mutate(my_df2, 'Subject' = subjects, 'Activity' = activities)
  

# 4)  Appropriately labels the data set with the descriptive variable names
  
    # I did this in step 1. I guess I'll pretty it up a bit more:
  names <- colnames(my_df3)
  names <- gsub('^f', "Frequency Domain-", names)
  names <- gsub('^t', "Time Domain-", names)
  names <- gsub('Acc', "-Acceleration", names)
  names <- gsub('std', "Standard Deviation", names)
  colnames(my_df3) <- names
  
  my_df3 <- mutate(my_df3, Activity = replace(Activity, Activity == 1, "Walking"))
  my_df3 <- mutate(my_df3, Activity = replace(Activity, Activity == 2, "Walking_Upstairs"))
  my_df3 <- mutate(my_df3, Activity = replace(Activity, Activity == 3, "Walking_Downstairs"))
  my_df3 <- mutate(my_df3, Activity = replace(Activity, Activity == 4, "Sitting"))
  my_df3 <- mutate(my_df3, Activity = replace(Activity, Activity == 5, "Standing"))
  my_df3 <- mutate(my_df3, Activity = replace(Activity, Activity == 6, "Laying"))

# 5)  From the data set in step 4, create a second independent tidy data set with the average of each variable for each activity and each subject
 # my_df4 <- mutate(Activity = replace(Activity, Activity == 1, "Running"))
#  my_df_Activity <- select(my_df3, -Subject)
  my_df_Activity <- group_by(my_df3, Subject, Activity)
  my_df_Activity <- summarise_all(my_df_Activity, funs(mean))
  
  write.table(my_df_Activity, "dataset.txt", row.name = FALSE)
  
#  my_df_Subject <- select(my_df3, -Activity)
#  my_df_Subject <-  group_by(my_df_Subject, Subject)
#  my_df_Subject <- summarise_all(my_df_Subject, funs(mean))
