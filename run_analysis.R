#STEPS

#Download dataset
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "Dataset.zip")

#Unzip the dataset that has been downloaded
listZip <- unzip("Dataset.zip")

#Read in necessary tables from the dataset
features <- read.table("./UCI HAR Dataset/features.txt")
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#Assign colum names to the train data
colnames(train_x) <- features[,2]
colnames(train_y) <- "Activity"
colnames(train_subject) <- "SubjectID"

#Assign column names to the test data
colnames(test_x) <- features[,2]
colnames(test_y) <- "Activity"
colnames(test_subject) <- "SubjectID"

#Assign colum names to the activity table, this goes with both the train and test data sets
colnames(activity) <- c('Activity','ActivityType')

#Combine the train data, combine the test data, then combine the test and train together into 1 dataset
train <- cbind(train_subject,train_y, train_x)
test <- cbind(test_subject, test_y, test_x)
combined <- rbind(train,test)

#Bring in the descriptive activity name
combined2 <- merge(combined, activity, by='Activity')

#Make all column names valid, so that the next step of selecting only the columns I want works
valid_column_names <- make.names(names=names(combined2), unique=TRUE, allow_ = TRUE)
names(combined2) <- valid_column_names

#Need the dplyr library, then select only the columns with mean, std, making sure to bring the SubjectID, and ActivityType, don't need freq or angle
library(dplyr)
combined3 <- select(combined2, contains("SubjectID"), contains("ActivityType"),contains("mean"),contains("std"), -contains("freq"),-contains("angle"))

#Cleaning up the column names so that it's easier to understand
names(combined3) <- gsub("\\.", "_", names(combined3))
names(combined3) <- gsub("BodyBody", "Body", names(combined3))
names(combined3) <- gsub("__", "_", names(combined3))
names(combined3) <- gsub("_$", "", names(combined3))
names(combined3)<-gsub("^t", "time", names(combined3))
names(combined3)<-gsub("^f", "frequency", names(combined3))
names(combined3)<-gsub("Acc", "Accelerometer", names(combined3))
names(combined3)<-gsub("Gyro", "Gyroscope", names(combined3))
names(combined3)<-gsub("Mag", "Magnitude", names(combined3))

#Group the data and get averages by SubjectID and Activity
DataAverages <- combined3 %>%
  group_by(SubjectID, ActivityType) %>%
  summarise_each(funs(mean))

#Write out the grouped/averaged data
write.table(groupData, "DataAverages.txt", row.names = FALSE)Enter file contents here
