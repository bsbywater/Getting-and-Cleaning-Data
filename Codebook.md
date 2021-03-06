# Analysis Steps

This Codebook will describe the steps taken in the run_analysis.R script.  

### STEPS
- The data sets were downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip into a folder on my c drive.
Once the data sets were downloaded, the data sets needed to be unzipped.  Once that was complete several tables were read into R: features.txt, activity_labels.txt, X_train.txt, y_train.txt, subject_train.txt, X_test.txt, y_test.txt, and subject_test.txt. 
- The data columns are then given names based on the features.txt file.
- The three train files were combined, the three test files were combined, then the train and test files were combined to make one data set. 
- The descriptive name of the activity was brought into the main data set.
- Columns that hold mean or standard deviation measurements are selected from the dataset, while the other measurement columns are excluded from the rest of the analysis. I excluded columns that contained "freq" or "angle". Before it was possible to select the appropriate columns, code was ran to make all column names valid since some of the characters in the column names were giving me an error.
- Column names were then cleaned up, duplicate phrase BodyBody was replaced with Body, t was replaced with time, f was replaced with frequency, Acc was replaced with Accelerometer, Gyro was replaced with Gyroscope, and Mag was replaced with Magnitude.
- The data was then grouped by subject and activity, and the average is calculated for every measurement column.
- Last, the grouped/averaged dataset is written to a file, DataAverages.txt.

Comments can also be found in the run_analysis.R with the purpose of the code.

### Output File Columns

The columns included in the output file are listed below:
- SubjectID: The id of the experiment participant.
- ActivityType: The name of the activity that the measurements correspond to, e.g. Walking, Standing.

All of the following fields represent the average for the given subject and activity.
- timeBodyAccelerometer_mean__X
- timeBodyAccelerometer_mean__Y
- timeBodyAccelerometer_mean__Z
- timeGravityAccelerometer_mean__X
- timeGravityAccelerometer_mean__Y
- timeGravityAccelerometer_mean__Z
- timeBodyAccelerometerJerk_mean__X
- timeBodyAccelerometerJerk_mean__Y
- timeBodyAccelerometerJerk_mean__Z
- timeBodyGyroscope_mean__X
- timeBodyGyroscope_mean__Y
- timeBodyGyroscope_mean__Z
- timeBodyGyroscopeJerk_mean__X
- timeBodyGyroscopeJerk_mean__Y
- timeBodyGyroscopeJerk_mean__Z
- timeBodyAccelerometerMagnitude_mean
- timeGravityAccelerometerMagnitude_mean
- timeBodyAccelerometerJerkMagnitude_mean
- timeBodyGyroscopeMagnitude_mean
- timeBodyGyroscopeJerkMagnitude_mean
- frequencyBodyAccelerometer_mean__X
- frequencyBodyAccelerometer_mean__Y
- frequencyBodyAccelerometer_mean__Z
- frequencyBodyAccelerometerJerk_mean__X
- frequencyBodyAccelerometerJerk_mean__Y
- frequencyBodyAccelerometerJerk_mean__Z
- frequencyBodyGyroscope_mean__X
- frequencyBodyGyroscope_mean__Y
- frequencyBodyGyroscope_mean__Z
- frequencyBodyAccelerometerMagnitude_mean
- frequencyBodyAccelerometerJerkMagnitude_mean
- frequencyBodyGyroscopeMagnitude_mean
- frequencyBodyGyroscopeJerkMagnitude_mean
- timeBodyAccelerometer_std__X
- timeBodyAccelerometer_std__Y
- timeBodyAccelerometer_std__Z
- timeGravityAccelerometer_std__X
- timeGravityAccelerometer_std__Y
- timeGravityAccelerometer_std__Z
- timeBodyAccelerometerJerk_std__X
- timeBodyAccelerometerJerk_std__Y
- timeBodyAccelerometerJerk_std__Z
- timeBodyGyroscope_std__X
- timeBodyGyroscope_std__Y
- timeBodyGyroscope_std__Z
- timeBodyGyroscopeJerk_std__X
- timeBodyGyroscopeJerk_std__Y
- timeBodyGyroscopeJerk_std__Z
- timeBodyAccelerometerMagnitude_std
- timeGravityAccelerometerMagnitude_std
- timeBodyAccelerometerJerkMagnitude_std
- timeBodyGyroscopeMagnitude_std
- timeBodyGyroscopeJerkMagnitude_std
- frequencyBodyAccelerometer_std__X
- frequencyBodyAccelerometer_std__Y
- frequencyBodyAccelerometer_std__Z
- frequencyBodyAccelerometerJerk_std__X
- frequencyBodyAccelerometerJerk_std__Y
- frequencyBodyAccelerometerJerk_std__Z
- frequencyBodyGyroscope_std__X
- frequencyBodyGyroscope_std__Y
- frequencyBodyGyroscope_std__Z
- frequencyBodyAccelerometerMagnitude_std
- frequencyBodyAccelerometerJerkMagnitude_std
- frequencyBodyGyroscopeMagnitude_std
- frequencyBodyGyroscopeJerkMagnitude_std
