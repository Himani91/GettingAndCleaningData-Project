##-----------1st Question Merges the training and the test sets to create one data set--------------
Features <- read.table("./UCI HAR Dataset/features.txt") ##reads the labels of the features columns (total number of columns - 561)
SubjectsForTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")  
FeaturesDataTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
ActivityLabelTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
SubjectsForTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")  
FeaturesDataTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
ActivityLabelTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
activityNames <- read.table("./UCI HAR Dataset/activity_labels.txt")
## binds all columns for the test data and creates the test data set. Activity + Subject + FeatureDataValues  
testDataSet <- data.frame(cbind(Subject = SubjectsForTest,Activity = ActivityLabelTest,FeaturesDataTest))
## name all the columns of the newly formed test data set. 
colnames(testDataSet)<-  c("Subject","Activity",as.character(Features$V2))
## binds all the columns for the train data set and creates the train data set. Activity + Subject + FeatureDataValues 
trainDataSet <- data.frame(cbind(Subject = SubjectsForTrain,Activity = ActivityLabelTrain,FeaturesDataTrain))
## name all the columns of the newly formed train data set. 
colnames(trainDataSet)<-  c("Subject","Activity",as.character(Features$V2))
##merges both test and train dataset. 
mergeTestAndTrainData <- rbind(testDataSet,trainDataSet)

## --------- 2nd Question- Extracts only the measurements on the mean and standard deviation
## for each measurement ---------- 

##contains the column indexes of the column names containing 'mean' or 'std' (std- standard deviation)  
colIndexofDataMean_SD <- grep(".Mean.*|.*Std." , names(mergeTestAndTrainData), ignore.case=TRUE) 
## subset of the main data with only selected columns
extractDataMean_SD <- data.frame(mergeTestAndTrainData[c(1,2,colIndexofDataMean_SD)]) 


##----------3rd Question - Uses descriptive activity names to name the activities in
## the data set 1:6 - "WALKING" "WALKING_UPSTAIRS" "WALKING_DOWNSTAIRS" "SITTING" "STANDING" "LAYING"

mergeTestAndTrainData$Activity <- factor(mergeTestAndTrainData$Activity)
## renames the activity lables factors with the activity names. 
levels(mergeTestAndTrainData$Activity) <- factor(activityNames[,2]) 


## ----------- 4th Question - Appropriately labels the data set with descriptive variable names-----------

## renames all the short forms in the column names with the complete names
names(mergeTestAndTrainData)<-gsub("^t", "Time_", names(mergeTestAndTrainData))
names(mergeTestAndTrainData)<-gsub("^f", "Frequency_", names(mergeTestAndTrainData))
names(mergeTestAndTrainData)<-gsub("Acc", "accelerometer", names(mergeTestAndTrainData))
names(mergeTestAndTrainData)<-gsub("Gyro", "gyroscope", names(mergeTestAndTrainData))
names(mergeTestAndTrainData)<-gsub("Mag", "magnitude", names(mergeTestAndTrainData))
names(mergeTestAndTrainData)<-gsub("BodyBody", "body_", names(mergeTestAndTrainData))


## ----------5th Question - From the data set in step 4, creates a second, independent tidy data 
##set with the average of each variable for each activity and each subject----------------------

## independent tidy data set with the average of each variable for each activity and each subject.
tidyDataActivity <- aggregate( .~Subject+Activity,mergeTestAndTrainData, FUN=mean)
## writes the tidyDataActivity data set into a file named - TidyDataTable.txt
write.table(tidyDataActivity, file = "TidyDataTable.txt",row.name=FALSE)

