Download the zipped file in your working directory. 
Source : https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

All the files are inside the directory UCI HAR Dataset. 

## VARIABLES USED :

*  *Features*  : Reads filename : features.txt . Contains the labels of the features columns (total number of columns - 561) 

*  *SubjectsForTest* : Reads filename : "test/subject_test.txt" . Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30 and contains 9 unique values ( 30% of total people)

*  *FeaturesDataTest* : Reads filename : "test/X_test.txt" . Test set containing values of all the variables of "Features" dataset. 

*  *ActivityLabelTest* : Reads filename : "test/y_test.txt" . Factor column : 1-6 for each row. Numbers represent the following activities.  
 1. WALKING
 2. WALKING_UPSTAIRS
 3. WALKING_DOWNSTAIRS
 4. SITTING
 5. STANDING
 6. LAYING 

*  *SubjectsForTrain* :  Reads filename : "train/subject_train.txt" .Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30 and contains 21 unique values ( 70% of total people)

*  *FeaturesDataTrain* : Reads filename : "train/X_train.txt" . Train set containing values of all the variables of "Features" dataset. 

*  *ActivityLabelTrain*  : Reads filename : "train/y_train.txt" . Factor column : 1-6 for each row. Numbers represent the following activities.  
 1. WALKING
 2. WALKING_UPSTAIRS
 3. WALKING_DOWNSTAIRS
 4. SITTING
 5. STANDING
 6. LAYING 


*  *testDataSet*  : Data table for Test. 

*  *trainDataSet* : Data table for Train. 

*  *mergeTestAndTrainData* : Combined dataset for both Test and Train. 

*  *colIndexofDataMean_SD* : Contains the indexes of columns with means and standard deviations. 

*  *extractDataMean_SD* : Subset of the "mergeTestAndTrainData" ; contains only columns mentioned in "colIndexofDataMean_SD" 

*  *activityNames* : Reads filename  "activity_labels.txt". Links the class labels with their activity name.

*  *tidyDataActivity* : Tidy dataset which contains the average of each variable for each activity and each subject. 

*  *TidyDataTable.txt* : Contains the tidyDataActivity dataset. 
