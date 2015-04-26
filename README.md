
# Getting and Cleaning Data

 **Goal of the project** : Collecting the raw data from the given site and creating a tidy and readable data set which can be used to perform computations or to analyse the data. 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Following files will be used to attain the goal :

*(All files are inside the directory UCI HAR Dataset)*

* *features_info.txt* : Shows information about the variables used on the feature vector.

* *features.txt*: List of all features.

* *activity_labels.txt*: Links the class labels with their activity name.

* *train/X_train.txt*: Training set.

* *train/y_train.txt*: Training labels.

* *test/X_test.txt*: Test set.

* *test/y_test.txt*: Test labels.

* *test/subject_test.txt* : Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. Contains 9 unique values. 

* *train/subject_train.txt* : Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. Contains 21 unique values. 


**Output** : The tidy data set created is contained in the file named : "TidyDataTable.txt" 



## To do : 

You should create one R script called **run_analysis.R** that does the following. 
  1.	Merges the training and the test sets to create one data set.
  2.	Extracts only the measurements on the mean and standard deviation for each measurement.
  3.	Uses descriptive activity names to name the activities in the data set
  4.	Appropriately labels the data set with descriptive variable names.
  5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Steps taken : 

a. **For task 1 -** 
  * Step 1 - Read all the data files and name each data set with appropriate names. 
  * Step 2 - Merge the activity, subject and features columns into one data set for each "test" and "train" data. 
  * Step 3 - Merge both test and train datasets.
  
b. **For task 2 -**
  * Step 4 - With the help of the function - grep() - retrieve all the column indexes of the column names containing "mean" or "std". 
  * Step 5 - Subset the main dataset (created in step 3) with the selected columns indexes (selected in step 4) 
  
c. **For task 3 -** 
  * Step 6 - Rename the activity labels with the activity names in the main data set. __Method applied-__ since it is the factor column, we can overwrite each level of the factor with their respective activity name.
*Syntax:* levels(mainDataSet$ActivityColumn) <- factor(activityNames) where  activityNames is a vector containing all 6 activity names. 
  
d. **For task 4 -** 
  * Step 7 - Rename all the short forms in the feature column names with the complete names. __Function used- gsub()__
  
e. **For task 5 -** 
  * Step 8 - Create an independent tidy data set with the average of each variable for each activity and each subject. __Function used - aggregate()__ - takes Subject and Activity columns as unique values and computes the mean of the rest of the columns. Total rows : 180 ( total permutations : 30 * 6 , where unique subjects = 30 , unique activities = 6 )  
  
**To read the file :** 
  * Store the file in your working directory. 
  * Use read.table() to read the contents of the file. 
 
 
#### For converting the rmd files into md files : 
 ```
library(knitr)
knit2html("CodeBook.Rmd")

```
