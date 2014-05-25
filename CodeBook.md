The data download from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
zipfile contain: 1)two directories, test and train. consist of a directory: Inertial Signals, subject, activity(y)file.
2)activity_labels.txt
3)features.txt
4)features_info.txt
5)README.txt

run_analysis.R that does the following: 
1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement. 
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive activity names. 
5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

The tidy data variables contain subject, activity.label, 66 average of the measurements on the mean and standard deviation for each measurement. 


