##The course project of getting and cleaning the data
###download the data from website
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","Dataset.zip")
#unzip the zip file
unzip(zipfile="Dataset.zip")

#1.Merges the training and the test sets to create one data set  
train<-read.table("UCI HAR Dataset/train/X_train.txt") #load the train data
test<-read.table("UCI HAR Dataset/test/X_test.txt") #load the test data
feature<-read.table("UCI HAR Dataset/features.txt") #load the feature of train and test data
colnames(train)<-feature$V2 # add the colnames to train data
colnames(test)<-feature$V2 # add the colnames to test data
data.set<-rbind(train,test) # merge the train and test data by rbind()

###2.Extracts only the measurements on the mean and standard deviation for each measurement. 
sub<-c(grep("mean()|std()",colnames(data.set))) # the index of column which measure on the mean and standard deviation 
sub.Freq<-c(grep("meanFreq", colnames(data.set))) # the index of column which measure on meanFreq() 
sub.mean.std<-setdiff(sub,sub.Freq) # delete the index which is meanFreq()
data.subset<-data.set[,sub.mean.std] #subset the measurements on the mean and standart deviation

#add subject to the data.subset
subject.train<-read.table("UCI HAR Dataset/train/subject_train.txt") # load subject of train data
subject.test<-read.table("UCI HAR Dataset/test/subject_test.txt") # load subject of test data
subject<-rbind(subject.train,subject.test) #combine train and test subject
data.subset$subject<-subject$V1 # add the subject to the data.subset
#add groups(contain train group and test group)
data.subset$groups<-c(rep("train",times=nrow(train)),rep("test",times=nrow(test)))

###3.Uses descriptive activity names to name the activities in the data set
y.train<-read.table("UCI HAR Dataset/train/y_train.txt") # load the activity numbers (1-6) of train
y.test<-read.table("UCI HAR Dataset/test/y_test.txt") # load the activity numbers (1-6) of test
activity.ID<-rbind(y.train,y.test) # combine the activity numbers (1-6) of train and test data.
data.subset$activity.ID<-activity.ID$V1 # add activity names by activity numbers(1-6)

###4.Appropriately labels the data set with descriptive activity names. 
activity.label.num<-read.table("UCI HAR Dataset/activity_labels.txt") 
label<-as.vector(activity.label.num$V2)
#convert the activity number into activity label,and add the new column which contain the activity names 
activity.label<-c() 
for(i in activity.ID$V1){
  activity.label<-c(activity.label,label[i])
}
data.subset$activity.label<-activity.label 

###5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
library(plyr)
tidy.data<-ddply(data.subset,.(subject,activity.label),function(x)colMeans(x[,colnames(data.subset)[1:length(sub.mean.std)]]))
write.table(tidy.data,file="tidy.data.txt",sep="\t",quote=F,row.names=F) # export the tidy data
