---
title: "README"
output: html_document
---

#Introduction

This repository is hosting the R code for the course project of the DataScience track's "Getting and Cleaning Data" course.It contains three files, this README,the R script to create the new data set (run_analyis.R) and a code book (CodeBook.md) describing the variables in the newly created data set.
The script works on the Human Activity Recognition database created by [SMARTLAB](www.smartlab.ws) and creates a new 
data set by taking the average of each variable for each activity and each subject of the underlying data set.

##Preleminaries

For the script to work it needs the data set from the [UCI site](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones), which can be found here:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
Download and unzip the raw data onto your local computer. It should contain:

- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. 


###Execution
The script awaits as parameter the local path to the unzipped raw data set, i.e 
```
run_analysis("~/R/UCI HAR Dataset/")

```

###Internals
```
##run_analysis 
# Input: Path to directory which contains the test and training data
# Output: Data set which contains the average of each variable for each activity and each subject

run_analysis <- function(directory){
    
    #path to test and training data set
    testdir <- paste0(directory,"test/")
    traindir <- paste0(directory,"train/")
    
    #read in test set data
    x_test <- read.table(paste(testdir,list.files(testdir,pattern="^X.+txt"),sep=""))
    y_test <- read.table(paste(testdir,list.files(testdir,pattern="^y.+txt"),sep=""))
    subject_test <- read.table(paste(testdir,list.files(testdir,pattern="^subject.+txt"),sep=""))
    
    #read in training set data
    x_train <- read.table(paste(traindir,list.files(traindir,pattern="^X.+txt"),sep=""))
    y_train <- read.table(paste(traindir,list.files(traindir,pattern="^y.+txt"),sep=""))
    subject_train <- read.table(paste(traindir,list.files(traindir,pattern="^subject.+txt"),sep=""))
    
    #read in activity data
    activity <- read.table(paste(directory,list.files(directory,pattern="^activity.+txt"),sep=""))
    
    #read in features
    features <- read.table(paste(directory,list.files(directory,pattern="^features.txt"),sep=""))
    
    ##Add column names to data
    #create vector for column names and remove "()"  
    cnames <- gsub("\\()","",features$V2)
    
    #set column names for test and training data
    colnames(x_test) <- cnames
    colnames(x_train) <- cnames
    
    ##Add variables "subject" and "activities" to test and training data
    #create subject variable for test data
    x_test$subject <-subject_test$V1
    #create subject variable for training data
    x_train$subject <- subject_train$V1 
    
    #create new activity variable for test and training data 
    #first create vector from activity data i.e. ("WALKING",...)
    lookup <- as.vector(activity$V2)
    
    #set names for lookup vector to ("1","2",...,"6")
    names(lookup)<-as.vector(activity$V1)
    
    #now call lookup on y_test and y_train which hold the corresponding activities for x_test and x_train
    x_test$activity <-as.vector(lookup[y_test$V1])
    x_train$activity <- as.vector(lookup[y_train$V1])
    
    #Merge test and training data set
    X<-rbind(x_test,x_train)
    
    ##Filter for mean and std values
    #filter colum names for "mean" and "std" 
    cnames_meanstd<- c(cnames[sort(c(grep("std",cnames,ignore.case=TRUE),grep("mean",cnames,ignore.case=TRUE)))])
    
    ##remove all columns except for mean,std,subject and activity   
    X <- X[c(cnames_meanstd,"subject","activity")]
    
    ##reshape data so that we get the average of each variable for each activity and each subject 
    ## load library Reshape2
    library(reshape2)
    #Melt data
    X <- melt(X,id=c("subject","activity"))
    #Cast
    X <-dcast(X,subject+activity~variable,mean)
    
    #Return new data set
    X
    
    #Done
}
```


####Output
Returns a list with first two columns as ids (subject,activity) and remaining colums as variables 
