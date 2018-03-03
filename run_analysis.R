# Peer-graded Assignment: Getting and Cleaning Data Course Project
# By: Cecilia Cruz-Ram, MD

# R script for getting and cleaning data project

# A. Install Packages
install.packages(c("downloader", "plyr", "knitr"))

# B. Load Packages
library(downloader)
library(plyr);
library(knitr)

# C. Get the data
path <- setwd("/Users/sexybaboy/Documents/Files/Zetch/Online Courses/Data Science Specialization Feb18/GitHub/ProgrammingAssignment3")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# D. List all the files of UCI HAR Dataset folder
path <- file.path("/Users/sexybaboy/Documents/Files/Zetch/Online Courses/Data Science Specialization Feb18/GitHub/ProgrammingAssignment3", "UCI HAR Dataset")
files <- list.files(path, recursive = TRUE)

# E. Load activity, subject and feature files
## Read data from the files into the variables

## E.1. Read the Activity files
ActivityTest  <- read.table(file.path(path, "test", "Y_test.txt"),header = FALSE)
ActivityTrain <- read.table(file.path(path, "train", "Y_train.txt"),header = FALSE)

## E.2. Read the Subject files
SubjectTest  <- read.table(file.path(path, "test", "subject_test.txt"),header = FALSE)
SubjectTrain <- read.table(file.path(path, "train", "subject_train.txt"),header = FALSE)

## E.3. Read Features files
FeaturesTest  <- read.table(file.path(path, "test", "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(path, "train", "X_train.txt"),header = FALSE)

## E.4. Test: Check properties

str(ActivityTest)
str(ActivityTrain)
str(SubjectTrain)
str(SubjectTest)
str(FeaturesTest)
str(FeaturesTrain)

# 1. Merges the training and the test sets to create one data set.

## 1.1. Concatenate the data tables by rows
dataSubject <- rbind(SubjectTrain, SubjectTest)
dataActivity <- rbind(ActivityTrain, ActivityTest)
dataFeatures <- rbind(FeaturesTrain, FeaturesTest)

## 1.2. Set names to variables
names(dataSubject) <- c("subject")
names(dataActivity) <- c("activity")
dataFeaturesNames <- read.table(file.path(path, "features.txt"), head = FALSE)
names(dataFeatures) <- dataFeaturesNames$V2

## 1.3. Merge columns to get the data frame Data for all data
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## 2.1. Subset Name of Features by measurements on the mean and standard deviation
## i.e taken Names of Features with "mean()" or "std()"
## Extract using grep
subdataFeaturesNames <- dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
## 2.2. Subset the data frame Data by seleted names of Features
selectedNames <- c(as.character(subdataFeaturesNames), "subject", "activity" )
Data <- subset(Data,select=selectedNames)
## 2.3. Test : Check the structures of the data frame Data
str(Data)

# 3. Uses descriptive activity names to name the activities in the data set

## 3.1. Read descriptive activity names from "activity_labels.txt"
activityLabels <- read.table(file.path(path, "activity_labels.txt"),header = FALSE)
## 3.2. Factorize Variale activity in the data frame Data using descriptive activity names
Data$activity <- factor(Data$activity, labels=activityLabels[,2])
## 3.3. Test Print
head(Data$activity, 30)

# 4. Appropriately labels the data set with descriptive variable names

names(Data) <- gsub("^t", "time", names(Data))
names(Data) <- gsub("^f", "frequency", names(Data))
names(Data) <- gsub("Acc", "Accelerometer", names(Data))
names(Data) <- gsub("Gyro", "Gyroscope", names(Data))
names(Data) <- gsub("Mag", "Magnitude", names(Data))
names(Data) <- gsub("BodyBody", "Body", names(Data))
## 4.1. Test Print
names(Data)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

newData <- aggregate(. ~subject + activity, Data, mean)
newData <- newData[order(newData$subject,newData$activity),]
write.table(newData, file = "tidydata.txt", row.name = FALSE, quote = FALSE, sep = '\t')