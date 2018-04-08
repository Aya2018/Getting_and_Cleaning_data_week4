#peer-grade assigntment

library(dplyr)

#download file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","week4_peer.zip")
unzip("week4_peer.zip")

#read data
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/Y_train.txt")
train_sub <-  read.table("./UCI HAR Dataset/train/subject_train.txt")

test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/Y_test.txt")
test_sub <-  read.table("./UCI HAR Dataset/test/subject_test.txt")

data_name <- read.table("./UCI HAR Dataset/features.txt")
labels <-read.table("./UCI HAR Dataset/activity_labels.txt")



#Merges the training and the test sets to create one data set.
x_data <- rbind(train_x,test_x)
y_data <- rbind(train_y,test_y)
sub_data <- rbind(train_sub,test_sub)


#remove raw data
rm("train_x","train_y","train_sub","test_x","test_y","test_sub")

#Extracts only the measurements on the mean and standard deviation for each measurement. 
var <- data_name %>%
        dplyr::filter(grepl("mean\\(\\)",V2) | grepl("std\\(\\)",V2))
x_data <- x_data[,var[,1]]


#Uses descriptive activity names to name the activities in the data set
colnames(y_data) <- "activity"
y_data$activitylabel <- factor(y_data$activity, labels = as.character(labels[,2]))
activitylabel <- y_data[, 2]


#Appropriately labels the data set with descriptive variable names. 
colnames(x_data) <- var[,2]

#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

colnames(sub_data) <- "subject"
total <- cbind(x_data,activitylabel,sub_data)
total_mean <- total %>%
        group_by(activitylabel,subject) %>%
        summarize_all(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/submitdata.txt",row.names = F)