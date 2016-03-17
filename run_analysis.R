# run_analysis.R
# This script performs the following:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

run_analysis <- function() {
  #Load Activity and Feature Data
  features <- read.table("UCI HAR Dataset/features.txt")
  activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  #Identify required features (Mean and Std Dev)
  desiredFeatures <- grep(".*mean.*|.*std.*", features[,2])
  desiredFeaturesNames <- features[desiredFeatures,2]
  #Clean up headings
  desiredFeaturesNames = gsub("mean","Mean",desiredFeaturesNames)
  desiredFeaturesNames = gsub("std","Std",desiredFeaturesNames)
  desiredFeaturesNames <- gsub("[-()]","",desiredFeaturesNames)
  
  #Load Train Data
  trainID <- read.table("UCI HAR Dataset/train/subject_train.txt")
  trainActivity <- read.table("UCI HAR Dataset/train/y_train.txt")
  trainData <- read.table("UCI HAR Dataset/train/X_train.txt")[desiredFeatures]
  
  #Load Test Data
  testID <- read.table("UCI HAR Dataset/test/subject_test.txt")
  testActivity <- read.table("UCI HAR Dataset/test/y_test.txt")
  testData <- read.table("UCI HAR Dataset/test/X_test.txt")[desiredFeatures]
  
  #Combine ID, Activity & Data
  test <- cbind(testID,testActivity,testData)
  train <- cbind(trainID,trainActivity,trainData)
  
  #Merge Test and Train Data Sets
  full <- rbind(test,train)
  
  #Clean the Data Set
  colnames(full) <- c("subject","activity", desiredFeaturesNames)
  full$activity <- factor(full$activity, levels = activityLabels[,1], labels = activityLabels[,2])
  full$subject <- as.factor(full$subject)
  
  #Create Tidy Output with Mean for Variable for Each Subject and Activity Combination
  full.melt <- melt(full, id = c("subject", "activity"))
  full.mean <- dcast(full.melt, subject + activity ~ variable, mean)
  
  #Return the dataset
  full.mean
}