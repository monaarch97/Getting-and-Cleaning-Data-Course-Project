#run_analysis.R
#To make a clean data set, outputting the resulting tidy data to a file named "tidy_data.txt"

library(dplyr)

# First of all, get the data
# Download the file first and save the zipped file to local document
# unzip zip file containing data if data directory doesn't already exist

# Reading the data
        #reading the training tables; x_train, y_train, and subject_train
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

        #reading the testing tables; x_test, y_test, subject_test
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

        #reading the features data, don't convert text labels to factors
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
## note: feature names (in features[, 2]) are not unique
##       e.g. fBodyAcc-bandsEnergy()-1,8

        #reading activity lables data
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


 # step 1 - Merging the train and test data
        #Making a single data table

humanActivity <- rbind(
        cbind(trainingSubjects, trainingValues, trainingActivity),
        cbind(testSubjects, testValues, testActivity)
)

        # remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

        # assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

#########

# step 2 - Extract only the mean and standard deviation for each measurement
        
        # the columns of data set we want to keep
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]


# step 3 - Uses descriptive activity names to name the activities

        # replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


# step 4 - Appropriately labels the data set with descriptive variable names
        # get column names
humanActivityCols <- colnames(humanActivity)

        # remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

        # expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

        # use new labels as column names
colnames(humanActivity) <- humanActivityCols

# step 5 - From the previous step data set, creates a second, independent tidy data set 
#       with the average of each variable for each activity and each subject.

        # group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
        group_by(subject, activity) %>%
        summarise_each(funs(mean))

        # output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)


