# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

features <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# 1. Merges the training and the test sets to create one data set.
X_dataset <- rbind(X_train, X_test)
y_dataset <- rbind(y_train, y_test)

# 2. Extracts only the measurements on the mean and standard deviation 
# for each measurement. 
colnames(X_dataset) <- c(as.character(features[, 2]))
meanMeasurement <- grep("mean()", colnames(X_dataset), fixed = TRUE)
stdMeasurement <- grep("std()", colnames(X_dataset), fixed = TRUE)
measurementOnMeanStd <- X_dataset[, c(meanMeasurement, stdMeasurement)]

# 3. Uses descriptive activity names to name the activities in the data set
colnames(y_dataset) <- "Activity"
#dataset <- cbind(y_dataset, X_dataset)
y_X <- cbind(y_dataset, measurementOnMeanStd)

# 4. Appropriately labels the data set with descriptive variable names. 
activity_labels[, 2] <- as.character(activity_labels[, 2])
for (i in 1:nrow(y_X)) {
    y_X[i, 1] <- activity_labels[as.numeric(y_X[i, 1]), 2]
}

# 5. Creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject. 
subject <- rbind(subject_train, subject_test)
subj_y_X <- cbind(subject, y_X)
colnames(subj_y_X)[1] <- "Subject"

tidySet <- aggregate(subj_y_X[, 3] ~ Subject + Activity, subj_y_X, FUN = "mean")

for (j in 4:ncol(subj_y_X)) {
    tidySet[, j] <- aggregate(subj_y_X[, j] ~ Subject + Activity, subj_y_X, FUN = "mean")[, 3]
}

colnames(tidySet) <- colnames(subj_y_X)

# write to file
write.table(tidySet, file = "TidyDataset.txt")


# test <- read.table("./TidyDataset.txt")
# head(test)