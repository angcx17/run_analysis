## Load library
library(dplyr)

## Read test, train, features, and activity labels data
subject_test <- read.table(".//test/subject_test.txt")
xtest <- read.table(".//test/x_test.txt")
ytest <- read.table(".//test/y_test.txt")
subject_train <- read.table(".//train/subject_train.txt")
xtrain <- read.table(".//train/x_train.txt")
ytrain <- read.table(".//train/y_train.txt")
features <- read.table(".//features.txt")
labels <- read.table(".//activity_labels.txt")

## Label with descriptive variable names
colnames(xtrain) <- features[ ,2]
colnames(ytrain) <- "activityid"
colnames(subject_train) <- "subjectid"
colnames(xtest) <- features[ ,2]
colnames(ytest) <- "activityid"
colnames(subject_test) <- "subjectid"
colnames(labels) <- c("activityid", "type")

## Merging test and train tables
trainmerge <- cbind(ytrain, subject_train, xtrain)
testmerge <- cbind(ytest, subject_test, xtest)
full <- rbind(trainmerge, testmerge)

## Extracting only measurements on mean and standard deviation
meanstd <- (grepl("activityid", colnames(full)) | grepl("subjectid", colnames(full)) 
            | grepl("mean", colnames(full)) | grepl("std", colnames(full)))
newmeanstd <- full[ , meanstd]

## Use descriptive activity names to name activities
actnames <- merge(newmeanstd, labels, by = "activityid", all.x = TRUE)

## Creating a dataset with the average of each variable by subject and activity
final <- actnames %>%
          group_by(subjectid, type) %>%
          summarise_all(funs(mean))

## Final ordering
final <- final[order(final$subjectid, final$activityid), ]

