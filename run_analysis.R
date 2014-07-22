library(data.table)
library(reshape2)

## Merges the training and the test sets to create one data set.
setwd("C:/Users/Zhetong/Desktop/Coursera/Getting and Cleaning Data/UCI HAR Dataset")
list.files(getwd(), recursive = TRUE)
fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtSubjectTrain <- fileToDataTable(file.path(getwd(), "train", "subject_train.txt"))
dtSubjectTest <- fileToDataTable(file.path(getwd(), "test", "subject_test.txt"))

dtActivityTrain <- fileToDataTable(file.path(getwd(), "train", "Y_train.txt"))
dtActivityTest <- fileToDataTable(file.path(getwd(), "test", "Y_test.txt"))

dtTrain <- fileToDataTable(file.path(getwd(), "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(getwd(), "test", "X_test.txt"))

dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)

dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)

setkey(dt, subject, activityNum)


## Extracts only the measurements on the mean and standard deviation for each measurement.

dtFeatures <- fileToDataTable(file.path(getwd(), "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

## Uses descriptive activity names to name the activities in the data set

dtActivityNames <- fileToDataTable(file.path(getwd(), "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

## Appropriately labels the data set with descriptive variable names. 
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", 
            all.x = TRUE)
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)


grepthis <- function(regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))


r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
                           "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2


setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, 
       featJerk, featMagnitude, featVariable, featAxis)
dtTidydata <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
write.table(dtTidydata,"tidydata.txt")