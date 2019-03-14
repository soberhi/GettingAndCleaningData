library(dplyr)

folderProject <- "C:/Data Science/Assignments/Course 3/UCI HAR Dataset/"
folderTest <- paste(folderProject,"test/",sep="")
folderTraining <- paste(folderProject,"train/",sep="")

## read in all the data files
dtActivityLabels <- read.table(paste(folderProject, "activity_labels.txt", sep=""), header=FALSE)
dtFeatures <- read.table(paste(folderProject, "features.txt", sep=""), header=FALSE)
dtSubjectTest <- read.table(paste(folderTest, "subject_test.txt", sep=""), header=FALSE)
dtXTest <- read.table(paste(folderTest, "X_test.txt", sep=""), header=FALSE)
dtYTest <- read.table(paste(folderTest, "y_test.txt", sep=""), header=FALSE)
dtSubjectTrain <- read.table(paste(folderTraining, "subject_train.txt", sep=""), header=FALSE)
dtXTrain <- read.table(paste(folderTraining, "X_train.txt", sep=""), header=FALSE)
dtYTrain <- read.table(paste(folderTraining, "y_train.txt", sep=""), header=FALSE)

## give column headers to datasets
names(dtXTest) <- dtFeatures[,2]
names(dtXTrain) <- dtFeatures[,2]

## add the columns for the Activity and Subject
names(dtYTest) = "Activity"
names(dtSubjectTest) = "Subject"
dtXTestCombined=cbind(dtYTest,dtSubjectTest,dtXTest)

names(dtYTrain) = "Activity"
names(dtSubjectTrain) = "Subject"
dtXTrainCombined=cbind(dtYTrain,dtSubjectTrain,dtXTrain)

## STEP ONE: combine the test and training data into one data set
dtXCombined <- rbind(dtXTestCombined,dtXTrainCombined)

## STEP TWO: filter dataset for mean and std deviation measurements
toMatch <- c("mean", "std")
matches <- unique(grep(paste(toMatch,collapse="|"), dtFeatures[,2], value=TRUE))

## make sure that Activity and Subject remain
matches <- paste(c("Activity","Subject",matches))

## finish the filtering
dtXCombinedMeanSTD <- dtXCombined[matches]

## STEP THREE: add descriptions for the Activity 
index <- match(dtXCombinedMeanSTD$Activity,dtActivityLabels$V1)
dtXCombinedMeanSTD$Activity <- dtActivityLabels[index,]$V2

## STEP FOUR: descriptive column names
names(dtXCombinedMeanSTD) <- sub("^t", "Time ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("^f", "Frequency ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("Gyro", "(using gyrometer) ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("Acc", "(using accelerometer) ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("Body", "Body Acceleration ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("Gravity", "Gravity Acceleration ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("Jerk", "Jerk Signal ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("Mag", "Magnitude ", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("meanFreq", "Mean Frequency", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("\\(\\)", "", names(dtXCombinedMeanSTD))
names(dtXCombinedMeanSTD) <- sub("\\-", " ", names(dtXCombinedMeanSTD))

## STEP FIVE: find the average of each variable for each activity and subject
dtTidyData <- dtXCombinedMeanSTD %>%
  group_by(Activity, Subject) %>%
  summarise_all(funs(mean))

## write the tidy data set to csv text file
write.csv(dtTidyData, (paste(folderProject, "TidyData.txt", sep="")))