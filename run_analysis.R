#Read in the test data
library(data.table)
filePath <- paste (getwd(),"/UCI HAR Dataset/test/X_test.txt", sep = "", collapse = NULL)
testData <- fread(filePath)

#Read in the feature names so that they can be used to label the columns
filePath <- paste (getwd(),"/UCI HAR Dataset/features.txt", sep = "", collapse = NULL)
featureNames <- read.table(filePath)

#Convert featureNames to a character vector so it can be used to rename the columns
ftNames <- as.character(featureNames[,2])

#Rename the columns
setnames(testData,names(testData),ftNames)

#Read in the test activities
filePath <- paste (getwd(),"/UCI HAR Dataset/test/y_test.txt", sep = "", collapse = NULL)
testActivities <- read.table(filePath)
setnames(testActivities,"V1","activitylabel")

#Add the activities as new column
testData$activitylabel <- testActivities

#Add the test subject
filePath <- paste (getwd(),"/UCI HAR Dataset/test/subject_test.txt", sep = "", collapse = NULL)
testSubject <- read.table(filePath)
setnames(testSubject,"V1","subject")

#Add the test subject as new column
testData$subject <- testSubject

#Read in the train data and rename
filePath <- paste (getwd(),"/UCI HAR Dataset/train/X_train.txt", sep = "", collapse = NULL)
trainData <- fread(filePath)
setnames(trainData,names(trainData),ftNames)

#Read in the train activities
filePath <- paste (getwd(),"/UCI HAR Dataset/train/y_train.txt", sep = "", collapse = NULL)
trainActivities <- read.table(filePath)
setnames(trainActivities,"V1","activitylabel")

#Add the train activities as new column
trainData$activitylabel <- trainActivities

#Add the train subject
filePath <- paste (getwd(),"/UCI HAR Dataset/train/subject_train.txt", sep = "", collapse = NULL)
trainSubject <- read.table(filePath)
setnames(trainSubject,"V1","subject")

#Add the test subject as new column
trainData$subject <- trainSubject

#Append train data to test data

cols <- c("subject","activitylabel",grep("[Mm]ean\\(|std\\(",names(testData),value = TRUE))
testData <- testData[, cols,with = FALSE]
trainData <- trainData[, cols,with = FALSE]

mergedData <- rbind(testData,trainData)

#Read in the activity lables
filePath <- paste (getwd(),"/UCI HAR Dataset/activity_labels.txt", sep = "", collapse = NULL)
activityLabels <- read.table(filePath)
setnames(activityLabels,"V1","activitylabel")
setnames(activityLabels,"V2","activityname")

#Subset the columns to include only mean() and std(), activitylabel and subject
cols <- c("subject","activitylabel",grep("[Mm]ean\\(|std\\(",names(mergedData),value = TRUE))
mergedData <- mergedData[, cols,with = FALSE]

#Match the columns of the activity labels to provide a full description of the activity
mergedData = merge(mergedData,activityLabels,by.x = "activitylabel",by.y = "activitylabel",all=TRUE)
mergedData$activitylabel <- mergedData$activityname
mergedData <- mergedData[, cols,with = FALSE]

#reshape to create the tidy data
library(reshape2)

#convert back to data frame 
mergedData <- as.data.frame(mergedData)

#Melt the data
colMeasures <- grep("[Mm]ean\\(|std\\(",names(mergedData),value = TRUE)
mergeDataMelt <- melt(mergedData,id=c("subject","activitylabel"),measure.vars = colMeasures)

#Dcast the data to get the average
tidyData <- dcast(mergeDataMelt, subject + activitylabel ~ variable,mean)
