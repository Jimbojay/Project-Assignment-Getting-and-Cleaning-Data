#Settings
library(plyr) # load plyr first, then dplyr 
library(data.table) # a prockage that handles dataframe better
library(dplyr) # for fancy data table manipulations and organization
#rm(list = ls(all = TRUE))
path_rf <- file.path("./data" , "UCI HAR Dataset")

#Download Data
if(!file.exists("./data")){dir.create("./data")}
if(!file.exists("data/Dataset.zip"))   {fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "}
if(!file.exists("data/Dataset.zip"))   {file <- file.path(getwd(), "data/Dataset.zip")}
if(!file.exists("data/Dataset.zip"))   {download.file(fileUrl, destfile=file, mode="wb")}

unzip(zipfile="./data/Dataset.zip",exdir="./data")


#Load data
dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)

dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)

Features <- read.table(file.path(path_rf, "features.txt"),header = FALSE)


#Fix the columnnames
colnames(dataFeaturesTrain) <- t(Features[2])
colnames(dataFeaturesTest) <- t(Features[2])



# 1.    Merges the training and the test sets to create one data set.

#Merge activities anc participants
dataFeaturesTrain$activities <- dataActivityTrain[, 1]
dataFeaturesTrain$participants <- dataSubjectTrain[, 1]
dataFeaturesTest$activities <- dataActivityTest[, 1]
dataFeaturesTest$participants <- dataSubjectTest[, 1]

#Create testset
Testset <- rbind(dataFeaturesTrain, dataFeaturesTest)
duplicated(colnames(Testset))
Testset <- Testset[, !duplicated(colnames(Testset))]


#2.     Extracts only the measurements on the mean and standard deviation for each measurement.

#Mean
Mean <- grep("mean()", names(Testset), value = FALSE, fixed = TRUE)
#In addition, we need to include 555:559 as they have means and are associated with the gravity terms
Mean <- append(Mean, 471:477)
InstrumentMeanMatrix <- Testset[Mean]

# Standard deviation
STD <- grep("std()", names(Testset), value = FALSE)
InstrumentSTDMatrix <- Testset[STD]



#3.     Uses descriptive activity names to name the activities in the data set

#First convert activities to charaters for facilitation of conversion
Testset$activities <- as.character(Testset$activities)

#Actual conversion
Testset$activities[Testset$activities == 1] <- "Walking"
Testset$activities[Testset$activities == 2] <- "Walking Upstairs"
Testset$activities[Testset$activities == 3] <- "Walking Downstairs"
Testset$activities[Testset$activities == 4] <- "Sitting"
Testset$activities[Testset$activities == 5] <- "Standing"
Testset$activities[Testset$activities == 6] <- "Laying"

#Convert activities to factors
Testset$activities <- as.factor(Testset$activities)


#4.     Appropriately labels the data set with descriptive variable names.
names(Testset) <- gsub("Acc", "Accelerator", names(Testset))
names(Testset) <- gsub("Mag", "Magnitude", names(Testset))
names(Testset) <- gsub("Gyro", "Gyroscope", names(Testset))
names(Testset) <- gsub("^t", "time", names(Testset))
names(Testset) <- gsub("^f", "frequency", names(Testset))

#First convert participants to charaters for facilitation of conversion
Testset$participants <- as.character(Testset$participants)

#Actual conversion
Testset$participants[Testset$participants == 1] <- "Participant 1"
Testset$participants[Testset$participants == 2] <- "Participant 2"
Testset$participants[Testset$participants == 3] <- "Participant 3"
Testset$participants[Testset$participants == 4] <- "Participant 4"
Testset$participants[Testset$participants == 5] <- "Participant 5"
Testset$participants[Testset$participants == 6] <- "Participant 6"
Testset$participants[Testset$participants == 7] <- "Participant 7"
Testset$participants[Testset$participants == 8] <- "Participant 8"
Testset$participants[Testset$participants == 9] <- "Participant 9"
Testset$participants[Testset$participants == 10] <- "Participant 10"
Testset$participants[Testset$participants == 11] <- "Participant 11"
Testset$participants[Testset$participants == 12] <- "Participant 12"
Testset$participants[Testset$participants == 13] <- "Participant 13"
Testset$participants[Testset$participants == 14] <- "Participant 14"
Testset$participants[Testset$participants == 15] <- "Participant 15"
Testset$participants[Testset$participants == 16] <- "Participant 16"
Testset$participants[Testset$participants == 17] <- "Participant 17"
Testset$participants[Testset$participants == 18] <- "Participant 18"
Testset$participants[Testset$participants == 19] <- "Participant 19"
Testset$participants[Testset$participants == 20] <- "Participant 20"
Testset$participants[Testset$participants == 21] <- "Participant 21"
Testset$participants[Testset$participants == 22] <- "Participant 22"
Testset$participants[Testset$participants == 23] <- "Participant 23"
Testset$participants[Testset$participants == 24] <- "Participant 24"
Testset$participants[Testset$participants == 25] <- "Participant 25"
Testset$participants[Testset$participants == 26] <- "Participant 26"
Testset$participants[Testset$participants == 27] <- "Participant 27"
Testset$participants[Testset$participants == 28] <- "Participant 28"
Testset$participants[Testset$participants == 29] <- "Participant 29"
Testset$participants[Testset$participants == 30] <- "Participant 30"

#Convert participants to factors
Testset$participants <- as.factor(Testset$participants)



#5.     Create a tidy data set
Testset.dt <- data.table(Testset)
#This takes the mean of every column broken down by participants and activities
TidyData <- Testset.dt[, lapply(.SD, mean), by = 'participants,activities']
write.table(TidyData, file = "Tidy.txt", row.names = FALSE)