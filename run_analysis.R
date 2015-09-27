##Zip file was extracted into working directory
##Make sure we have the libraries we need
library(plyr)
library(dplyr)

## start by reading all the files we want
traindata <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainexercisedata <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainsubjectdata <- read.table("./UCI HAR Dataset/train/subject_train.txt")

testdata <- read.table("./UCI HAR Dataset/test/X_test.txt")
testexercisedata <- read.table("./UCI HAR Dataset/test/y_test.txt")
testsubjectdata <- read.table("./UCI HAR Dataset/test/subject_test.txt")

varnames <- read.table("./UCI HAR Dataset/features.txt")

## then we have to rename the columns for the two tables we want
varnames1 <- varnames[,2]
colnames(traindata) <- varnames1
colnames(testdata) <- varnames1

#then let's add the exercise and subject ids
traindata <- cbind(trainsubjectdata, trainexercisedata, traindata)
testdata <- cbind(testsubjectdata, testexercisedata, testdata)

## then we have to merge these two datasets
fulldata <- rbind(traindata, testdata)
names(fulldata)[1] <- "subject"
names(fulldata)[2] <- "exercise"

## Then Extracts only the measurements on the mean and standard deviation for each measurement. 
reduceddata <- fulldata[,grep("mean|std|Mean", names(fulldata))]
reduceddata <- cbind(fulldata[,1:2], reduceddata)

##Uses descriptive activity names to name the activities in the data set
for (i in 1:nrow(reduceddata)) {
     if (reduceddata[i,2] == 1) {
          reduceddata[i,2] <- "Walking"
     }
}
for (i in 1:nrow(reduceddata)) {
     if (reduceddata[i,2] == 2) {
          reduceddata[i,2] <- "WalkingUpstairs"
     }
}
for (i in 1:nrow(reduceddata)) {
     if (reduceddata[i,2] == 3) {
          reduceddata[i,2] <- "WalkingDownstairs"
     }
}
for (i in 1:nrow(reduceddata)) {
     if (reduceddata[i,2] == 4) {
          reduceddata[i,2] <- "Sitting"
     }
}
for (i in 1:nrow(reduceddata)) {
     if (reduceddata[i,2] == 5) {
          reduceddata[i,2] <- "Standing"
     }
}
for (i in 1:nrow(reduceddata)) {
     if (reduceddata[i,2] == 6) {
          reduceddata[i,2] <- "LayingDown"
     }
}

## Appropriately labels the data set with descriptive variable names. 
##This step was mostly done beforehand by assigning the measurement names
##But they could probably be improved a bit. Namely we can make it clear
## that "Acc" means "Acceleration"

for (i in 1:ncol(reduceddata)) {
     if (sum(grep("Acc",colnames(reduceddata)[i]))>0) {
          print(colnames(reduceddata)[i]) 
          temp <- strsplit(colnames(reduceddata)[i], "Acc")
          print(temp)
          temp <- unlist(temp)
          print(temp)
          temp[3] <- temp[2]
          print(temp)
          temp[2] <- "Acceleration"
          print(temp)
          newname<-paste0(temp[1],temp[2],temp[3])
          print(newname)
          colnames(reduceddata)[i] <- newname
          print(colnames(reduceddata)[i])
          
     }
}

## Last is to create a second, independent tidy data set with the average 
## of each variable for each activity and each subject.
reduceddata<-group_by(reduceddata, subject, exercise)
finaldata<- summarize_each(reduceddata, funs(mean))
print(finaldata)