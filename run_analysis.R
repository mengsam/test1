## Assign definitions 
zipFile <- "UCI HAR Dataset.zip"
zipURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00240/UCI%20HAR%20Dataset.zip"
zipName <- "UCI HAR Dataset"
Resultsfolder <- "Results"

## Load plyr package 
library(plyr)

## Download zipFile and create Results folder
download.file(zipURL,zipFile,mode="wb")
dir.create(Resultsfolder)

## getTable function allows a table to be read from the zipFile
## Cols = Null is applied and is used later to define column names of train and test data sets
getTable <- function (filename,cols = NULL){
  print(paste("Getting table:", filename))
  f <- unz(zipFile, paste(zipName,filename,sep="/"))
  data <- data.frame()
  if(is.null(cols)){
  data <- read.table(f,sep="",stringsAsFactors=F)} 
  else 
  {data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)}
  data}

## use getTable function to read and create a combined dataset
## 'type' can be test or train 
getData <- function(type, features){
  print(paste("Getting data", type))
  subject_data <- getTable(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y_data <- getTable(paste(type,"/","y_",type,".txt",sep=""),"activity")    
  x_data <- getTable(paste(type,"/","X_",type,".txt",sep=""),features$V2) 
  return (cbind(subject_data,y_data,x_data))}

## saveOutcome function produces results into a CSV file
saveOutcome <- function (data,name) {
print(paste("Saving output", name))
zipFile <- paste(Resultsfolder,"/", name,".csv" ,sep="")
write.csv(data,zipFile)}

## Assign column names for test and train datasets
features <- getTable("features.txt")

## Read dataset
train <- getData("train",features)
test <- getData("test",features)

##Q1: Merges the training and the test sets to create one data set
datasetQ1 <- rbind(train, test)
datasetQ1 <- arrange(datasetQ1, id) 
saveOutcome (datasetQ1,"datasetQ1")

##Q2: Extracts only the measurements on the mean and standard deviation for each measurement
datasetQ2 <- datasetQ1[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveOutcome(datasetQ2,"datasetQ2") ## save results

##Q3 & Q4 - Done
activity_label <- getTable("activity_labels.txt")
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

##Q5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
datasetQ5 <- ddply(datasetQ2, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(datasetQ5)[-c(1:2)] <- paste(colnames(datasetQ2)[-c(1:2)], "_mean", sep="") ## adds mean to column names
saveOutcome(datasetQ5,"datasetQ5") ##save results 
