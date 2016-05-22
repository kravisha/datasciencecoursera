run_analysis <- function() {
    #    trainFile <- "C:/data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train_test.txt"
    #    subjectTrainFile <- "C:/data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train_test.txt"
    #    activityTrainFile <- "C:/data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train_test.txt"
    #    testFile <- "C:/data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test_test.txt"
    #    subjectTestFile <- "C:/data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test_test.txt"
    #    activityTestFile <- "C:/data/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test_test.txt"

    # Read filename dirctories    
    trainFile <- "X_train.txt"
    subjectTrainFile <- "subject_train.txt"
    activityTrainFile <- "y_train.txt"
    labelsFile <-    "features.txt"
    testFile <- "X_test.txt"
    subjectTestFile <- "subject_test.txt"
    activityTestFile <- "y_test.txt"

    # Read data into Dataframes

    trainData <- read.fwf(trainFile,widths=rep(16,561))
    testData <- read.fwf(testFile,widths=rep(16,561))
    subjectTrainData <-read.csv(subjectTrainFile,header=FALSE)
    subjectTestData <-read.csv(subjectTestFile,header=FALSE)
    activityTrainData <-read.csv(activityTrainFile,header=FALSE)
    activityTestData <-read.csv(activityTestFile,header=FALSE)
    
    #create Activities table - it is small and so manually created
    a1 <- c(1,2,3,4,5,6)
    a2 <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
    activities <- cbind(a1,a2)
    activities <- data.frame(activities)
    col_headings <- c('Activity','ActivityName')
    names(activities) <- col_headings
    
    #merge Subject data
    allsubjectData <- rbind(subjectTrainData,subjectTestData)
    col_headings <- c('Subject')
    names(allsubjectData) <- col_headings
    
    #merge All Activity Data
    allactivityData <- rbind(activityTrainData,activityTestData)
    col_headings <- c('Activity')
    names(allactivityData) <- col_headings
    
    #berge Train and Test data
    allData <- rbind(trainData,testData)

    #read the labels file and save to name vector
    labels <- read.table(labelsFile, sep=" ",header=FALSE)
    myNamevec <- labels[,2]
    
    #change column names to NameVector
    names(allData) <- myNamevec
    #create the filtered names vector
    #get only the fields corresponding to mean and std deviation
    filteredNames <- grep("mean|std",myNamevec) 
    #filter the appropriate columns
    filteredData <- allData[,filteredNames]
    
    # merge the subject and activity info into the main data set
    filteredData <- cbind(allsubjectData,filteredData)
    filteredData <- cbind(allactivityData,filteredData)
    
    #lookup activity names for activity ID and make it pat of dataset
    mergedData = merge(activities,filteredData,by.x="Activity",by.y="Activity",all=FALSE)
    
    #clean up "()" and "-"
    names(mergedData) <- gsub("\\(|\\)", "", names(mergedData))
    names(mergedData) <- gsub("-", "_", names(mergedData))

    #gropy data by activity and subject    
    activitySubjectGroupedData <- group_by(mergedData,ActivityName,Subject)
    # summarize the data to get the mean for each variable
    finalData <- summarize(activitySubjectGroupedData,
                           tBodyAccstdZ=mean(tBodyAcc_std_Z),
                           tGravityAccmeanX=mean(tGravityAcc_mean_X),
                           tGravityAccmeanY=mean(tGravityAcc_mean_Y),
                           tGravityAccmeanZ=mean(tGravityAcc_mean_Z),
                           tGravityAccstdX=mean(tGravityAcc_std_X),
                           tGravityAccstdY=mean(tGravityAcc_std_Y),
                           tGravityAccstdZ=mean(tGravityAcc_std_Z),
                           tBodyAccJerkmeanX=mean(tBodyAccJerk_mean_X),
                           tBodyAccJerkmeanY=mean(tBodyAccJerk_mean_Y),
                           tBodyAccJerkmeanZ=mean(tBodyAccJerk_mean_Z),
                           tBodyAccJerkstdX=mean(tBodyAccJerk_std_X),
                           tBodyAccJerkstdY=mean(tBodyAccJerk_std_Y),
                           tBodyAccJerkstdZ=mean(tBodyAccJerk_std_Z),
                           tBodyGyromeanX=mean(tBodyGyro_mean_X),
                           tBodyGyromeanY=mean(tBodyGyro_mean_Y),
                           tBodyGyromeanZ=mean(tBodyGyro_mean_Z),
                           tBodyGyrostdX=mean(tBodyGyro_std_X),
                           tBodyGyrostdY=mean(tBodyGyro_std_Y),
                           tBodyGyrostdZ=mean(tBodyGyro_std_Z),
                           tBodyGyroJerkmeanX=mean(tBodyGyroJerk_mean_X),
                           tBodyGyroJerkmeanY=mean(tBodyGyroJerk_mean_Y),
                           tBodyGyroJerkmeanZ=mean(tBodyGyroJerk_mean_Z),
                           tBodyGyroJerkstdX=mean(tBodyGyroJerk_std_X),
                           tBodyGyroJerkstdY=mean(tBodyGyroJerk_std_Y),
                           tBodyGyroJerkstdZ=mean(tBodyGyroJerk_std_Z),
                           tBodyAccMagmean=mean(tBodyAccMag_mean),
                           tBodyAccMagstd=mean(tBodyAccMag_std),
                           tGravityAccMagmean=mean(tGravityAccMag_mean),
                           tGravityAccMagstd=mean(tGravityAccMag_std),
                           tBodyAccJerkMagmean=mean(tBodyAccJerkMag_mean),
                           tBodyAccJerkMagstd=mean(tBodyAccJerkMag_std),
                           tBodyGyroMagmean=mean(tBodyGyroMag_mean),
                           tBodyGyroMagstd=mean(tBodyGyroMag_std),
                           tBodyGyroJerkMagmean=mean(tBodyGyroJerkMag_mean),
                           tBodyGyroJerkMagstd=mean(tBodyGyroJerkMag_std),
                           fBodyAccmeanX=mean(fBodyAcc_mean_X),
                           fBodyAccmeanY=mean(fBodyAcc_mean_Y),
                           fBodyAccmeanZ=mean(fBodyAcc_mean_Z),
                           fBodyAccstdX=mean(fBodyAcc_std_X),
                           fBodyAccstdY=mean(fBodyAcc_std_Y),
                           fBodyAccstdZ=mean(fBodyAcc_std_Z),
                           fBodyAccmeanFreqX=mean(fBodyAcc_meanFreq_X),
                           fBodyAccmeanFreqY=mean(fBodyAcc_meanFreq_Y),
                           fBodyAccmeanFreqZ=mean(fBodyAcc_meanFreq_Z),
                           fBodyAccJerkmeanX=mean(fBodyAccJerk_mean_X),
                           fBodyAccJerkmeanY=mean(fBodyAccJerk_mean_Y),
                           fBodyAccJerkmeanZ=mean(fBodyAccJerk_mean_Z),
                           fBodyAccJerkstdX=mean(fBodyAccJerk_std_X),
                           fBodyAccJerkstdY=mean(fBodyAccJerk_std_Y),
                           fBodyAccJerkstdZ=mean(fBodyAccJerk_std_Z),
                           fBodyAccJerkmeanFreqX=mean(fBodyAccJerk_meanFreq_X),
                           fBodyAccJerkmeanFreqY=mean(fBodyAccJerk_meanFreq_Y),
                           fBodyAccJerkmeanFreqZ=mean(fBodyAccJerk_meanFreq_Z),
                           fBodyGyromeanX=mean(fBodyGyro_mean_X),
                           fBodyGyromeanY=mean(fBodyGyro_mean_Y),
                           fBodyGyromeanZ=mean(fBodyGyro_mean_Z),
                           fBodyGyrostdX=mean(fBodyGyro_std_X),
                           fBodyGyrostdY=mean(fBodyGyro_std_Y),
                           fBodyGyrostdZ=mean(fBodyGyro_std_Z),
                           fBodyGyromeanFreqX=mean(fBodyGyro_meanFreq_X),
                           fBodyGyromeanFreqY=mean(fBodyGyro_meanFreq_Y),
                           fBodyGyromeanFreqZ=mean(fBodyGyro_meanFreq_Z),
                           fBodyAccMagmean=mean(fBodyAccMag_mean),
                           fBodyAccMagstd=mean(fBodyAccMag_std),
                           fBodyAccMagmeanFreq=mean(fBodyAccMag_meanFreq),
                           fBodyBodyAccJerkMagmean=mean(fBodyBodyAccJerkMag_mean),
                           fBodyBodyAccJerkMagstd=mean(fBodyBodyAccJerkMag_std),
                           fBodyBodyAccJerkMagmeanFreq=mean(fBodyBodyAccJerkMag_meanFreq),
                           fBodyBodyGyroMagmean=mean(fBodyBodyGyroMag_mean),
                           fBodyBodyGyroMagstd=mean(fBodyBodyGyroMag_std),
                           fBodyBodyGyroMagmeanFreq=mean(fBodyBodyGyroMag_meanFreq),
                           fBodyBodyGyroJerkMagmean=mean(fBodyBodyGyroJerkMag_mean),
                           fBodyBodyGyroJerkMagstd=mean(fBodyBodyGyroJerkMag_std),
                           fBodyBodyGyroJerkMagmeanFreq=mean(fBodyBodyGyroJerkMag_meanFreq))
    #use Reshape2 library for tidying the data
    library(reshape2)
    # Tidy the data for each combination of Activity and Subject
    TidyData <- melt(finalData, id.vars=c("ActivityName", "Subject"))
    
    # Tidy teh data to make it more meaningful
    TidyData$variable <- gsub("mean"," Mean",TidyDataTemp$variable)
    TidyData$variable <- gsub("stdZ"," Standard Deviation for Z",TidyDataTemp$variable)
    TidyData$variable <- gsub("stdX"," Standard Deviation for X",TidyDataTemp$variable)
    TidyData$variable <- gsub("stdY"," Standard Deviation for Y",TidyDataTemp$variable)
    TidyData$variable <- gsub("MeanX"," Mean for X",TidyDataTemp$variable)
    TidyData$variable <- gsub("MeanY"," Mean for Y",TidyDataTemp$variable)
    TidyData$variable <- gsub("MeanZ"," Mean for Z",TidyDataTemp$variable)
    TidyData$variable <- gsub("Gyro"," Gyroscope",TidyDataTemp$variable)
    TidyData$variable <- gsub("Acc"," Accelerometer",TidyDataTemp$variable)
    
    setwd("C:/Users/User/Documents/R/course1 Assignments/datasciencecoursera") 
    write.csv(TidyDataTemp, file = "TidyData.csv")
    
    return(TidyData)
    
}