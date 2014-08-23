run_analysis <- function(){

        ##reads the csv files using read.csv and read.fwf (for the X_test and X_train)
        subject_test<-read.csv("./test/subject_test.txt", header=FALSE)
        subject_train<-read.csv("./train/subject_train.txt", header=FALSE)

        testXdata <- read.fwf("./test/X_test.txt", header=FALSE, widths=c(rep(16,561)),buffersize=100)
        testYdata <- read.csv("./test/y_test.txt", sep=" ", header=FALSE)
        trainXdata <- read.fwf("./train/X_train.txt", header=FALSE, widths=c(rep(16,561)),buffersize=100)
        trainYdata <- read.csv("./train/y_train.txt", sep=" ", header=FALSE)
        
        ## create complete Test and Train datasets using all the fields provided across files
        testdata <- cbind(subject_test,testYdata,testXdata)
        traindata <- cbind(subject_train,trainYdata,trainXdata)

        ## changes the column names in mergedata
        names(testdata)[1] <- c("subject")
        names(traindata)[1] <- c("subject")

        ## Merges the training and the test sets to create one data set.
        mergedata <-rbind(testdata,traindata)

        ##reads the features file used to set the column names
        features <-read.csv("./features.txt", sep ="", header=FALSE)
        colnames(mergedata)[3:563] <- c(as.character(features[,2])) 

        ##extract only the measurements on the mean and standard deviation 
        test <- c(1,2,grep("std\\(\\)|mean\\(\\)",colnames(mergedata), ignore.case=TRUE))
        mean_std_data <-mergedata[,test]

        ##Uses descriptive activity names to name the activities in the data set
        activitynames <- read.csv("./activity_labels.txt", sep=" ", header=FALSE)
        colnames(activitynames)[1:2] <- c("activity", "activity_label")
        colnames(mean_std_data)[1:2] <- c("subject", "activity")
        mergeDdata <- merge(activitynames, mean_std_data, by.x="activity", by.y = "activity", all.y= TRUE)
        
        ##Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
        FinishedtidyData <- aggregate(mergeDdata, by=list(factor(mergeDdata$subject), factor(mergeDdata$activity), mergeDdata[[2]]), FUN=mean, na.rm=TRUE) 
        colnames(FinishedtidyData)[1:3]<-c("subject","activity","activity_label")
        FinishedtidyData <- FinishedtidyData[,-(4:6)]
        write.table(FinishedtidyData, file = "./tidydata.txt", append = FALSE, quote = TRUE, sep = " ", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, qmethod = c("escape", "double"), fileEncoding = "")

}
