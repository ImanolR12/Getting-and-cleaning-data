library(dplyr)
library(plyr)

setwd("C:/Users/imano/Downloads/getdata_projectfiles_UCI HAR")

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# Merge data
x1<-union(x_test, x_train)
y2<-union_all(y_test, y_train)
subject<-union_all(subject_test,subject_train)
data<-cbind(x1,y2,subject)
df<-data_frame(data)

# Extracts only the measurements on the mean and standard deviation for each measurement. 

meanandsd <- grep(".*Mean.*|.*Std.*", names(df), ignore.case=TRUE)

requiredcolumns <- c(meanandsd, 562, 563)

df_meanandsd <- df[,requiredcolumns]

dim(df_meanandsd)
colnames(df_meanandsd)

# Uses descriptive activity names to name the activities in the data set

df_meanandsd$code <- factor(df_meanandsd$code, levels = activities[,1], labels = activities[,2]) 
head(df_meanandsd$code,20)

# Appropriately labels the data set with descriptive variable names.

#prefix t is replaced by time
#Acc is replaced by Accelerometer
#Gyro is replaced by Gyroscope
#prefix f is replaced by frequency
#Mag is replaced by Magnitude
#BodyBody is replaced by Body

names(df_meanandsd)<-gsub("^t", "time", names(df_meanandsd))
names(df_meanandsd)<-gsub("^f", "frequency", names(df_meanandsd))
names(df_meanandsd)<-gsub("Acc", "Accelerometer", names(df_meanandsd))
names(df_meanandsd)<-gsub("Gyro", "Gyroscope", names(df_meanandsd))
names(df_meanandsd)<-gsub("Mag", "Magnitude", names(df_meanandsd))
names(df_meanandsd)<-gsub("BodyBody", "Body", names(df_meanandsd))

names(df_meanandsd)


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)
df2<-aggregate(. ~subject + code, df_meanandsd, mean)
head(df2)


df2<-df2[order(df2$subject,df2$code),]

write.table(df2, file = "tidydata.txt",row.name=FALSE)
