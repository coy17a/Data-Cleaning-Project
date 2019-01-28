library(dplyr)
library(data.table)
library(memisc)
setwd("C:/Users/Lu/Documents/Data Scientist Specialization/Getting and Cleaning Data/Project")
feature <- read.table("features.txt")

## 1. Merge the training and test sets to create one data set
# training data set
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
train <- cbind(subject_train,y_train,x_train)

# test data set
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")
test <- cbind(subject_test,y_test,x_test)

# combine
all <- rbind(train,test)

a <- data.frame(-1,"ID")
b <- data.frame(0,"Y")

names(a) <- c("V1","V2")
names(b) <- c("V1","V2")
namelist <- rbind(a,b,feature)

# fix duplicate column names
colnames(all) <- namelist$V2
valid_names <- make.names(names = names(all), unique=TRUE, allow_=TRUE)
names(all) <- valid_names

## 2. Extracts only the measurements on the mean and standard deviation
meansd <- dplyr::select(all,ID,Y,contains("mean"), contains("std"))

## 3. name the activities in the data set
act.label <- read.table("activity_labels.txt")
select.data <- merge(act.label, meansd, by.x="V1", by.y="Y")

## 4. Appropriately labels the data set
colnames(select.data)[1] <- "activity.num"
colnames(select.data)[2] <- "activity"
name.list <- 
names(select.data) <- gsub("...",".",names(select.data),fixed = TRUE)
names(select.data) <- gsub("..","",names(select.data),fixed = TRUE)

## 5. the average of each variable for each activity and each subject
mean.all <- aggregate(select.data[,4:89], list(select.data$ID,select.data$activity), mean)
mean.all = select.data %>%
        group_by(ID, activity) %>%
        summarise_at(.vars=names(.)[4:89], .funs=funs(mean))
write.table(mean.all,"Finaldataset.txt", sep="\t",row.names = FALSE)