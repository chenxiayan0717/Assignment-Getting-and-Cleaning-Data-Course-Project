#reading the files in the folder with read.table
subject_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt", header=FALSE, sep="")
x_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt", header=FALSE, sep="")
y_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt", header=FALSE, sep="")
body_acc_x_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt", header=FALSE, sep="")
body_acc_y_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt", header=FALSE, sep="")
body_acc_z_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt", header=FALSE, sep="")
body_gyro_x_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt", header=FALSE, sep="")
body_gyro_y_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt", header=FALSE, sep="")
body_gyro_z_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt", header=FALSE, sep="")
total_acc_x_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt", header=FALSE, sep="")
total_acc_y_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt", header=FALSE, sep="")
total_acc_z_test <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt", header=FALSE, sep="")
subject_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt", header=FALSE)
x_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt", header=FALSE, sep="")
y_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt", header=FALSE, sep="")
body_acc_x_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt", header=FALSE, sep="")
body_acc_y_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt", header=FALSE, sep="")
body_acc_z_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt", header=FALSE, sep="")
body_gyro_x_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt", header=FALSE, sep="")
body_gyro_y_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt", header=FALSE, sep="")
body_gyro_z_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt", header=FALSE, sep="")
total_acc_x_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt", header=FALSE, sep="")
total_acc_y_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt", header=FALSE, sep="")
total_acc_z_train <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt", header=FALSE, sep="")
features <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt", header=FALSE)
activity_labels <- read.table("C:/Users/xiaya/Desktop/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt", header=FALSE)
library(dplyr)
#Q1 Merges the training and the test sets to create one data set
x_combined <- rbind(x_train, x_test)
y_combined <- rbind(y_train, y_test)
subject_combined <- rbind(subject_train, subject_test)
body_acc_x_combined <- rbind(body_acc_x_train, body_acc_x_test)
body_acc_y_combined <- rbind(body_acc_y_train, body_acc_y_test)
body_acc_z_combined <- rbind(body_acc_z_train, body_acc_z_test)

body_gyro_x_combined <- rbind(body_gyro_x_train, body_gyro_x_test)
body_gyro_y_combined <- rbind(body_gyro_y_train, body_gyro_y_test)
body_gyro_z_combined <- rbind(body_gyro_z_train, body_gyro_z_test)

total_acc_x_combined <- rbind(total_acc_x_train, total_acc_x_test)
total_acc_y_combined <- rbind(total_acc_y_train, total_acc_y_test)
total_acc_z_combined <- rbind(total_acc_z_train, total_acc_z_test)
#Q2 Extracts only the measurements on the mean and standard deviation for each measurement
mean_std_indices <- grep("-(mean|std)\\(\\)", features[, 2])
x_mean_std <- x_combined[, mean_std_indices]

#Q3 Uses descriptive activity names to name the activities in the data set
combined_activity_names <- left_join(y_combined, activity_labels, by = "V1")

#Q4 Appropriately labels the data set with descriptive variable names
selected_features_names <- features[mean_std_indices, 2]
colnames(x_mean_std) <- selected_features_names

#Q5 creates a second, independent tidy data set with the average of each variable for each activity and each subject
colnames(combined_activity_names) <- c("ActivityID", "ActivityName")
combined_data <- cbind(subject_combined, combined_activity_names, x_mean_std)
colnames(combined_data)[1]<- 'Subject'
tidy_data <- combined_data %>%
  group_by(Subject, ActivityName) %>%
  summarise(across(everything(), mean, na.rm = TRUE))
