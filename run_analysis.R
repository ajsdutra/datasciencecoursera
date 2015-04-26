#
#   Getting and Cleaning Data --- Course Project
#

# Initializing code:
library(dplyr)

# Downloading and unzipping data file:
# file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# file_name <- "uci_har_data.zip"
# download.file(url = file_url, destfile = file_name)
# unzip(file_name)

uci_har_dir <- "./UCI HAR Dataset/"
fname <- paste(uci_har_dir, "features.txt", sep = "")
features <- read.csv(fname, header = FALSE, sep = "")   # list of features
colnames(features)[2] <- "name"

fname <- paste(uci_har_dir, "activity_labels.txt", sep = "")
act_labels <- read.csv(fname, header = FALSE, sep = "")   # list of activities
colnames(act_labels)[2] <- "activity"

# Reading train data:
train_dir <- "./UCI HAR Dataset/train/"
train_files <- dir(train_dir)
fname <- paste(train_dir, train_files[2], sep = "")
train_subj <- read.csv(fname, header = FALSE, sep = "") # subject data
fname <- paste(train_dir, train_files[3], sep = "")
train_data <- read.csv(fname, header = FALSE, sep = "") # features data
fname <- paste(train_dir, train_files[4], sep = "")
train_labels <- read.csv(fname, header = FALSE, sep = "") # label data

# Reading test data:
test_dir <- "./UCI HAR Dataset/test/"
test_files <- dir(test_dir)
fname <- paste(test_dir, test_files[2], sep = "")
test_subj <- read.csv(fname, header = FALSE, sep = "") # subject data
fname <- paste(test_dir, test_files[3], sep = "")
test_data <- read.csv(fname, header = FALSE, sep = "") # features data
fname <- paste(test_dir, test_files[4], sep = "")
test_labels <- read.csv(fname, header = FALSE, sep = "") # label data

# Item 01 from the assignment:
# Merging train+test data into one single dataset:
har_subj <- rbind(train_subj, test_subj)
har_data <- rbind(train_data, test_data)
har_labels <- rbind(train_labels, test_labels)
# Labels data with appropriate descriptive variable names (item 03):
colnames(har_data) <- features$name

# Memory clearing -- getting rid of unnecessary data:
rm(train_subj, train_data, train_labels, test_subj, test_data, test_labels)

# Item 02 from the assignment:
# Extract measurements on the mean and standard deviation:
mean_feat_idx <- grep(pattern = "mean()", x = features$name, fixed = TRUE)
    # find measurements on the mean.
std_feat_idx <- grep(pattern = "std()", x = features$name, fixed = TRUE)
    # find measurements on the standard deviation

# Temporary indices for relevant data extraction:
t1 <- features$V1 %in% mean_feat_idx
t2 <- features$V1 %in% std_feat_idx
rel_feat_idx <- t1 | t2

rm(t1, t2)  # Cleaning up.

# Keeping only relevant data:
rel_data <- har_data[, rel_feat_idx]
    #   'rel_data' contains only the measurements on the mean and standard 
    #   deviation for each measurement.

# Items 03 and 04 from the assignment:
# Use descriptive activity names:
# Add 'subject' and 'activity' columns to merged dataset, with appropriate
# descriptive column names:
rel_data <- cbind(har_subj, act_labels$activity[har_labels$V1], rel_data)
colnames(rel_data)[1] <- "Subject"
colnames(rel_data)[2] <- "Activity"

# Item 05 from the assignment:
# From the present dataset (rel_data), create an independent dataset with the 
# average of each variable for each activity and each subject:
t_data <- rel_data %>% group_by(Subject, Activity) %>% 
    arrange(Subject, Activity)

out_df <- aggregate(t_data[, 3:68], by = list(Subject = t_data$Subject, 
                              Actvity = t_data$Activity), mean)

# Writes tidy dataset to a file:
write.table(out_df, file = "tidy_dataset.txt", row.names = FALSE, sep = ",")
