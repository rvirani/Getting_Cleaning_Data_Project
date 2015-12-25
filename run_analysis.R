########################################################################################################
# Course Project for Getting and Cleaning Data
# Goal is to prepare a tidy dataset from source that can ultimately be used for analysis later.
# Source Data : http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
########################################################################################################

# Step 1:  Load packages and source datasets::
library(dplyr)

  # Function to retrieve files individually from main source folder
  sourceFile<-function(file){
    paste("UCI HAR Dataset/", file, sep = "")
  }

  # Get training data 
    x_Train <- read.table(sourceFile("train/X_train.txt"))
    y_Train <- read.table(sourceFile("train/y_train.txt"))
  sub_Train <- read.table(sourceFile("train/subject_train.txt"))

  # Get test data
    x_Test <- read.table(sourceFile("test/X_Test.txt"))
    y_Test <- read.table(sourceFile("test/y_Test.txt"))
  sub_Test <- read.table(sourceFile("test/subject_Test.txt"))
  
  # Combine training and test data by group i.e., x or y 
             df.x <- rbind(x_Train,x_Test)
             df.y <- rbind(y_Train, y_Test)
       df.subject <- rbind(sub_Train, sub_Test)

# Step 2: Use the mean and standard deviation for each measurement:
  
 # use grep function in R to match and retrieve fields with mean and std
       features <- read.table(sourceFile("features.txt"))
    features.ms <- grep("-(mean|std)\\(\\)", features[,2])
           df.x <- df.x[,features.ms]
    names(df.x) <- features[features.ms, 2]
    names(df.x) <- gsub("\\(|\\)", "", names(df.x))

# Step 3: Provide descriptive col names for activities::
        activities <- read.table(sourceFile("activity_labels.txt"))
    activities[,2] <- gsub("_", "", tolower(as.character(activities[,2])))
          df.y[,1] <- activities[match(df.y[,1],activities[,1]),2]
       names(df.y) <- "activity"
       
# Step 4: Provide descriptive col name for subject data::       
 names(df.subject) <- "subject_id"
    
# Step 5: Combine into one dataset and write to text file::
            df.all <- cbind(df.subject, df.y,df.x)
            write.table(df.all, "tidyData.txt", row.names = FALSE)

# Step 6: From 'df.all' create a second text file with the averages of each activity and subject::
            df.all_summary <- df.all %>% group_by(subject_id, activity) %>% summarise_each(funs(mean))
            write.table(df.all_summary, "avgData.txt", row.names = FALSE)
    
