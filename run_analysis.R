# You should create one R script called run_analysis.R that does the following.
# 
# 1. Merges the training and the test sets to create one data set.
# Main idea of this section:
# rowbind x_test and x_train together then rowbind features to the test set.
# rowbind the y_test and y_train (activity) and the sub_test and sub_train together 
# and cbind the activities and the subjects to the front of everything 


# SET THE WD to the UCI HAR Dataset first!

library(data.table)
# Read everything
sub_test <- fread("test/subject_test.txt")
x_test <- fread("test/X_test.txt")
y_test <- fread("test/y_test.txt")
sub_train <- fread("train/subject_train.txt")
x_train <- fread("train/X_train.txt")
y_train <- fread("train/y_train.txt")
# join x
xj <- rbind(x_test, x_train)
#read and set the names of the features to attach those to this data before cbinding others
feat <- read.table("features.txt")
setnames(xj, names(xj), as.character(feat[,2]))
# rowbind the y and rename it "Activity"
yj <- rbind(y_test, y_train)
setnames(yj, names(yj), "Activity")
# rowbind the subjects and rename them "Subject" then join everything together and remove big stuff that is not good for my slow computer
subj <-rbind(sub_test, sub_train)
setnames(subj, names(subj), "Subject")
joined <- cbind(subj, yj, xj)
rm(sub_test, x_test, x_train, xj, y_test, y_train, sub_train, subj, yj)

# 2. Extracts only the measurements on the mean and standard 
#    deviation for each measurement.  Anything mentioning mean or std
#    I left it ambiguous because they left it ambiguous. I also kept the Activity and Subject headers because I already put those in 
extract <- joined[ ,grep("Activity|Subject|mean|std", as.character(names(joined)), value = TRUE)]
joined2 <- subset(joined, select = extract)
rm(joined)

# 3. Uses descriptive activity names to name the activities in the data set
# Rename each activity with the knowledge of:
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING
joined2$Activity <- sapply(joined2$Activity, factor, levels = c(1, 2, 3, 4, 5, 6), labels = c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Laying"))

# 4. Appropriately labels the data set with descriptive variable names.
# I did that while extracting the data. These are stored in the var extract.

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
# Use plyr to melt() it into a long form dataset and split apply with 
# ddply() to get the means of everything
library(plyr)
library(tidyr)
tidyLong <-ddply(melt(joined2, id.vars=c("Subject", "Activity")), .(Subject, Activity, variable), summarize, MeanSamples=mean(value))
# Then spread() it into a wide format so that each observation is a row
# and each variable is a column making it tidy!
tidyWide <- spread(tidyLong, variable, MeanSamples)
#write it to a .csv outputting the requested tidy dataset.
write.table(tidyWide, "tidydata.txt", row.name=FALSE)

