run_analysis <- function() {
  ts <- read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE)
  ty <- read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE, col.names=c("label"))
  tx <- read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE)
  txy <- cbind(ty, tx)
  rs <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE)
  ry <- read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE, col.names=c("label"))
  rx <- read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE)
  rxy <- cbind(ry, rx)
  
  # Merges the training and the test sets to create one data set.
  data <- rbind(rxy, txy)
  subject <- rbind(rs, ts)
  
  # Extracts only the measurements on the mean and standard deviation for each measurement. 
  fs <- read.table("UCI HAR Dataset/features.txt", sep=" ", header=FALSE, col.names=c("no","feat"), stringsAsFactors=FALSE)
  ffs <- fs[grepl("std|mean", fs$feat),c("feat")]
  fds <- data[, c(TRUE, ffs)]
  
  # Uses descriptive activity names to name the activities in the data set
  # Appropriately labels the data set with descriptive activity names. 
  alabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names=c("no","activity"), stringsAsFactors=FALSE)
  fds$activity = alabels[fds$label, c("activity")]
  colnames(fds) = c("activity_no", ffs, "activity")
  
  # Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  fds$subject = subject$V1 # add subject into the data set
  library(reshape2)
  meltfds <- melt(fds, id=c("subject","activity"), measure.vars=ffs)
  td2 <- dcast(meltfds, subject+activity ~ variable, mean )
  write.table(td2, file="mean_std_group_by_activity_and_subject.csv", row.names=FALSE, col.names=TRUE, sep=",", quote=FALSE)

}