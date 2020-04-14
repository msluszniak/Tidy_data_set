#function tidy takes as arguments paths to files and return summarized tidy dataset as a result
tidy = function(path_subject_test, path_X_test, path_y_test, path_subject_train, path_X_train,
                path_y_train, path_features, path_activity_label){
  
  #read all files
  subject_test = read.table(path_subject_test)
  X_test = read.table(path_X_test)
  y_test = read.table(path_y_test)
  subject_train = read.table(path_subject_train)
  X_train = read.table(path_X_train)
  y_train = read.table(path_y_train)
  features = read.table(path_features)
  activity = read.table(path_activity_label)

  #merge data from train directory into train and analogically for test, then merge train and test into final_set
  train = cbind(X_train, activities = y_train[,1], subjects = subject_train[,1])
  test = cbind(X_test, activities = y_test[,1], subjects = subject_test[,1])
  final_set = rbind(train, test)
  
  #select the columns with means and standart deviation 
  #value = TRUE means that names of columns will be assigned to col_names
  col_names = grep("[Mm]ean|[Ss]td", features[,2] , value = TRUE)
  
  #dispose of special signs 
  #fixed = TRUE means that special signs in regular expressions will be ignored
  col_names = gsub("(", "", col_names, fixed = TRUE) 
  col_names = gsub(")", "", col_names, fixed = TRUE) 
  col_names = gsub(",", "", col_names, fixed = TRUE) 
  col_names = gsub("-", "", col_names, fixed = TRUE)
  
  #apart from means and std we need activities and subjects
  col_names = c(col_names, "activities", "subjects")
  
  #col_bool is a factor of indexes of mean and std columns 
  col_bool = grepl("[Mm]ean|[Ss]td", features[,2])
  
  #indexes length(col_bool)-1 and length(col_bool) represents activities and subjects, we want them to
  col_bool[length(col_bool)-1] = col_bool[length(col_bool)] = TRUE
  
  #choose columns which are needed
  final_set = final_set[, col_bool]
  
  #assign names of columns
  colnames(final_set) = col_names
  
  #trim white spaces from activity names
  #library("stringr")
  for(i in 1:length(activity[,2])){
    activity[i,2] = str_trim(activity[i,2])
  }
  
  #change from factor to character
  activity[,2] = as.character(activity[,2])
  
  #replace numbers in activities with names
    for(i in 1:length(final_set$activities)){
      if(final_set[i, "activities"] == 1){
        final_set[i, "activities"] = activity[1,2]
      }
      if(final_set[i, "activities"] == 2){
        final_set[i, "activities"] = activity[2,2]
      }
      if(final_set[i, "activities"] == 3){
        final_set[i, "activities"] = activity[3,2]
      }
      if(final_set[i, "activities"] == 4){
        final_set[i, "activities"] = activity[4,2]
      }
      if(final_set[i, "activities"] == 5){
        final_set[i, "activities"] = activity[5,2]
      }
      if(final_set[i, "activities"] == 6){
        final_set[i, "activities"] = activity[6,2]
      }
    }
    #group by subject and activities 
    #library("dplyr")
    result = group_by(final_set, subjects, activities)
    
    #apply a mean function to all columns of grouped data
    summarize_all(result, funs(mean))
    
    #in this case the summary looks like:
    #for each subject{
    #  for each activity{
    #    summarize using mean
    #  }
    #}
    
    #if you want to change into this:
    #for each subject{
    #  summarize using mean
    #}
    #for each activity{
    #  summarize using mean
    #}
    
    #simply you need to group subject first and summarize
    #then group and summarize activities and finally merge 
    #both summaries into resultant summary
}

# sample input 
#set = tidy("/home/user/Desktop/UCI HAR Dataset/test/subject_test.txt",
# "/home/user/Desktop/UCI HAR Dataset/test/X_test.txt",
# "/home/user/Desktop/UCI HAR Dataset/test/y_test.txt",
# "/home/user/Desktop/UCI HAR Dataset/train/subject_train.txt", 
# "/home/user/Desktop/UCI HAR Dataset/train/X_train.txt",
# "/home/user/Desktop/UCI HAR Dataset/train/y_train.txt",
# "/home/user/Desktop/UCI HAR Dataset/features.txt",
# "/home/user/Desktop/UCI HAR Dataset/activity_labels.txt")
