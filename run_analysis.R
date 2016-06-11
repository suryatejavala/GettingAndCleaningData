library(data.table)

path<-getwd()
path_dataset <- file.path(path, "UCI HAR Dataset")
if (!file.exists(path_dataset)){
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    f <- 'Dataset.zip'
    
    if (!file.exists(path)) {dir.create(path)}
    download.file(url, file.path(path, f),method='curl')
    unzip("Dataset.zip")
    
}


df_train_set<-read.table(file.path(path_dataset,"train","X_train.txt"))
dt_train_set<-data.table(df_train_set)

df_test_set<-read.table(file.path(path_dataset,"test","X_test.txt"))
dt_test_set<-data.table(df_test_set)

dt_set <- rbind(dt_train_set, dt_test_set)

df_subject_train<-read.table(file.path(path_dataset,"train","subject_train.txt"))
dt_subject_train<-data.table(df_subject_train)

df_subject_test<-read.table(file.path(path_dataset,"test","subject_test.txt"))
dt_subject_test<-data.table(df_subject_test)

dt_subject <- rbind(dt_subject_train, dt_subject_test)
setnames(dt_subject, names(dt_subject), c("subject_id"))

df_label_train <-read.table(file.path(path_dataset,"train","y_train.txt"))
dt_label_train <-data.table(df_label_train)

df_label_test <-read.table(file.path(path_dataset,"test","y_test.txt"))
dt_label_test <-data.table(df_label_test)

dt_label <- rbind(dt_label_train, dt_label_test)

setnames(dt_label, names(dt_label), c("activity_id"))

dt_set<-cbind(dt_set,dt_subject, dt_label)

# 2.
df_features <- read.table(file.path(path_dataset,"features.txt"))
dt_features<-data.table(df_features)

setnames(dt_features, names(dt_features), c("feature_id", "feature_name"))

my_logical_features <- grep ("mean\\(\\)|std\\(\\)", dt_features$feature_name)

dt_set_mean_std <- dt_set[,my_logical_features,with=FALSE]
dt_set_mean_std$subject_id <- dt_set$subject_id
dt_set_mean_std$activity_id <- dt_set$activity_id

# 3. 
df_activity_labels <- read.table(file.path(path_dataset,"activity_labels.txt"))
dt_activity_labels <- data.table(df_activity_labels)
setnames(dt_activity_labels, names(dt_activity_labels), c("activity_id", "activity_name"))
dt_set_mean_std <- merge(dt_set_mean_std, dt_activity_labels, by="activity_id", all.x=TRUE)

order_column<-names(dt_set_mean_std)[c(2:dim(dt_set_mean_std)[2],1)]
setcolorder(dt_set_mean_std, order_column)

# 4. Appropriately labels the data set with descriptive variable names. 
dt_features_logical <- dt_features[my_logical_features,]
setnames(dt_set_mean_std, names(dt_set_mean_std)[1:dim(dt_features_logical)[1]], as.character(dt_features_logical$feature_name))

# 5.
column_selected <- 1:((dim(dt_set_mean_std)[2])-3)
dt_tidy<-aggregate(dt_set_mean_std[,column_selected,with=FALSE],
                   (list(dt_set_mean_std$activity_name, dt_set_mean_std$subject_id)),mean)

setnames(dt_tidy, names(dt_tidy)[1:2], c("Activity_Name", "Subject_Id"))

f <- file.path(path, "tidy_data.txt")
write.table(dt_tidy, f, quote = FALSE, sep = "\t", row.names = FALSE)
