## read labels -------------------------------------------------------------------------
feature_label = read.table("C:/Users/User/Documents/UCI HAR Dataset/features.txt")
act_label = read.table("C:/Users/User/Documents/UCI HAR Dataset/activity_labels.txt")

## read train data
dat_train = read.table("C:/Users/User/Documents/UCI HAR Dataset/train/X_train.txt")
train_act_label = read.table("C:/Users/User/Documents/UCI HAR Dataset/train/y_train.txt")
train_sub = read.table("C:/Users/User/Documents/UCI HAR Dataset/train/subject_train.txt")

## read test data
dat_test = read.table("C:/Users/User/Documents/UCI HAR Dataset/test/X_test.txt")
test_act_label = read.table("C:/Users/User/Documents/UCI HAR Dataset/test/y_test.txt")
test_sub = read.table("C:/Users/User/Documents/UCI HAR Dataset/test/subject_test.txt")

## combine train & test data ----------------------------------------------------------
dat_all = rbind(dat_train,dat_test)
act_label_all = rbind(train_act_label,test_act_label)
sub_all = rbind(train_sub,test_sub)

## extract mean & standard deviation ---------------------------------------------------
# find all measurements of mean & std
tmp = which(grepl('mean()',feature_label[,2]) | grepl('std()',feature_label[,2]))
# extract data
feature_label = feature_label[tmp,]
feature_label = as.character(feature_label[,2])
dat_all = dat_all[,tmp]

## set activity names ------------------------------------------------------------------
act_label_all = merge(act_label_all,act_label,by.x = 'V1',by.y = 'V1',all = T,sort=F)

## set variable names
colnames(dat_all) = feature_label

## create tidy data set
num_act = nrow(act_label)
num_sub = max(sub_all)
num_fea = ncol(dat_all)
new_dat = data.frame(matrix(NA,nrow = 1,ncol = (num_fea+2)))
colnames(new_dat) = c('Subject ID','Activity',feature_label)  

for(subID in 1:num_sub){
  for(actID in 1:num_act){
    avg_var = colMeans(dat_all[which((sub_all$V1 == subID) & (act_label_all$V1 == actID)),])
    tmp = c(subID,as.character(act_label[actID,2]),avg_var)
    if ((subID == 1) & (actID == 1)) {
      new_dat[1,] = tmp
    } else {
      new_dat = rbind(new_dat,tmp)
    }
    
  }
}


write.table(new_dat,file = 'new_data.txt',row.names = F)
