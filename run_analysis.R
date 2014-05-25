## This script get and clean the UCI_HAR Dataset for the data scientist application
#  The tidy data includes all variables from original dataset, because 
#  this variables should be used in many applications e.g(forest randam, neural networksm, ...)
#
#  Project: Getting and Cleanning Data, creating a tidy dataset
#  Author: Leard Fernandes
tidy_data<-function(){
	#Verify for the folder, if not exists, create it, otherwhise, continues
	if(!file.exists("data")){
	  dir.create("./data")
	}

	##Dowload and unpack the DataSet

	#Set URL for the file
	fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	#download file
	download.file(fileUrl, destfile="./data/UCI_HAR_Dataset.zip")

	#unpack zip file
	unzip("./data/UCI_HAR_Dataset.zip")

	#End of Download and Pack

	##Setting the column names

	#data set for colunm names of X train and test
	labels<-read.table("data/UCI HAR Dataset/features.txt") #load the dataset
	names(labels)<-c("id", "name") #Set column name

	#formatting the columns names
	labels$name<-tolower(labels$name) #set names to lower case
	labels$name<-sub("(\\(\\))","", labels$name) #removing the parenthesis ()
	labels$name<-gsub("(\\()","-", labels$name) #substituting the "(" for "-"
	labels$name<-gsub("\\)","", labels$name) #substituting the ")" for ""
	labels$name<-gsub(",","-", labels$name) #substituting the "," for "-"

	##setting new names for columns
	#the columns from index 303 to 344 (Group 1) are repetead 3 times (Subgroups 1, 2 and 3)
	#the columns from index 382 to 423 (Group 2) are repetead 3 times (Subgroups 1, 2 and 3)
	#the columns from index 461 to 502 (Group 3) are repetead 3 times (Subgroups 1, 2 and 3)

	#config
	index_group=c(303, 382, 461) #index for the group repetead
	len=42 #legth of group of names
	repetead=3 #number of repetitions
	for(i in index_group){  
	  j=0
	  while(j < repetead){ #Three main groups
		init=(i+(len/repetead)*(j)) #index for beggining of subgroups
		end=(i+(len/repetead)*(j+1)-1) #index for the end of subgroups
		labels$name[init:end]<-paste0(labels$name[init:end], paste0("-b", j+1))
		#print(paste0(labels$name[init:end], paste0("-b", j+1)))    
		j=j+1
	  }  
	}

	##End of Columns Names Config

	##Load the activities names for the descriptive variable activity
	#activities Names
	activities<-read.table("data/UCI HAR Dataset/activity_labels.txt") #load the dataset
	names(activities)<-c("id", "activity") #Set column name

	##Read the DataSets

	#read data Train 
	subject_train<-read.table("data/UCI HAR Dataset/train/subject_train.txt")  #load the subject dataset
	y_train<-read.table("data/UCI HAR Dataset/train/y_train.txt")  #load the y dataset
	X_train<-read.table("data/UCI HAR Dataset/train/X_train.txt")  #load the X dataset

	#read data Test
	subject_test<-read.table("data/UCI HAR Dataset/test/subject_test.txt")  #load the subject dataset
	y_test<-read.table("data/UCI HAR Dataset/test/y_test.txt")  #load the y  dataset
	X_test<-read.table("data/UCI HAR Dataset/test/X_test.txt")  #load the X dataset


	##Configure the names of columns
	#set the column names of train datasets
	names(subject_train)<-c("subject")
	names(y_train)<-c("activity")
	names(X_train)<-labels$name

	#set the column names of test datasets
	names(subject_test)<-c("subject")
	names(y_test)<-c("activity")
	names(X_test)<-labels$name

	##Combine the Datasets X, y and subject

	#Combine the Train DataSet
	syx_train<-cbind(subject_train, y_train, X_train)

	#Remove the unused data from workspace
	rm(subject_train,y_train,X_train)

	#Combine the Test DataSet
	syx_test<-cbind(subject_test, y_test, X_test)

	#Remove the unused data from workspace
	rm(subject_test,y_test,X_test)

	#Merge the Train and Test Datasets
	syx_merged<-merge(syx_train, syx_test, all=T)

	#Remove the unused data from workspace
	rm(syx_train,syx_test)

	#End of Combining and Merge Datasets

	##Set the descriptive variables in the DataSet

	#Seetting subject as Factor
	syx_merged$subject<-factor(syx_merged$subject)

	#Seetting activity as Factor
	syx_merged$activity<-cut(syx_merged$activity, 6, activities$activity)

	#Calculate the mean of dataset by subject and activity
	syx_mean<-aggregate(syx_merged[-c(1,2)], by = syx_merged[c(1,2)], FUN=mean)
	#Calculate the standard deviation of dataset by subject and activity
	syx_sd<-aggregate(syx_merged[-c(1,2)], by = syx_merged[c(1,2)], FUN=sd)

	#Remove syx_merged
	rm(syx_merged)
	#Setting new column names for Mean
	labels$nameMean<-paste0(labels$name, "-mean")
	names(syx_mean)[-c(1,2)]<-labels$nameMean

	#Setting new column names Standard Deviation
	labels$nameSd<-paste0(labels$name, "-sd")
	names(syx_sd)[-c(1,2)]<-labels$nameSd

	#Merge Mean and SD Datas
	syx_tidy<-merge(syx_mean, syx_sd, all=T)

	#remove sys_mean and syx_sd
	rm(syx_mean, syx_sd)
	#Reorder by subject and activity
	syx_tidy<-syx_tidy[order(syx_tidy$subject, syx_tidy$activity),]

	#Save on file
	write.csv(syx_tidy, "data/UCI_HAR_tydy_Dataset.txt")
	syx_tidy
}

