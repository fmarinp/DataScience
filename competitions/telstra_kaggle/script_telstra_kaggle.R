#R script for Telstra Kaggle competition
library(data.table)
library(randomForest)
library(stringr)
#directory
setwd('/Users/fmarinperucci/datascience/competitions/telstra_kaggle')

#open training file
train.file.df <- read.csv('train.csv')

#number of locations
n.locations <-  length(unique(train.file.df$location))

#extract number only from splits
get_number_fun  <- function(x) as.numeric(strsplit(x," ")[[1]][2])

#extract for location
train.file.df$LocationNumber <- lapply(as.character(train.file.df$location,strsplit),get_number_fun)

#open test file too 
test.file.df <- read.csv('test.csv')
test.file.df$LocationNumber <- lapply(as.character(test.file.df$location,strsplit),get_number_fun)

#now open other files: eventfile
event.type.df <- read.csv('event_type.csv')
#wide format: each event is a column
event.type.wide <- dcast.data.table(as.data.table(event.type.df), id ~  event_type, fun = length,value.var='event_type')
event.type.wide <- as.data.frame(event.type.wide)
#event.type.df$EventTypeNumber <- lapply(as.character(event.type.df$event_type,strsplit),get_number_fun)
#etype <- sapply(event.type.wide, factor)
#event.type.wide <- as.data.frame(etype)

#now open other files: resource type
resource.type.df <- read.csv('resource_type.csv')
#resource.type.df$ResourceTypeNumber <- lapply(as.character(resource.type.df$resource_type,strsplit),get_number_fun)
resource.type.wide <- dcast.data.table(as.data.table(resource.type.df), id ~  resource_type, fun = length,value.var='resource_type')
resource.type.wide <- as.data.frame(resource.type.wide)
#rtype <- sapply(resource.type.wide, factor)
#resource.type.wide <- as.data.frame(rtype)


#now open other files: severityfile
severity.type.df <- read.csv('severity_type.csv')
#severity.type.df$SeverityTypeNumber <- lapply(as.character(severity.type.df$severity_type,strsplit),get_number_fun)
severity.type.wide <- dcast.data.table(as.data.table(severity.type.df), id ~  severity_type, fun = length,value.var='severity_type')
severity.type.wide <- as.data.frame(severity.type.wide)
#stype <- sapply(severity.type.wide, factor)
#severity.type.wide <- as.data.frame(stype)



#now open other files: log_feature file
logfeature.df <- read.csv('log_feature.csv')
#do wide type - log feature type + volume
#logfeature.df$FeatureNumber <- lapply(as.character(logfeature.df$log_feature,strsplit),get_number_fun)
logfeature.type.wide <- dcast.data.table(as.data.table(logfeature.df), id ~  log_feature, fun = sum,value.var='volume')
logfeature.type.wide <- as.data.frame(logfeature.type.wide)

#exploratory data analysis
#there are several logs for same id - or different events 
# train.sev <- merge(train.file.df,severity.type.df,by='id')
# View(train.sev)
# plot(train.sev$fault_severity,train.sev$SeverityTypeNumber)
# conclusions: severity type 1 & 2 correlate with fault severity 2

#need to create tables where event and resoruces appear as columns, check later
#first approach: brute force - each event type, resource etc are to appear as columns - will
#get close 500 columns!
# how do i do this in R? 

train.df <- cbind(train.file.df$id,train.file.df$LocationNumber,train.file.df$fault_severity)
colnames(train.df) <- c('id','Location_Number','fault_severity')
train.df <- as.data.frame(train.df)
train.df$id <- as.numeric(train.df$id)
train.df$Location_Number <- as.character(train.df$Location_Number)
train.df$fault_severity <- as.character(train.df$fault_severity)
train.big <- merge(train.df,resource.type.wide,by='id')
train.big <- merge(train.big,event.type.wide,by='id')
train.big <- merge(train.big,severity.type.wide,by='id')
train.wide <- merge(train.big,logfeature.type.wide,by='id')
#change colnames
x <- colnames(train.wide)
x<- str_replace_all(x,"\\s+","_")
colnames(train.wide) <- x
train.wide <- train.wide[,-1]

train.wide <- train.wide[,-zeroVar(train.wide)]

#eliminate zero variance variables
zv <- nearZeroVar(train.wide,saveMetrics = T)$zeroVar
train.wide <- train.wide[,zv==FALSE]

#test set
test.df <- cbind(test.file.df$id,test.file.df$LocationNumber)
colnames(test.df) <- c('id','Location_Number')
test.df <- as.data.frame(test.df)
test.df$id <- as.character(test.df$id)
test.df$Location_Number <- as.character(test.df$Location_Number)
#test.df$fault_severity <- as.integer(test.df$fault_severity)
test.big <- merge(test.df,resource.type.wide,by='id')
test.big <- merge(test.big,event.type.wide,by='id')
test.big <- merge(test.big,severity.type.wide,by='id')
test.wide <- merge(test.big,logfeature.type.wide,by='id')
x <- colnames(test.wide)
x<- str_replace_all(x,"\\s+","_")
colnames(test.wide) <- x

View(test.wide)

#OK i have 457 variables - but only ~7,000 observations - do PCA? 
#or any gradient boosting stuff? 

require(caret)
rf_modc<-train(as.factor(fault_severity) ~ .,data=train.wide,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE,verbose=TRUE)

rf_mod <- randomForest(as.factor(fault_severity) ~ .,data=train.wide,method="rf",do.trace=T,ntree=500)


#rf_mod <- randomForest(fault_severity ~ Location_Number + resource_type_1 + event_type_1,data=train.wide,method="rf")

gbm_mod <- train(as.factor(fault_severity) ~ ., data=train.wide,method="gbm",verbose=T)


features <- paste("feature",2:15, sep="")
lm(output ~ myData[,features], data=myData)

#for xgboost
fault.severity <- train.wide2$fault_severity
head(fault.severity)
train.wide2.xgb <- train.wide2[,-2]


