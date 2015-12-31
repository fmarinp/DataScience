#R script for Telstra Kaggle competition
library(data.table)
library(randomForest)
library(stringr)
library(xgboost)
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
#do xgboost

#fault.severity <- train.wide2$fault_severity

train.dfo <- cbind(as.numeric(train.file.df$id),as.numeric(train.file.df$fault_severity),as.numeric(train.file.df$LocationNumber))
train.df <- as.data.frame(train.dfo)
colnames(train.df) <- c('id','fault_severity','Location_Number')
train.df <- merge(train.df,resource.type.wide,by='id')
train.df <- merge(train.df,event.type.wide,by='id')
train.df <- merge(train.df,severity.type.wide,by='id')
train.wide <- merge(train.df,logfeature.type.wide,by='id')
#change colnames (to avoid spaces)
x <- colnames(train.wide)
x<- str_replace_all(x,"\\s+","_")
colnames(train.wide) <- x
#save fault severity
fault.severity <- as.numeric(train.wide$fault_severity)
id.train <- train.wide$id
#eliminate id and fault_severity
train.4xgb <- train.wide[,3:457]
#transform to matrix
trainMatrix <- data.matrix(train.4xgb)


#train.wide <- train.wide[,-zeroVar(train.wide)]
#eliminate zero variance variables
#zv <- nearZeroVar(train.wide,saveMetrics = T)$zeroVar
#train.wide <- train.wide[,zv==FALSE]

#test set
test.dfo <- cbind(as.numeric(test.file.df$id),as.numeric(test.file.df$LocationNumber))
test.df <- as.data.frame(test.dfo)
colnames(test.df) <- c('id','Location_Number')
test.df <- merge(test.df,resource.type.wide,by='id')
test.df <- merge(test.df,event.type.wide,by='id')
test.df <- merge(test.df,severity.type.wide,by='id')
test.wide <- merge(test.df,logfeature.type.wide,by='id')
x <- colnames(test.wide)
x<- str_replace_all(x,"\\s+","_")
colnames(test.wide) <- x
#View(test.wide)
id.test <- test.wide$id
#eliminate id and fault_severity
test.4xgb <- test.wide[,2:456]
#transform to matrix
testMatrix <- data.matrix(test.4xgb)

#now xgboost
#cross-validaton and model building
numberOfClasses <- 3
params <- list('objective'='multi:softprob','eval_metric'='mlogloss','num_class'=numberOfClasses)
cv.nround <- 500
cv.nfold <- 5
bst.cv <- xgb.cv(param=params,data=trainMatrix,label=fault.severity,nfold=cv.nfold,nrounds=cv.nround)

#plot(bst.cv$test.mlogloss.mean)
#choose how many trees
nround <- which(bst.cv$test.mlogloss.mean==min(bst.cv$test.mlogloss.mean))

#train the model
bst <- xgboost(data=trainMatrix,label=fault.severity,param=params,nrounds=nround)

#tests
ypred <- predict(bst,testMatrix)

predMatrix <- data.frame(matrix(ypred,ncol=3,byrow=TRUE))
colnames(predMatrix) <- c('predict_0','predict_1','predict_2')
submit.xgb <- data.frame(id.test,predMatrix)
write.csv(submit.xgb,'submission_fmarin_xgb01.csv',quote=F,row.names=F)