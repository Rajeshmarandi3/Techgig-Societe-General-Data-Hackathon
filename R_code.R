library(randomForest)
library(caret)
library(corrplot)

#Loading all datasets
Deodorant_B <- read.csv("~/R_PROG/DataSet/Deodorant B.csv")
Deodorant_F <- read.csv("~/R_PROG/DataSet/Deodorant F.csv")
Deodorant_G <- read.csv("~/R_PROG/DataSet/Deodorant G.csv")
Deodorant_H <- read.csv("~/R_PROG/DataSet/Deodorant H.csv")
Deodorant_J <- read.csv("~/R_PROG/DataSet/Deodorant J.csv")

test_data <- read.csv("~/R_PROG/DataSet/test_data.csv")#test_data contains testing data for all doedorant

#################################################################################################################
#Working on Deodorant_B data
#making an instance
ddb=Deodorant_B

#Subsetting Deodorant_B from test_data
test_b=subset(test_data,Product=="Deodorant B")

#Extracting common names between train and test data 
#Because for final prediction both should have same number of columns
nt=colnames(Deodorant_B) #colnames of train data
ns=colnames(test_b) #colnames of test data
rmv=intersect(ns,nt) #Intersecton set of both colnames
ddb=ddb[,rmv] #Making instance of train data with specific colnames
ddb$Instant.Liking=as.factor(Deodorant_b$Instant.Liking) #Attaching target variable to train data

#Preprocessing
#Checking correlation
corrplot(cor(Deodorant_B[,5:39]))

#Model fitting
train_control1 <- trainControl(method="repeatedcv", number=10, repeats=2)
model1=train(Instant.Liking~.,data=ddb[,-c(1,2,3)],trControl=train_control1, method="rf")

rb=predict(model1,test_b[,-c(1,2,3)])# predicton result of B type
#################################################################################################################
#Working on Deodorant_F data
#making an instance
ddf=Deodorant_F

#Subsetting Deodorant_F from test_data
test_f=subset(test_data,Product=="Deodorant F")

#Extracting common names between train and test data 
#Because for final prediction both should have same number of columns
nt=colnames(Deodorant_F) #colnames of train data
ns=colnames(test_f) #colnames of test data
rmv=intersect(ns,nt) #Intersecton set of both colnames
ddf=ddf[,rmv] #Making instance of train data with specific colnames
ddf$Instant.Liking=as.factor(Deodorant_F$Instant.Liking) #Attaching target variable to train data

#Preprocessing
#Checking correlation
corrplot(cor(Deodorant_F[,5:39]))

#Model fitting
train_control1 <- trainControl(method="repeatedcv", number=10, repeats=2)
model2=train(Instant.Liking~.,data=ddf[,-c(1,2,3)],trControl=train_control1, method="rf")

rf=predict(model2,test_f[,-c(1,2,3)])# predicton result of F type
#################################################################################################################
#Working on Deodorant_G data
#making an instance
ddg=Deodorant_G

#Subsetting Deodorant_G from test_data
test_g=subset(test_data,Product=="Deodorant G")

#Extracting common names between train and test data 
#Because for final prediction both should have same number of columns
nt=colnames(Deodorant_G) #colnames of train data
ns=colnames(test_g) #colnames of test data
rmv=intersect(ns,nt) #Intersecton set of both colnames
ddg=ddg[,rmv] #Making instance of train data with specific colnames
ddg$Instant.Liking=as.factor(Deodorant_G$Instant.Liking) #Attaching target variable to train data

#Preprocessing
#Checking correlation
corrplot(cor(Deodorant_G[,5:39]))

#Model fitting
train_control1 <- trainControl(method="repeatedcv", number=10, repeats=2)
model3=train(Instant.Liking~.,data=ddg[,-c(1,2,3)],trControl=train_control1, method="rf")

rg=predict(model3,test_g[,-c(1,2,3)])# predicton result of G type
#################################################################################################################
#Working on Deodorant_H data
#making an instance
ddh=Deodorant_H

#Subsetting Deodorant_H from test_data
test_h=subset(test_data,Product=="Deodorant H")

#Extracting common names between train and test data 
#Because for final prediction both should have same number of columns
nt=colnames(Deodorant_H) #colnames of train data
ns=colnames(test_h) #colnames of test data
rmv=intersect(ns,nt) #Intersecton set of both colnames
ddh=ddh[,rmv] #Making instance of train data with specific colnames
ddh$Instant.Liking=as.factor(Deodorant_H$Instant.Liking) #Attaching target variable to train data

#Preprocessing
#Checking correlation
corrplot(cor(Deodorant_H[,5:39]))

#Model fitting
train_control1 <- trainControl(method="repeatedcv", number=10, repeats=2)
model4=train(Instant.Liking~.,data=ddh[,-c(1,2,3)],trControl=train_control1, method="rf")

rh=predict(model5,test_h[,-c(1,2,3)])# predicton result of H type
#################################################################################################################
#Working on Deodorant_J data
#making an instance
ddj=Deodorant_J

#Subsetting Deodorant_J from test_data
test_j=subset(test_data,Product=="Deodorant J")

#Extracting common names between train and test data 
#Because for final prediction both should have same number of columns
nt=colnames(Deodorant_J) #colnames of train data
ns=colnames(test_j) #colnames of test data
rmv=intersect(ns,nt) #Intersecton set of both colnames
ddj=ddj[,rmv] #Making instance of train data with specific colnames
ddj$Instant.Liking=as.factor(Deodorant_J$Instant.Liking) #Attaching target variable to train data

#Preprocessing
#Checking correlation
corrplot(cor(Deodorant_J[,5:39]))

#Model fitting
train_control1 <- trainControl(method="repeatedcv", number=10, repeats=2)
model5=train(Instant.Liking~.,data=ddj[,-c(1,2,3)],trControl=train_control1, method="rf")

rj=predict(model5,test_j[,-c(1,2,3)])# predicton result of J type
###################################################################
#Combining all the prediction result in one dataframe for submission
rbd=data.frame(result=rb)
rfd=data.frame(result=rf)
rgd=data.frame(result=rg)
rhd=data.frame(result=rh)
rjd=data.frame(result=rj)
all_result=rbind(rbd,rfd)
all_result=rbind(all_result,rgd)
all_result=rbind(all_result,rhd)
all_result=rbind(all_result,rjd)

sample_submission <- read.csv("~/R_PROG/DataSet/sample_submission.csv")
sample_submission$Instant.Liking=all_result$result
