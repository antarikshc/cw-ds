# Simple
house = read.csv(file.choose(),sep=",",header=T) # Open index.csv
reg1 = lm(death_rate~hosp_avail,data = house)
summary(reg1)

plot(house$hosp_avail,house$death_rate)
abline(reg1, col="red")
plot(reg1)

# Multiple
house=read.csv(file.choose(),sep=",",header=T)
pairs(~death_rate+doctor_avail+annual_income+density_per_capita, data=house)
housemodel=lm(density_per_capita~death_rate+doctor_avail+hosp_avail+annual_income,data=house)
summary(housemodel)

plot(housemodel)

index=read.csv(file.choose(),sep=",",header = T) # regression.csv
names(index)
pairs(~index+written+language+tech+gk,data=index)
model1=lm(index~.,data=index)
summary(model1)

plot(model1)

index$pred=fitted(model1)
head(index)

index$res=residuals(model1)
head(index)

library(car)
vif(model1)

plot(index$pred,index$res,col="red")

library(car)
ncvTest(model1,~written+language+tech+gk)

shapiro.test(index$res)

library(car)
durbinWatsonTest(model1)

influencePlot(model1)

index=index[-33,]
library("caret")
library("lattice")
library("ggplot2")
index = read.csv(file.choose(),sep = ",",header = T)
summary(index)

data=createDataPartition(index$empid,p=0.8,list=F)
head(data)

dim(data)

traidata = index[data,]
testdata = index[-data,]
dim(traidata)

dim(testdata)

names(traidata)

modeltrain<-lm(index~written+language+tech+gk,data=traidata)
modeltrain$res<-residuals(modeltrain)
RMSEtrain<-sqrt(mean(modeltrain$res**2))
RMSEtrain

testdata$pred=predict(modeltrain,testdata)
testdata$res=testdata$index-testdata$pred
RMSEtest<-sqrt(mean(testdata$res**2))
RMSEtest

kfolds <- trainControl(method = "cv",number = 4)
modelkfold <- train(index~written+language+tech+gk,data = index,method="lm",trControl=kfolds)
modelkfold

kfoldsrp <- trainControl(method="repeatedcv",number=4,repeats=5)
modelkfoldsrp <- train(index~written+language+tech+gk,data = index ,method="lm",trControl=kfoldsrp)
modelkfoldsrp

kfoldsloocv<-trainControl(method = "LOOCV")
kfoldsloocvmodel<-train(index~written+language+tech+gk,data = index,method="lm",trControl=kfoldsloocv)
kfoldsloocvmodel

null <- lm(index~1,data=index)
full <- lm(index~.,data = index)
names(index)

step(null,scope = list(lower=null,upper=full),direction = "forward")

step(full,scope=list(lower=null,upper=full),direction = "backward")
