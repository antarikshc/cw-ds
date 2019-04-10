library(MASS)
data(biopsy)
head(biopsy)

str(biopsy)

biopsy$ID=NULL
names(biopsy)=c("thick","usize","ushape","adhsn","celsiz","nucl","chrom","nuclus","mit","class")
colSums(is.na(biopsy))

biopsy1=na.omit(biopsy)
set.seed(123)
ind=sample(2,nrow(biopsy1),replace = TRUE,prob = c(0.7,0.3))
train=biopsy1[ind==1,]
test=biopsy1[ind==2,]
str(test)

table(train$class)

table(test$class)

fullfit=glm(class~.,family=binomial,data=train)
summary(fullfit)

# 

exp(coef(fullfit))

train$prob=predict(fullfit,type="response")
train$prob[1:5]

train$class[1:5]

train$predict=rep("benign",474)
train$predict[train$prob>0.5]="malignant"
table(train$predict,train$class)

mean(train$predict==train$class)

test$prob=predict(fullfit,newdata=test,type="response")
test$predict=rep("benign",209)
test$predict[test$prob>0.5]="malignant"
table(test$predict, test$class)

mean(test$predict==test$class)

library(ROCR)
ROCRpred<-prediction(test$prob,test$class)
ROCRpref<-performance(ROCRpred,'tpr','fpr')
plot(ROCRpref,colorize=TRUE)

auc=performance(ROCRpred,'auc')
auc@y.values

reducefit=glm(class~thick+nucl,family=binomial,data=train)
summary(reducefit)

train$prob=predict(reducefit,type="response")
train$predict=rep("benign",474)
train$predict[train$prob>0.5]="malignant"
cf=table(train$predict,train$class)
mean(train$predict==train$class)

library(caret)
sensitivity(cf)

specificity(cf)

test$prob=predict(reducefit,newdata=test,type="response")
test$predict=rep("benign",209)
test$predict[test$prob>0.5]="malignant"
table(test$predict,test$class)

mean(test$predict==test$class)