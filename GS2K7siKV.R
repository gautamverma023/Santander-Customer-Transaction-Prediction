rm(list=ls())
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages("pROC")
require('pROC')
library(caret)
lapply(x, require, character.only = TRUE)
rm(x)
getwd()
train=read.csv("train.csv")
test=read.csv("test.csv")
##missing value analysis
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))
## Correlation Plot 
corrgram(train[,3:202], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#variable plot
range<-c(1:9)
grid<-matrix(c(1:9),nrow=3,ncol = 3,byrow = TRUE)
layout(grid)
for(i in range){
  hist(train[,i+2],col = "yellow",border = "blue",xlab =colnames(train[i+2]),main = "")
}
grid<-matrix(c(1:4),nrow=2,ncol = 2,byrow = TRUE)

layout(grid)
variable_mean=c()
variable_min=c()
variable_max=c()
for(i in c(1:200))
{
  variable_mean<-c(mean(train[,i+2]),variable_mean)
  variable_max<-c(max(train[,i+2]),variable_max)
  variable_min<-c(min(train[,i+2]),variable_min)
  
}
hist(variable_mean,col = "yellow",border = "blue",main=)

hist(variable_max,col = "yellow",border = "blue",main=)

hist(variable_min,col = "yellow",border = "blue",main=)

layout(1)
set.seed(1234)
train.index = createDataPartition(train$target, p = .80, list = FALSE)
train_file=train[train.index,]
train_file=train_file[,-1]
val_file=train[-train.index,]
val_file=val_file[,-1]
train_file$target<-as.factor(train_file$target)
val_file$target<-as.factor(val_file$target)
table(train_file$target)
#let's try decision tree
C50_model = C5.0(target ~.,train_file, rules = TRUE)
C50_Predictions = predict(C50_model,val_file[-1], type = "class")
C50_Predictions
Conf_matrix=table(val_file$target,C50_Predictions)
Conf_matrix=as.matrix(Conf_matrix)
TN=Conf_matrix[1]
FN=Conf_matrix[2]
FP=Conf_matrix[3]
TP=Conf_matrix[4]
accuary=(TN+TP)/(TN+TP+FN+FP)
accuary
precision <- TP/(TP+FP)    
precision
recall <- TP/(TP+FN)
recall
#let try
###Random Forest
RF_model = randomForest(target ~ ., train_file, importance = TRUE, ntree =25)
RF_Predictions = predict(RF_model,val_file[-1] )
Conf_matrix = table(val_file[,1], RF_Predictions)
Conf_matrix=as.matrix(Conf_matrix)
TN=Conf_matrix[1]
FN=Conf_matrix[2]
FP=Conf_matrix[3]
TP=Conf_matrix[4]
accuary=(TN+TP)/(TN+TP+FN+FP)
accuary
precision <- TP/(TP+FP)    
precision
recall <- TP/(TP+FN)
recall
##SMOTE
tab<-as.data.frame(table(train_file$target))
newtrain_file <- SMOTE(target ~ ., train_file,prec.over=tab[1,2]-tab[2,2])
table(newtrain_file$target)
RF_model = randomForest(target ~ ., newtrain_file, importance = TRUE, ntree =25)
RF_Predictions = predict(RF_model,val_file[-1] )
Conf_matrix= table(val_file[,1], RF_Predictions)
Conf_matrix=as.matrix(Conf_matrix)
TN=Conf_matrix[1]
FN=Conf_matrix[2]
FP=Conf_matrix[3]
TP=Conf_matrix[4]
accuary=(TN+TP)/(TN+TP+FN+FP)
accuary
precision <- TP/(TP+FP)    
precision
recall <- TP/(TP+FN)
recall
#install.library("pROC")
roc_obj <- roc(as.numeric(val_file[,1]), as.numeric(RF_Predictions))
auc(roc_obj)

#confusionMatrix(ConfMatrix_RF)
##variable imporatnce
gbmImp <- varImp(RF_model, scale = FALSE)
gbmImp<-gbmImp[order(-gbmImp$`0`),]
gbmImp
c<-rownames(gbmImp[1:10,])
barplot(gbmImp[1:10,1],names.arg=c)       
rm(gbmImp)
##optimizing the parameter
Range=c(1,2,4,8,16,32)
train_results<-c()
test_results<-c() 
for (i in Range)
  {
    RF_model = randomForest(target ~ ., newtrain_file, importance = TRUE, ntree =i)
    train_pred = predict(RF_model,newtrain_file[,-1])
    roc_obj <- roc(as.numeric(newtrain_file[,1]), as.numeric(train_pred))
    train_results<-c(train_results,auc(roc_obj))
    train_pred = predict(RF_model,val_file[,-1])
    roc_obj <- roc(as.numeric(as.numeric(val_file[,1])), as.numeric(train_pred))
    test_results<-c(test_results,auc(roc_obj))
  }
train_results_dataFrame=as.data.frame(list(train_results,Range))
test_results_dataFrame=as.data.frame(list(test_results,Range))
cols = c("AU", "value")
colnames(train_results_dataFrame) = cols
colnames(test_results_dataFrame) = cols
p = ggplot() + 
  geom_line(data = train_results_dataFrame, aes(x = value, y = AU), color = "blue") +
  geom_line(data = test_results_dataFrame, aes(x = value, y = AU), color = "red") +
  xlab('no of tree') +
  ylab('AUC Score')

print(p)
Range=c(2:14)
train_results<-c()
test_results<-c() 
for (i in Range)
{
  RF_model = randomForest(target ~ ., newtrain_file, importance = TRUE, ntree =12,mtry=i)
  train_pred = predict(RF_model,newtrain_file[,-1])
  roc_obj <- roc(as.numeric(newtrain_file[,1]), as.numeric(train_pred))
  train_results<-c(train_results,auc(roc_obj))
  train_pred = predict(RF_model,val_file[,-1])
  roc_obj <- roc(as.numeric(as.numeric(val_file[,1])), as.numeric(train_pred))
  test_results<-c(test_results,auc(roc_obj))
}
train_results_dataFrame=as.data.frame(list(train_results,Range))
test_results_dataFrame=as.data.frame(list(test_results,Range))
cols = c("AUC", "value")
colnames(train_results_dataFrame) = cols
colnames(test_results_dataFrame) = cols
q = ggplot() + 
  geom_line(data = train_results_dataFrame, aes(x = value, y = AUC), color = "blue") +
  geom_line(data = test_results_dataFrame, aes(x = value, y = AUC), color = "red") +
  xlab('no of predictor(mtry)') +
  ylab('AUC Score')

print(q)
Range=c(1,3,6,10,15,20,30)
train_results<-c()
test_results<-c() 
for (i in Range)
{
  RF_model = randomForest(target ~ ., newtrain_file, importance = TRUE, ntree =15,mtry=10,nodesize=i)
  train_pred = predict(RF_model,newtrain_file[,-1])
  roc_obj <- roc(as.numeric(newtrain_file[,1]), as.numeric(train_pred))
  train_results<-c(train_results,auc(roc_obj))
  train_pred = predict(RF_model,val_file[,-1])
  roc_obj <- roc(as.numeric(as.numeric(val_file[,1])), as.numeric(train_pred))
  test_results<-c(test_results,auc(roc_obj))
}
train_results_dataFrame=as.data.frame(list(train_results,Range))
test_results_dataFrame=as.data.frame(list(test_results,Range))
cols = c("AUC", "value")
colnames(train_results_dataFrame) = cols
colnames(test_results_dataFrame) = cols
r = ggplot() + 
  geom_line(data = train_results_dataFrame, aes(x = value, y = AUC), color = "blue") +
  geom_line(data = test_results_dataFrame, aes(x = value, y = AUC), color = "red") +
  xlab('no of predictor(mtry)') +
  ylab('AUC Score')

print(r)
#hyper parameter tuning
RF_model = randomForest(target ~ ., newtrain_file, importance = TRUE, ntree =32,mtry=7,nodesize=0.28)
RF_Predictions = predict(RF_model,val_file[-1] )
Conf_matrix = table(val_file[,1], RF_Predictions)
install.library("pROC")
Conf_matrix=as.matrix(Conf_matrix)
TN=Conf_matrix[1]
FN=Conf_matrix[2]
FP=Conf_matrix[3]
TP=Conf_matrix[4]
accuary=(TN+TP)/(TN+TP+FN+FP)
accuary
precision <- TP/(TP+FP)    
precision
recall <- TP/(TP+FN)
recall
roc_obj <- roc(as.numeric(val_file[,1]), as.numeric(RF_Predictions))
auc(roc_obj)

#confusionMatrix(ConfMatrix_RF)
##variable importance
gbmImp <- varImp(RF_model, scale = FALSE)
gbmImp<-gbmImp[order(-gbmImp$`0`),]
gbmImp
c<-rownames(gbmImp[1:10,])
barplot(gbmImp[1:10,1],names.arg=c)       
rm(gbmImp)

#k fold cross validition in R
train1<-train[sample(nrow(train)),]
train1$target<-as.factor(train1$target)
#Create 10 equally size folds
folds <- cut(seq(1,nrow(train1)),breaks=5,labels=FALSE)
k_acc<-c()

#Perform 5 fold cross validation
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  valData <- train1[testIndexes, -1]
  trainData <- train1[-testIndexes,-1]
  tab<-as.data.frame(table(trainData$target))
  newtrain_file1 <- SMOTE(target ~ .,  trainData ,prec.over=tab[1,2]-tab[2,2])
  RF_model = randomForest(target ~ ., newtrain_file1, importance = TRUE, ntree =32,mtry=7,nodesize=0.28)
  train_pred = predict(RF_model,valData[,-1])
  roc_obj <- roc(as.numeric(as.numeric(valData[,1])), as.numeric(train_pred))
  auc(roc_obj)
  K_acc<-c(auc(roc_obj),k_acc)
}
sum(k_acc)/5
#naive Bayes
library(e1071)
scale_train=as.data.frame(scale(train[,3:202]))
scale_train$target=as.factor(train$target)
scale_train_file=scale_train[train.index,]
scale_val_file=scale_train[-train.index,]
#Develop model
NB_model = naiveBayes(target ~ ., data = scale_train_file)
NB_Predictions = predict(NB_model, scale_val_file[,-201], type = 'class')
Conf_matrix = table(observed = scale_val_file[,'target'], predicted = NB_Predictions)
Conf_matrix=as.matrix(Conf_matrix)
TN=Conf_matrix[1]
FN=Conf_matrix[2]
FP=Conf_matrix[3]
TP=Conf_matrix[4]
accuary=(TN+TP)/(TN+TP+FN+FP)
accuary
precision <- TP/(TP+FP)    
precision
recall <- TP/(TP+FN)
recall
roc(as.numeric(scale_val_file[,201]), as.numeric(NB_Predictions))
install.packages("mmpf")
library(mmpf)
imp<-c()
for(i in c(1:200))
{
  imp=c(permutationImportance(scale_val_file,colnames(scale_val_file[i]), 'target', NB_model),imp)
  
}
imp=as.dataFrame(list(imp,colnames(scale_val_file)))
colnames(imp)=c(1:2)
imp=imp[order(imp$'1', decreasing=TRUE), ]
barplot(imp[1:10,1],names.arg=imp[1:10,2])
#PIMP(scale_val_file[,201],scale_val_file[,-201],NB_model)
#Boosting technique light GBM
#creating lgb dataset
x<-c('pscl', 'ROCR', 'lightgbm', 'methods', 'Matrix')
install.packages(x)
lapply(x, require, character.only = TRUE)
install.packages('LightGBM',version='2.2.4')
trainm = sparse.model.matrix(target ~., data = train_file)
train_label = train_file[,1]

valm = sparse.model.matrix(target~., data= val_file)
val_label = val_file[,"target"]

train_matrix = lgb.Dataset(data = as.matrix(trainm), label = train_label)
val_matrix = lgb.Dataset(data = as.matrix(valm), label = val_label)


#install.packages(file.path("C:/Users/HP/Documents/R/win-library/3.5", "LightGBM", "R-package"), repos = NULL, type = "source")
param = list(
  bagging_freq= 5,
  bagging_fraction=0.4,
  boost_from_average=false,
  boost= 'gbdt',
  feature_fraction=0.05,
  learning_rate= 0.01,
  max_depth= -1,  
  metric='auc',
  min_data_in_leaf= 80,
  min_sum_hessian_in_leaf= 10.0,
  num_leaves= 13,
  num_threads= 8,
  tree_learner= 'serial',
  objective= 'binary', 
  verbosity= 1
)
valid = list(test = val_matrix)

bst = lightgbm(params = params, train_matrix, valid, nrounds=1000000,
               early_stopping_rounds = 3000,
               eval_freq=1000,
               seed=44000)
p = predict(bst, valm)
predicted_lgm = ifelse(p > 0.5,1,0)
confusionMatrix(factor(predicted_lgm), factor(val_file$target))

#prediction

test$target1<-predict(bst,test[,-1])
scale_test=as.data.frame(scale(train[,2:201]))
test$target2<-predict(NB_model,scale_test,type='class')