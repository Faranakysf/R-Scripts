#___________________________________________________________________________________________________________________
#________________________________________________Predicting Employees’ Attrition __________________________________________________
#___________________________________________________________________________________________________________________

#Instal Packages and Libraries 
install.packages("kknn", 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/kknn_1.3.1.tgz')
yinstall.packages("igraph", 'https://cran.rstudio.com/bin/macosx/el-capitan/contrib/3.6/igraph_1.2.5.tgz')
install.packages(xgboost, 'https://cran.rstudio.com/src/contrib/xgboost_1.0.0.2.tar.gz')
install.packages("kernlab", "DMwR", "caret", "rpart.plot")
install.packages("ggplot")
library(pROC)
library(ggplot2)
library(caret)
library(DMwR)
library(mlbench)  
library(caTools)
library(xgboost)
library(plyr)
library(DMwR)
library(data.table)
library(DT)
library(gridExtra)
library(Metrics)
library(randomForest)
library(e1071)
library(rpart.plot) 
library(tidyverse)

#___________________________________________________________________________________________________________________
#________________________________________________Data Preparation___________________________________________________
#___________________________________________________________________________________________________________________
sdf = load("/Users/faranak/Desktop/exampleModels.rdata")
df=read.csv('~/Desktop/Rfiles/IBMdata.csv')
#  attach the dataset to access variables localy in R 
attach(df)
#explore the data structure
str(sdf)
sdf
#_____________________Feature Engineering___________________________
#convert int variables to ordered factors 
names <- c('RelationshipSatisfaction', 'PerformanceRating', 'WorkLifeBalance', 
           'JobInvolvement', 'JobSatisfaction', 'JobLevel', 'StockOptionLevel')
df[,names] <- lapply(df[,names] , factor, ordered = TRUE)
str(df)
#_____________________
#convert int variables to ordered factors 
fact_notordered <- c('Education', 'EnvironmentSatisfaction')
df[,fact_notordered] <- lapply(df[,fact_notordered] , factor, ordered = FALSE)
str(df)

#_____________________

#calculate the average tenure for past job eployees had 
df$AvgTnrPerPastJobs <- ifelse(df$NumCompaniesWorked!=0, df$TotalWorkingYears-df$YearsAtCompany/df$NumCompaniesWorked,0)
#_____________________
#convert age to factor 
df$AgeGroup <- as.factor(
  ifelse(df$Age<=30,"Young", ifelse(
    df$Age<=43,"Middle-Age","Adult"
  ))
)
df$AgeGroup

#---
#convert MonthlyIncome 
df$IncomGroup <- as.factor(
  ifelse(df$MonthlyIncome <= 2911,"Low", ifelse(
    df$MonthlyIncome <= 6503,"Average", ifelse(
    df$MonthlyIncome <= 8379,"AboveAverage","High"
  )))
)

#convert  YearsAtCompany
df$YrsAtCoGroup <- as.factor(
  ifelse(df$YearsAtCompany <= 3,"LessThan3", ifelse(
    df$YearsAtCompany <= 7,"3to7", ifelse(
    df$YearsAtCompany <= 10,"7to10","bove10"
  )))
)
#convert YearsWithCurrManager
df$YrsWtCurrMngrGroup <- as.factor(
  ifelse(df$YearsWithCurrManager <= 2,"LessThan2", ifelse(
    df$YearsWithCurrManager <= 4,"2to4", ifelse(
    df$YearsWithCurrManager <= 7,"4to7","bove7"
  )))
)
#convert YearsInCurrentRole
df$YrsInCurrRoleGroup <- as.factor(
  ifelse(df$YearsInCurrentRole <= 2,"LessTh2", ifelse(
    df$YearsInCurrentRole <= 4,"2to4", ifelse(
    df$YearsInCurrentRole <= 7,"4to7","bove7"
  )))
)
#create a new data frame with factor variables for decision tree
df_cat = df[, c(2, 3, 5, 7, 8, 11, 12, 14, 15, 16, 17, 18, 23, 25, 26, 28, 31,35, 34, 33, 37, 38, 39, 40, 41)]
str(df_cat)

#check for missing values
colSums(is.na(df))
#___________________________________________________________________________________________________________________
#________________________________________________Modeling___________________________________________________________
#___________________________________________________________________________________________________________________
#Split the Data
set.seed(010)
df_split <- sample.split(df_cat$Attrition, SplitRatio = 0.70)
df_cat_train <- subset(df_cat,df_split == T)
df_cat_test <- subset(df_cat,df_split == F)
# compare the dimention of splitted dataset
dim(df_cat)
dim(df_cat_test)
dim(df_cat_train) 

#_______________________DECESION TREES______________________________

#Split the Data for categorical features
seeds <- set.seed(010)
df_cat_split <- sample.split(df_cat$Attrition, SplitRatio = 0.70)
df_cat_train <- subset(df_cat,df_cat_split == T)
df_cat_test <- subset(df_cat,df_cat_split == F)
# compare the dimention of splitted dataset
dim(df_cat)
dim(df_cat_test)
dim(df_cat_train)
#build and plot the decision tree 
DTModel <- rpart(formula = Attrition ~., data=df_cat_train)
prp(DTModel, type = 3, clip.right.labs = TRUE, extra=8, box.palette="BlGnYl", branch = .5, under = FALSE, fallen.leaves= TRUE)
rpart.rules(DTModel, style = "tall")

#validation on test set
DTModel_pred = predict(DTModel, newdata = df_cat_test, type = 'class')
# confusion matrix
DTM_CM <- confusionMatrix(DTModel_pred, df_cat_test$Attrition)
DTM_CM
#plot the ROC curve
DTM_ROC <- plot.roc(as.numeric(df_cat_test$Attrition), as.numeric(DTModel_pred),lwd=4, type="b",grid.lty=3, grid=TRUE, print.auc=TRUE,print.auc.col= "#1B9E77", col ="#1B9E77")

#________________________RANDOM FOREST______________________________

#fitting random forest classification to the training set
RFModel = randomForest(x = df_cat_train[-1],y = df_cat_train$Attrition, ntree = 50, nodesize = 1, importance = TRUE)
#Plot the prediction
plot(RFModel)
RFModel
#predicting on the test set 
RF_pred = predict(RFModel,newdata = df_cat_test[-1],type="response")

#Making the confucion matrix
RFM_CM<-confusionMatrix(RF_pred, df_cat_test$Attrition)
RFM_CM
#Plot the ROC curve
RFModel_ROC <- plot.roc(as.numeric(df_cat_test$Attrition), as.numeric(RF_pred),lwd=4, type="b",grid.lty=3, grid=TRUE, print.auc=TRUE,print.auc.col= "#FF7F00", col ="#377EB8", main ="Tenure Based on Department")

#_____________________________XGB___________________________________
#set seeds
seeds <- set.seed(010)

#define control parameters for train function
fitControl <- trainControl(method="cv", number = 10, classProbs = TRUE, seeds = seeds)

#Build the model and predict
XGBModel <- train(Attrition~., data = df_cat_train, method = "xgbTree")
#, trControl = fitControl
XGBM_pred <- predict(XGBModel,df_cat_test)
XGBM_CM <- confusionMatrix(XGBM_pred, df_cat_test$Attrition)
XGBM_CM
#plot 
XGBModel_ROC <- plot.roc (as.numeric(df_cat_test$Attrition), as.numeric(XGBM_pred),lwd=3, 
                          type="b",grid.lty=3, grid=TRUE, print.auc=TRUE,print.auc.col= "#E41A1C", col ="#E41A1C", main ="eXtreme Gradient Boosting")


#_____________________________SVM___________________________________

#Train
SVMModel <- train(Attrition~.,df_cat_train,method = 'svmRadial',trControl = trainControl(method = 'repeatedcv',number = 3))
#Predict
SVM_pred <- predict(SVMModel,df_cat_test)
#Print confusion matrix
SVM_CM <- confusionMatrix(df_cat_test$Attrition, SVM_pred)
#Plot the ROC curve
SVM_ROC <- plot.roc (as.numeric(df_cat_test$Attrition), as.numeric(SVM_pred),lwd=3, type="b",grid.lty=3, grid=TRUE, print.auc=TRUE,print.auc.col= "#984EA3", col ="#984EA3" , main ="SVM Model")
SVM_ROC


#_____________________________GLM__________________________________
#fitting the GLM model to the training set
GLMModel <- train(Attrition~.,df_cat_train,method = 'glm', family=binomial, trControl = trainControl(method = 'repeatedcv',number = 3))
#predicting on the test set 
GLM_pred <- predict(GLMModel, df_cat_test)
#Making the confucion matrix
GLM_CM <- confusionMatrix(df_cat_test$Attrition, GLM_pred)
GLM_CM
#Plot the ROC curve
GLM_ROC <- plot.roc(as.numeric(df_cat_test$Attrition), as.numeric(GLM_pred),lwd=4, type="b",grid.lty=3, grid=TRUE, print.auc=TRUE,print.auc.col= "#386CB0", col ="#F0027F", main ="GLM Model")
GLM_ROC


#_________________________NAIVE BAYES_______________________________

#Fit the model on train data
NBModel <- naiveBayes(Attrition~., data=df_cat_train)

#Validate on test set
NBM_pred <- predict(NBModel, newdata = df_cat_test)


#Print confusion matrix
NBM_CM<-confusionMatrix(NBM_pred, df_cat_test$Attrition)
NBM_CM
#Plot
NBM_ROC <- plot.roc(as.numeric(df_cat_test$Attrition), as.numeric(NBM_pred),lwd=4, type="b",grid.lty=3, grid=TRUE, print.auc=TRUE,print.auc.col= "#FF7F00", col ="#FF7F00", main ="Naive Bayes")
NBM_ROC

#____________________________Knn____________________________________
#Fit the model on train data
KnnModel <- train(Attrition~.,df_cat_train,method = 'knn')
#trControl = trainControl(method = 'repeatedcv',number = 3)
#Predict with test set
Knn_pred <- predict(KnnModel,df_cat_test)
#Print confusion matrix
Knn_CM <- confusionMatrix(df_cat_test$Attrition, Knn_pred)
Knn_CM
#Plot the curve
plot.roc(as.numeric(df_cat_test$Attrition), as.numeric(Knn_pred),lwd=4, type="b",grid.lty=3, grid=TRUE, print.auc=TRUE,print.auc.col= "#BEBADA", col ="#BEBADA", main = "K-Nearest Neighbor")


#_____________________Find The best model___________________________

#Create a plot to compare all ROC curves
library("caTools")
colAUC(cbind(DTModel_pred, RF_pred, GLM_pred, XGBM_pred, SVM_pred, NBM_pred, Knn_pred), df_cat_test$Attrition, plotROC = TRUE)

#_____________________
# Extract values from confusion matrixes and create a list for each statistic value and models

Sensitivities <- c(DTM_CM$byClass["Sensitivity"], RFM_CM$byClass["Sensitivity"], XGBM_CM$byClass["Sensitivity"], SVM_CM$byClass["Sensitivity"], 
                   GLM_CM$byClass["Sensitivity"], NBM_CM$byClass["Sensitivity"], Knn_CM$byClass["Sensitivity"])

Specificities <- c(DTM_CM$byClass["Specificity"], RFM_CM$byClass["Specificity"], XGBM_CM$byClass["Specificity"], SVM_CM$byClass["Specificity"], 
                   GLM_CM$byClass["Specificity"], NBM_CM$byClass["Specificity"], Knn_CM$byClass["Specificity"])

Precisions <- c(DTM_CM$byClass["Precision"], RFM_CM$byClass["Precision"], XGBM_CM$byClass["Precision"], SVM_CM$byClass["Precision"], 
                GLM_CM$byClass["Precision"], NBM_CM$byClass["Precision"], Knn_CM$byClass["Precision"])

Recalls <- c(DTM_CM$byClass["Recall"], RFM_CM$byClass["Recall"], XGBM_CM$byClass["Recall"], SVM_CM$byClass["Recall"], 
             GLM_CM$byClass["Recall"], NBM_CM$byClass["Recall"], Knn_CM$byClass["Recall"])

Accuracies <- c(DTM_CM$overall[1], RFM_CM$overall[1], XGBM_CM$overall[1], SVM_CM$overall[1], GLM_CM$overall[1], NBM_CM$overall[1], Knn_CM$overall[1])

Balanced_Accuracies <- c(DTM_CM$byClass["Balanced Accuracy"], RFM_CM$byClass["Balanced Accuracy"], XGBM_CM$byClass["Balanced Accuracy"], SVM_CM$byClass["Balanced Accuracy"], 
                         GLM_CM$byClass["Balanced Accuracy"], NBM_CM$byClass["Balanced Accuracy"], Knn_CM$byClass["Balanced Accuracy"])

F1_Scores <- c(DTM_CM$byClass["F1"], RFM_CM$byClass["F1"], XGBM_CM$byClass["F1"], SVM_CM$byClass["F1"], 
               GLM_CM$byClass["F1"], NBM_CM$byClass["F1"], Knn_CM$byClass["F1"])

#Create a data frame for stat lists
Models  <- c("DTM", "RFM", "XGBM", "SVM", "GLM", "NBM", "KNN")
StatComp <- data.frame(Models, Sensitivities, Specificities, Precisions, Recalls, F1_Scores, Accuracies, Balanced_Accuracies)


#define a grid table to visualize the values 
t1 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:2], col=NA),
            fg_params=list(col="#084594", fontface=3)), 
  colhead=list(fg_params=list(col="#2171B5", fontface=2L)),
  rowhead=list(fg_params=list(col="white")))
grid.table(StatComp, theme=t1, title)
StatComp

#_______________________________________________________

#Round values to 3 decimal 
StatComp$Sensitivities <- round(StatComp$Sensitivities, digit=3)
StatComp$Specificities <- round(StatComp$Specificities, digit=3)
StatComp$F1_Scores <- round(StatComp$F1_Scores, digit=3)
StatComp$Balanced_Accuracy <- round(StatComp$Balanced_Accuracy, digit=3)

#define bar plot for all four measures
sn_p <- StatComp %>% mutate(Models = fct_reorder(Models, Sensitivities)) %>% ggplot(aes(Models, Sensitivities, fill = Models)) + geom_bar(stat='identity', alpha = 0.5) + 
  geom_text(aes(label=Sensitivities),vjust=3, size = 4) + ggtitle('Comparative Sensitivities') + xlab('') + ylab('') + theme_void() + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#377EB8", size = 16), axis.text.y = element_text(colour = "white", size = 0), axis.text.x = element_text(colour = "black", size = 10),
        axis.title.y = element_text(face = "bold", colour = "#386CB0", size = 12)) + scale_fill_brewer(palette="Greens") 
#___
sp_p <- StatComp %>% mutate(Models = fct_reorder(Models, Specificities)) %>% ggplot(aes(Models, Specificities, fill = Models)) + geom_bar(stat='identity', alpha = 0.5) + 
  geom_text(aes(label=Specificities),vjust=3, size = 4) + ggtitle('Comparative Specificities') + xlab('') + ylab('') + theme_void() + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#377EB8", size = 16), axis.text.y = element_text(colour = "white", size = 0), axis.text.x = element_text(colour = "black", size = 10),
        axis.title.y = element_text(face = "bold", colour = "#386CB0", size = 12)) + scale_fill_brewer(palette="Reds") 
#___
ba_p <- StatComp %>% mutate(Models = fct_reorder(Models, Balanced_Accuracy)) %>% ggplot(aes(Models, Balanced_Accuracy, fill = Models)) + geom_bar(stat='identity', alpha = 0.5) + 
  geom_text(aes(label=Balanced_Accuracy),vjust=3, size = 4) + ggtitle('Comparative Balanced Accuracies') + xlab('') + ylab('') + theme_void() + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#377EB8", size = 16), axis.text.y = element_text(colour = "white", size = 0), axis.text.x = element_text(colour = "black", size = 10), 
        axis.title.y = element_text(face = "bold", colour = "#386CB0", size = 12)) + scale_fill_brewer(palette="PuRd") 
#___
f1_p <- StatComp %>% mutate(Models = fct_reorder(Models, F1_Scores)) %>% ggplot(aes(Models, F1_Scores, fill = Models)) + geom_bar(stat='identity', alpha = 0.5) + 
  geom_text(aes(label=F1_Scores),vjust=3, size = 4) + ggtitle('Comparative F1 Scores') + xlab('') + ylab('') + theme_void() + 
  theme(plot.title = element_text(hjust=0.5, face = "bold", colour = "#377EB8", size = 16), axis.text.y = element_text(colour = "white", size = 0), axis.text.x = element_text(colour = "black", size = 10), 
        axis.title.y = element_text(face = "bold", colour = "#386CB0", size = 12)) + scale_fill_brewer(palette="Blues") 

# Arrange plots in one figure 
ggarrange( sp_p, sn_p, f1_p, ba_p, legend='none',  ncol=2, nrow = 2) 




