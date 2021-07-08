# Project College Admission 

# install.packages('dplyr')
#install.packages('moments')
#install.packages('corrplot')

#Importing libraries 

library(ggplot2)
library(nortest)
library(corrplot)
library(car)

library(caret)


# Loading(importing) the required file and checking it with the View function

clgAdmission<- read.csv(choose.files(), stringsAsFactors = TRUE )
View(clgAdmission)


clgAdmission_plot<-clgAdmission


head(clgAdmission)


#checking the summary of the data 
summary(clgAdmission)


#checking for the missing values in the data
sum(is.na(clgAdmission)) # no missing values present in the dataset


sapply(clgAdmission, function(x)sum(is.na(x))) # no missing value present in the data

#finding out the outliers 

boxplot(clgAdmission)


min(clgAdmission$gre)
max(clgAdmission$gre)

boxplot(clgAdmission$gre)
boxplot(clgAdmission$gpa)




quantile(clgAdmission$gre) # looking the quantiles in the gre columns 
clgAdmission$gre<-ifelse(clgAdmission$gre<520,520,clgAdmission$gre) #replacing the lower outlier with the 2nd quantile

quantile(clgAdmission$gre)


quantile(clgAdmission$gpa)

clgAdmission$gpa<-ifelse(clgAdmission$gpa<3.130,3.130,clgAdmission$gpa)


# finding the structure of the data

str(clgAdmission)
View(clgAdmission)

# Finding out the how is the data distribution 

hist(clgAdmission$gpa) # gpa column data is not normally distributed (left skewed and has kurtosis)
hist(clgAdmission$gre) #gre column data is not normally distributed (right skewed and has kurtosis)


gpa_density<-density(clgAdmission$gpa) # plotting density plot also, showed it is not normally distributed

plot(gpa_density)

gre_density<-density(clgAdmission$gre)

plot(gre_density)

# Also, checking with the qq plot
qqnorm(clgAdmission$gre)
qqline(clgAdmission$gre)

qqnorm(clgAdmission$gpa)
qqline(clgAdmission$gpa)


shapiro.test(clgAdmission$gre) # showed the p value less than 0.05 means it is not normally distributed
shapiro.test(clgAdmission$gpa) #  # showed the p value less than 0.05 means it is not normally distributed


skewness(clgAdmission$gre)
kurtosis(clgAdmission$gre)

skewness(clgAdmission$gpa)
kurtosis(clgAdmission$gpa)

# Correcting the skewness 

clgAdmission$gpa<-log10(clgAdmission$gpa) # log10 function improves the skewness and kurtosis 

skewness(clgAdmission$gpa)
kurtosis(clgAdmission$gpa)


clgAdmission$gre<-sqrt(clgAdmission$gre)
skewness(clgAdmission$gre)
kurtosis(clgAdmission$gre) # sqrt function improves the skewness and kurtosis


# Spliting of dataset into train and test

set.seed(42)
train_indices<-sample(1:nrow(clgAdmission),round(nrow(clgAdmission)*0.75,0))
train_indices

train_clgAdmission<-clgAdmission[train_indices,]
test_clgAdmission<-clgAdmission[-train_indices,]


train_clgAdmission
test_clgAdmission

#Implementing the Logistic Regression 

logistic_model <- glm(admit ~.,family=binomial(link='logit'),data=train_clgAdmission)
summary(logistic_model)


# only using the gre, gpa, rank column as independent variable and admit as a dependent variable

logistic_model_tun <- glm(admit ~gre+gpa+rank,family=binomial(link='logit'),data=train_clgAdmission)
summary(logistic_model_tun)


# predictions

pred_train<-predict(logistic_model_tun, train_clgAdmission, type = 'response')
pred_train

pred_test<-predict(logistic_model_tun, test_clgAdmission, type = 'response')
pred_test
# probabilities for 1


logit_train<-ifelse(pred_train>=0.5 , 1, 0)

logit_test<-ifelse(pred_test>=0.5, 1, 0)

# confusion matrix
confusion_mat<-table(predicted=logit_test, actual=test_clgAdmission$admit)
confusion_mat

#accuracy

accuracy_logit<-sum(diag(confusion_mat))/(sum(confusion_mat))
accuracy_logit     


# logistic Regression giving the Accuracy of 78-79 % 

# Decision Tree

library(party)


decision_clf<- ctree(admit~.,  data = train_clgAdmission)

plot(decision_clf)

summary(decision_clf)


# prediction for the decision tree
pred_decision_tree<-predict(decision_clf,test_clgAdmission[-1])



# Accuracy for the Decision Tree

pred_dec_clf<-predict(decision_clf, test_clgAdmission)
pred_dec_clf


# confusion matrix 

confusion_mat_decision<-table(predicted=pred_dec_clf,actual=test_clgAdmission$admit)
confusion_mat_decision

accuracy_decision<-sum(diag(confusion_mat_decision))/sum(confusion_mat_decision)
accuracy_decision

# decision tree giving the accuracy of 60 %


# implementing the SVM 


library(e1071)

svm_clf <- svm(factor(admit) ~ gre+gpa+rank, data = train_clgAdmission, cost = 62.5, gamma = 0.5)

# prediction for the test data with svm

pred_svm<-predict(svm_clf,test_clgAdmission[-1])

pred_svm

# Confusion Matrix

confusion_mat_svm<-table(predicted=pred_svm,actual=test_clgAdmission$admit)

accuracy_svm<-sum(diag(confusion_mat_svm))/sum(confusion_mat_svm)

accuracy_svm

# SVM giving the accuracy of 75 % 



# Naive Bayes 

nb_model<-naiveBayes(admit~.,data = train_clgAdmission)

nb_model

# prediction with Naive Bayes model
pred_nb_model<-predict(nb_model,test_clgAdmission)

pred_nb_model


# confusion matrix

confusion_mat_nb<- table(predicted=pred_nb_model, actual=test_clgAdmission$admit)

confusion_mat_nb


# accuracy 

accuracy_nb<- sum(diag(confusion_mat_nb))/sum(confusion_mat_nb)

accuracy_nb


# Naive Bayes giving the accuracy of 77%

# KNN 

library("class")

knn_model<- knn(train_clgAdmission, test_clgAdmission, train_clgAdmission$admit, k=10)
knn_model

# confusion matrix

confusion_mat_knn<- table(predicted=knn_model, actual=test_clgAdmission$admit)
confusion_mat_knn

# accuracy

accuracy_knn<- sum(diag(confusion_mat_knn))/sum(confusion_mat_knn)
accuracy_knn

#KNN giving the accuracy of 96-97%

# KNN giving the best accuracy score followed by Logistic Regression.


#Descriptive Analysis 
# Categorizing the average of grade point into High, Medium, and Low 
# (with admission probability percentages) and plot it on a point chart.  

des_Analysis<- transform(clgAdmission_plot, GRE_score=ifelse(gre<440,"Low",ifelse(gre<580,"Medium","High")))
View(des_Analysis)



sum_des_Analysis<- aggregate(admit~GRE_score, des_Analysis,FUN = sum)

length_des_Analysis<-aggregate(admit~GRE_score, des_Analysis,FUN = length)

prob_table<- cbind(sum_des_Analysis, Rec=length_des_Analysis[,2])

prob_table_transform<- transform(prob_table, Probability_Admission_in_percentage=admit/Rec)

prob_table_transform


# Plotting 

library("ggplot2")

ggplot(prob_table_transform, aes(x=GRE_score, y=Probability_Admission_in_percentage)) +  geom_point()


#Cross grid for admission variables with GRE Categorization 
table(des_Analysis$admit,des_Analysis$GRE_score)







