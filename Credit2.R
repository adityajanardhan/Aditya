# Uploading all the necessary libraries for regression#
library(gains)
library(dplyr)
library(irr)
library(caret)
library(ROCR)
library(pROC)

#Setting the path for the file#
setwd("C:/Users/Admin/Downloads")

#Load the csv file#
credit<-read.csv("Credit.csv",stringsAsFactors =TRUE)

#Checking the Summary Statistics
summary(credit)
plot(credit$NPA.Status)
str(credit)

#Deleting duplicate column monthlyincome.1 #
credit%>%select(-MonthlyIncome.1)->credit

#################Data preperation ###################################################

#Renaming complex variable names#
credit$Default60<-credit$NumberOfTime30.59DaysPastDueNotWorse
credit$Default90<-credit$NumberOfTime60.89DaysPastDueNotWorse
credit$Default91<-credit$NumberOfTimes90DaysLate
credit$Opencredit<-credit$NumberOfOpenCreditLinesAndLoans
credit$Realestate<-credit$NumberRealEstateLoansOrLines
credit$Dependents<-credit$NumberOfDependents
credit$Revolve<-credit$RevolvingUtilizationOfUnsecuredLines

#Deleting old variables#
credit<-select(credit,-NumberOfTime30.59DaysPastDueNotWorse,-NumberOfTime60.89DaysPastDueNotWorse,-NumberOfTimes90DaysLate,
               -NumberOfOpenCreditLinesAndLoans,-NumberRealEstateLoansOrLines,-NumberOfDependents,
               -RevolvingUtilizationOfUnsecuredLines)


#Deleting missing values for Dependents variable#
index1<-which(is.na(credit$Dependents))
credit<-credit[-index1,]

#Replacing percentage values greater than 1 as 1 by thumb rule(Percentage cannot be greater than 1)#
index2<-which(credit$Revolve>1)
credit$Revolve[index2]<-1

index3<-which(credit$DebtRatio>1)
credit$DebtRatio[index3]<-1


#Simplyfying the values for regression based on frequency#

table(credit$Default60,credit$NPA.Status)
table(credit$Default90,credit$NPA.Status)
table(credit$Default91,credit$NPA.Status)
table(credit$Opencredit,credit$NPA.Status)
table(credit$Realestate,credit$NPA.Status)
table(credit$Dependents,credit$NPA.Status)

table(credit$Region,credit$NPA.Status)

#Note: Here 3 implies all values equal or greater than 3 in the column due to less data-points#
credit$Default60<-ifelse(credit$Default60>=3,3,as.integer(credit$Default60))

#Note: Here 1 implies all values equal or greater than 1 in the column due to less data-points#
credit$Default90<-ifelse(credit$Default90>=1,1,as.integer(credit$Default90))
credit$Default91<-ifelse(credit$Default91>=1,1,as.integer(credit$Default91))
#Note: Here 3 implies all values equal or greater than 3 in the column #
credit$Dependents<-ifelse(credit$Dependents>=3,3,as.integer(credit$Dependents))
credit$Realestate<-ifelse(credit$Realestate>=3,3,as.integer(credit$Realestate))

#Imputing MonthlyIncome missing values and deleting outliers#

n_breaks<-sqrt(nrow(credit))
histogram(credit$MonthlyIncome,breaks=n_breaks)
plot(credit$MonthlyIncome)
boxplot(credit$MonthlyIncome)
quantile(credit$MonthlyIncome,p=c(1:100)/100,na.rm = TRUE)
index_outlier1<-which(credit$MonthlyIncome==730483)
credit<-credit[-index_outlier1,]
index_outlier2<-which(credit$MonthlyIncome>35000)
credit<-credit[-index_outlier2,]

#Gives the default in each decile#
credit%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(NPA.Status,quantile)%>%summarise(N=n())%>%filter(NPA.Status==1)->dat


#Gives total rows in each decile#
credit%>%mutate(quantile=ntile(MonthlyIncome,10))%>%group_by(quantile)%>%summarise(N=n())->dat1
dat1$percentage<-dat1

#Finding the default rate in each decile#
summary(credit$MonthlyIncome)
dat$percentage<- dat$N/dat1$N
dat

quantile(credit$MonthlyIncome,p=(0:10)/10,na.rm=T)

#Replacing the missing values with the approximate average of 4th and 5th decile#
credit$MonthlyIncome[is.na(credit$MonthlyIncome)]<-5000




#Converting variables into factors#
credit$NPA.Status<-as.factor(credit$NPA.Status)
credit$Default60<-as.factor(credit$Default60)
credit$Default90<-as.factor(credit$Default90)
credit$Default91<-as.factor(credit$Default91)
credit$Dependents<-as.factor(credit$Dependents)
credit$Realestate<-as.factor(credit$Realestate)


#Splitting into test and training samples#
set.seed(200)
index_mod<-sample(nrow(credit),0.7*nrow(credit),replace=F)
train<-credit[index_mod,]
test<-credit[-index_mod,]

##Build first model using all variables##########################

mod<-glm(NPA.Status~.,data=test[,-1],family="binomial")#Dropping the ID number#
summary(mod)

step(mod,direction = "both")
mod1<-glm(formula=NPA.Status~Region+Education+Default60+Default90+Default91+
            Revolve,data=train[,-1],family="binomial")
summary(mod)


#Creating a model with significant variables#
mod2<-glm(formula=NPA.Status~Region+Default60+Default90+Default91+
            Revolve,data=train[,-1],family="binomial")
summary(mod2)

#Using the model to predict the values for test#
predict <- predict(mod2, type = 'response',newdata=test)
head(predict)

table(credit$NPA.Status)/nrow(credit)
predict<-ifelse(predict>=0.10,1,0)
predict<-as.numeric(predict)


#Validation of the model#

#Confusion Matrix#
table(test$NPA.Status, predict)


#ROC#
ROCRpred <- prediction(predict, test$NPA.Status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf)
abline(0,1,col="blue") #45 degree line#

#AUC Value#
auc(test$NPA.Status,predict)



#######DECISION TREES##########################################################

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

mod_dt<-rpart(NPA.Status~.,data=credit[,-1],control=rpart.control(cp=0.002
              ,maxdepth=7),method="class", parms=list(split="gini"))

plot(mod_dt,margin=0.1)
text(mod_dt,use.n=TRUE,all=TRUE,cex=0.7)

#Better graphical plot#
fancyRpartPlot(mod_dt)
princp(mod_dt)
plotcp(mod_dt,minline=TRUE)

mod_dt<-prune(mod_dt,cp=0.027)

predict_dt<-predict(mod_dt,type="class")
predict_dt<-as.numeric(predict_dt)

#ROC#
pred<-prediction(predict_dt,credit$NPA.Status)
perf<-performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,col="blue")

#AUC#
auc(credit$NPA.Status,predict_dt)

##### Checking the new default rate using the optimum model#########

#We will use Logistic model based on better AUC#


#Using the predicted probability#
predict2 <- predict(mod2, type = 'response',newdata=test)

#Assigning cutoff for 80%ile values of predicted probabilty#
cutoff<-quantile(predict2,0.8)
cutoff

act_val<-as.numeric(test$NPA.Status)
act_val<-ifelse(act_val==2,1,0)
act_val

pred_val<-ifelse(predict2>cutoff,1,0)


#The default rate when acceptance rate is 80%#
sum(act_val[pred_val==0])/length(act_val)











