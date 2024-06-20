library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(lessR)

#Load dataframe into r
df<-telecom_customer_churn_totally_cleaned_2

# Selecting specific numeric columns for the correlation
df1<-df %>%
  dplyr::select(Age,`Tenure (Months)`,`Total Charges`)

#calculate correlation coefficients, round to 2d.p
cormat <- round(cor(df1),2)
head(cormat) #view head of correlation coefficient rounded to 2d.p 

#melt the data frame using reshape2 library
melted_cormat <- melt(cormat)
head(melted_cormat)#view head of melted data frame

#Plotting the Correlation matrix 
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


#LOGISTIC REGRESSION

##Omitting NA values from the data
data<-telecom_customer_churn_totally_cleaned_2
data1<-na.omit(data)
sum(is.na(data1))
data1

##Selecting specific columns from the data for the model
data2<-data1 %>%
  dplyr::select(Age,`Tenure (Months)`,`Total Charges`, `Churn`)
data2$Churn<-ifelse(data2$Churn=="Churned",1,0)
data2$Churn <- as.factor(plyr::mapvalues(data2$Churn,
                                                 from=c("1","0"),
                                                 to=c("Yes", "No")))
data2


#Splitting the data into train and test subsets for the data modelling.

set.seed(1)# To reset the random number generator used for sampling
train = sample(nrow(data2),nrow(data2)*0.7,replace=FALSE)
data2_train = data2[train,]
data2_test = data2[-train,]
dim(data2_train)
dim(data2_test)

line_of_best_fit <- glm(as.factor(Churn)~.,data2_train,
              family=binomial(link='logit'))
summary(line_of_best_fit)


#Predicting both the train and test subsets

Reg_prob1 <- predict(line_of_best_fit, data2_test, type="response")
Reg_pred1 <- ifelse(Reg_prob1 > 0.5,"Yes","No")
Reg_prob2 <- predict(line_of_best_fit, data2_train, type="response")
Reg_pred2 <- ifelse(Reg_prob2 > 0.5,"Yes","No")
Pred_matrix1<- table(Predicted = Reg_pred1, Actual = data2_test$Churn)
Pred_matrix2<- table(Predicted = Reg_pred2, Actual = data2_train$Churn)
Pred_matrix1
Pred_matrix2


#Confusion Matrix based on the predicted model
library(caret)

# Test
confusionMatrix(
  as.factor(Reg_pred1),
  as.factor(data2_test$Churn),
  positive = "Yes" 
)

# Train
confusionMatrix(
  as.factor(Reg_pred2),
  as.factor(data2_train$Churn),
  positive = "Yes" 
)

#Evaluating the model

#Recall Value

Reg_recall <- Pred_matrix1[2,2]/(Pred_matrix1[2,2]+Pred_matrix1[1,2])
Reg_recall

#Precision Value

Reg_precision<- Pred_matrix1[2,2]/(Pred_matrix1[2,2]+Pred_matrix1[2,1])
Reg_precision

#Accuracy Value

model_acc1 <- sum(diag(Pred_matrix1))/sum(Pred_matrix1)
model_acc1


# ANOVA Test
## Loading a new dataset
df<- telecom_customer_churn_totally_Anova

## Data Cleaning
df1<-na.omit(df)
sum(is.na(df1))
df1

df1$Churn<-as.factor(df1$Churn) ##encoding the vector as a factor
df1$Churn<-as.numeric(df1$Churn) ## encoding the factor as a numeric

# Computing ANOVA
Anova_Test<-aov(Churn ~ OfferType, df1) ## one-way anova test
summary(Anova_Test)


































