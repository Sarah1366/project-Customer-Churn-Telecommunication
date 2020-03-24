---
title: "Customer_churn"
author: "Sara Mousavimehr"
date: "March 5, 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r }
library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(partition)
library(caret)
library(randomForest)
library(ROSE)
library(naivebayes)
library(psych)
install.packages("ggcorrplot")
library(ggcorrplot)

```

```{r}
#read the dataset
cchurn<-read.csv("C:\\Users\\sara_\\OneDrive\\Documents\\capstone project\\Telco-Customer-Churn.csv")
str(cchurn)
summary(cchurn)
#we notice that Totalcharges is the only attribute that has missing value( 11 NA`s) 


```

```{r}
#Dealing with the missing values

#We just find 11NA`s in Totalcharges attribute and it`s very small(1,5% of the data)
#so we replace these 11NA`s with the mean of Totalcharges.
cchurn$TotalCharges[is.na((cchurn$TotalCharges))]<-mean(cchurn$TotalCharges, na.rm = TRUE)

#Checking the missing values
sum(is.na(cchurn))

```

```{r}
#Dropping customerID column from the dataset since it has no influence 
cchurn<- subset(cchurn, select = -c(customerID))
View(cchurn)

```

```{r}
#Changing SeniorCitizen to be a factor
cchurn$SeniorCitizen<- as.factor(cchurn$SeniorCitizen)
str(cchurn)
```

```{r}
#The dataset has 21 variables.18 of them are categorical

#Univariate Analysis
names(cchurn)
descriptive<-c("gender" ,"SeniorCitizen","Partner","Dependents" ,"tenure", "PhoneService","MultipleLines","InternetService" ,"OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport" ,"StreamingTV", "StreamingMovies","Contract","PaperlessBilling","PaymentMethod","MonthlyCharges", "TotalCharges", "Churn"  )

#Let`s see who are our customers:
for (i in descriptive) {
  print(barplot(table(cchurn[,i]), xlab = colnames(cchurn[i]), col = c("bisque","cyan1","darkorchid1","darkseagreen1")))
}

```

```{r}
#Bivariate Analysis
#The relationship between each attribute and the churn
#The count of gender and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, gender) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = gender, y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 

#The count of SeniorCirizen and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, SeniorCitizen) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = SeniorCitizen, y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of PhoneService and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, PhoneService) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = PhoneService ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of MultipleLines and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, MultipleLines) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = MultipleLines ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )



#The relationship between each attribute and the churn 
#The count of InternetService and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, InternetService) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = InternetService ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of OnlineSecurity and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, OnlineSecurity) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = OnlineSecurity ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of DeviceProtection and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, DeviceProtection) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = DeviceProtection ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of TechSupport and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, TechSupport) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x = TechSupport ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of StreamingTV and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, StreamingTV) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x =StreamingTV ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of StreamingMovies and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, StreamingMovies) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x =StreamingMovies ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of Contract and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, Contract) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x =Contract ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


#The relationship between each attribute and the churn 
#The count of PaperlessBilling and frequency and the relationship with customer churn or not

cchurn %>%

  group_by(Churn, PaperlessBilling) %>%

  summarize(count = n())%>%

  mutate(Frequency = count / sum(count))%>%

  ggplot(., aes(x =PaperlessBilling ,y = Frequency, fill = Churn)) +

  geom_col( position ="dodge" )


```

```{r}
#let's subset the customer churn and see if they have anything in common 

churn_sub <- cchurn[which(cchurn$Churn == 'Yes'), ] 

#subsetting the dataset 

nochurn_sub <- cchurn[which(cchurn$Churn == 'No'), ] 

```

```{r}

#Higher monthly charges might be one of the reasons of customer churn 

boxplot(churn_sub$MonthlyCharges, nochurn_sub$MonthlyCharges, names = c("Churn", "Not Churn"), col = c("antiquewhite1", "darkorchid"), horizontal = TRUE, xlab = "Monthly Charges")

```

```{r}
#Notice that the churn monthly charge is always higher in all kind of contracts too 

ggplot(cchurn, aes(x = Contract, y = MonthlyCharges, fill = Churn)) +

  geom_boxplot( ) +

  coord_flip() +

  theme_economist()

#We can coclude that the higher the monthly charge the more likely for the customer to churn 

```

```{r}
#Percentage of customer churn

churn_count <- table(cchurn$Churn)

churn_percent <- table(cchurn$Churn) / length(cchurn$Churn)

barplot(churn_count, main = "Customer Churn", col =  "darkorchid1")

#notice the the data is imbalance 

```

```{r}
#We notice that the data imbalanced.



#imbalanced data refers to classification problems where we have unequal instances for different classes. Most machine learning classification algorithms are sensitive to unbalance in the predictor classes. 



#Under-sampling, we randomly select a subset of samples from the class with more instances to match the number of samples coming from each class. 

#oversampling, we randomly duplicate samples from the class with fewer instances or we generate additional instances based on the data that we have, so as to match the number of samples in each class. 

#checking the severity of imbalanced data 

table(cchurn$Churn)

```

```{r}

#Dividing our dataset into training and test set then deal with the imbalanced data 

set.seed(100)

index <- sample(2, nrow(cchurn), replace = TRUE, prob = c(0.7, 0.3)) #in this case 70% of the data will go for training 

train_data <- cchurn[index==1, ] #subsetting the training data 70% 

test_data  <- cchurn[index==2,  ] #subsetting the test data 30%


```

```{r}
#imbalanced data: means the predicting target class distribution is skewed. balancing the data will avoid over or under sampling 

#balancing the data 

nrow(train_data)

balanced <- ovun.sample(Churn~., data = train_data, method = "both", p = 0.5)$data

table(both$Churn)

```

```{r}

# Developing predictive model 

# Predictive model: Random Forest

rf_model <- randomForest(Churn~., data = balanced)

#evaluating the model

confusionMatrix(predict(rf_model, test_data), test_data$Churn, positive = "Yes")

#Model accuracy 0.76

```

```{r}

#Developing predictive model 

#Predictive model: Naive Bayes

nb_model <- naive_bayes(Churn~., data = balanced)


#evaluating the model 

confusionMatrix(predict(nb_model,test_data), test_data$Churn, positive = "Yes")

#Model accuracy is 0.7
```

```{r}
#Predictive model:SVM
library(e1071)

svm_model <-svm (Churn~., data = balanced)

#evaluating the model 

confusionMatrix(predict(svm_model ,test_data), test_data$Churn, positive = "Yes")


#model Acurracy 0.73



Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
