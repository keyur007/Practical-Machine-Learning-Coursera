---
title: "Practical Machine Learning Project"
author: "Keyur Kariya"
date: "27 June 2018"
output: pdf_document
---

#Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.

In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har 


#Data Analysis and Creating Predictors

##1- Preparations for Analysis

Loading libraries
```{r ,echo=TRUE, message=FALSE,warning=FALSE,results='hide'}
library(caret)
library(randomForest)
library(knitr)
library(ggplot2)

```

Downloading and loading data
```{r ,echo=TRUE}
trainlink <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
validationlink <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training_main <- read.csv(url(trainlink))
validation_main<- read.csv(url(validationlink))
#featurePlot()
```

Checking data

```{r ,echo=TRUE, message=FALSE,warning=FALSE,results='hide'}
head(training_main)
summary(training_main) 
```

Some columns have lots of NA or blank values.

##2- Cleaining the data

Cleaning columns with more than %60 NA values

```{r ,echo=TRUE}
training_cleaned<-training_main[ , colSums(is.na(training_main)) < .6] #We are creating new variables cleaning unnecessarry NA Values
```


Removing Near Zero Variances

```{r ,echo=TRUE}
training_NZV <- nearZeroVar(training_cleaned, saveMetrics=TRUE)
training_cleaned<- training_cleaned[,training_NZV$nzv==FALSE]
```

Removing first column which is test numbers and checking 


```{r ,echo=TRUE}
training_cleaned$X<-NULL
```

Checking again Cleaned Data for anomalies

```{r ,echo=TRUE, message=FALSE,warning=FALSE,results='hide'}
summary(training_cleaned)

```


We also need to clean validation set with same values

```{r ,echo=TRUE}
validation<-validation_main[ , colSums(is.na(training_main)) < .6]
validation<- validation[,training_NZV$nzv==FALSE]
validation$X<-NULL
```


##3- Creating a test set from our training Data

We are dividing our training data into two parts; training (0.70) and test (0.30) to test it before validation

```{r ,echo=TRUE}
set.seed(32343)
inTrain <- createDataPartition(y=training_cleaned$classe, p=0.70, list=FALSE)
training<-training_cleaned[inTrain,] #creating training data set

testing<-training_cleaned[-inTrain,]

dim(testing)

```
```{r ,echo=TRUE}
dim(training)

```
```{r ,echo=TRUE}
dim(validation)

```


##4- Creating Models

###a-  Classification Tree

```{r ,echo=TRUE}
modelFit_class <- train(classe ~.,data=training, method="rpart")
prediction_class <- predict(modelFit_class,newdata=testing)
confusionMatrix(prediction_class,testing$classe)


```

With "classification Tree" model, accuracy is 0.46 on testing set


###b-  Random Forest


```{r ,echo=TRUE}
modelFit_rf <- randomForest(classe ~.,data=training)
prediction_rf <- predict(modelFit_rf,newdata=testing)
confusionMatrix(prediction_rf,testing$classe)

```

With Random Forest model, accuracy is 0.9988 on testing set which is better than Classification Tree model. 


#Summary

We will select "Random Forest method for our validation set which has better prection on testing set.

Validation set results:

```{r ,echo=TRUE}
fixFrame <- head(training,1) #take first row of training set

fixFrame <- fixFrame[, -length(colnames(fixFrame))] #remove last column (classe)

validation1<-validation[,-58] #remove id from validation data set since it is not needed for predict model. Now both have same amount of column

validation1 <- rbind(fixFrame, validation1) #add first row of training set to validation set, it somehow make column class same as testing and training sets

validation1 <- validation1[-1,] #remove first row we added previously

validation_predicts<-predict(modelFit_rf,newdata=validation1) # run RF method and it works well

validation_predicts # print classe from prediction

```
