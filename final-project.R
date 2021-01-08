##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)

incomes_ds <- read.csv('adult.csv', stringsAsFactors = F, na.strings=c("","NA"," ","?"))
nrow(incomes_ds)
ncol(incomes_ds)
head(incomes_ds)
colnames(incomes_ds)
str(incomes_ds)

### workclass simplification
table(incomes_ds$workclass)
incomes_ds$workclass <- ifelse(incomes_ds$workclass %in% c("Never-worked","Without-pay"),"Unemployed",incomes_ds$workclass)
incomes_ds$workclass <- ifelse(incomes_ds$workclass %in% c("Local-gov","Federal-gov","State-gov"),"Gov",incomes_ds$workclass)
incomes_ds$workclass <- ifelse(incomes_ds$workclass %in% c("Self-emp-inc","Self-emp-not-inc"),"Self-emp",incomes_ds$workclass)

### marital status simplification
incomes_ds$marital.status <- ifelse (incomes_ds$marital.status %in% c("Separated","Divorced","Widowed"),"Not-Married",incomes_ds$marital.status)
incomes_ds$marital.status <- ifelse (incomes_ds$marital.status %in% c("Married-AF-spouse","Married-civ-spouse","Married-spouse-absent"),"Married",incomes_ds$marital.status)
table(incomes_ds$marital.status)

### country simplification
Asia <- c("China","Hong","India","Iran","Cambodia","Japan", "Laos","Philippines" ,"Vietnam" ,"Taiwan", "Thailand")

N.A <- c("Canada","United-States","Puerto-Rico")

Europe <- c("England","France","Germany" ,"Greece","Holand-Netherlands","Hungary","Ireland","Italy","Poland","Portugal","Scotland"
            ,"Yugoslavia")

S.A <- c("Columbia","Cuba","Dominican-Republic","Ecuador","El-Salvador","Guatemala","Haiti","Honduras","Mexico","Nicaragua"
                   ,"Outlying-US","Peru","Jamaica","Trinadad&Tobago")
Others <- c("South")
grp_cntry <- function(cntry){
    if (cntry %in% Asia){
        return("Asia")
    }else if (cntry %in% N.A){
        return("North America")
    }else if (cntry %in% Europe){
        return("Europe")
    }else if (cntry %in% S.A){
        return("South America")
    }else{
        return("Others")      
    }
}

incomes_ds$native.country <- sapply(incomes_ds$native.country,grp_cntry)
table(incomes_ds$native.country)

#### conversion to factor
incomes_ds$workclass <- as.factor(incomes_ds$workclass)
incomes_ds$education <- as.factor(incomes_ds$education)
incomes_ds$education.num <- as.factor(incomes_ds$education.num)
incomes_ds$marital.status <- as.factor(incomes_ds$marital.status)
incomes_ds$occupation <- as.factor(incomes_ds$occupation)
incomes_ds$relationship <- as.factor(incomes_ds$relationship)
incomes_ds$race <- as.factor(incomes_ds$race)
incomes_ds$sex <- as.factor(incomes_ds$sex)
incomes_ds$native.country <- as.factor(incomes_ds$native.country)
incomes_ds$income <- ifelse(incomes_ds$income=='>50K',1,0)
incomes_ds$income <- as.factor(incomes_ds$income)

### missing data
library(Amelia)
missmap(incomes_ds)

sum(is.na(incomes_ds$occupation))
incomes_ds <- incomes_ds[!is.na(incomes_ds$occupation),]
sapply(1:ncol(incomes_ds), function (n) {sum(is.na(incomes_ds[,n]))})
nrow(incomes_ds)


### Data exploration
incomes_ds %>% ggplot(aes(x=income))+ geom_bar()
incomes_ds %>% group_by(income)%>% summarise(avg=n()/nrow(incomes_ds))

incomes_ds %>% ggplot(aes(x=workclass))+ geom_bar()
incomes_ds %>% group_by(workclass)%>% summarise(count=n())

incomes_ds %>% ggplot(aes(x=workclass,fill=income)) + 
  geom_bar(position = "fill")+ 
  scale_fill_discrete(labels=c("<=50k", ">50k"))+
  theme(axis.text.x = element_text(angle = 90))

incomes_ds %>% ggplot(aes(x=sex,fill=income)) + 
  geom_bar(position = "fill")+
  facet_wrap(incomes_ds$workclass) + 
  scale_fill_discrete(labels=c("<=50k", ">50k"))

incomes_ds %>% ggplot(aes(x=education.num,fill=income)) + 
  geom_bar(position = "fill")+ scale_fill_discrete(labels=c("<=50k", ">50k"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(breaks=incomes_ds$education.num,

incomes_ds %>% ggplot(aes(x=income,y=age))+geom_boxplot()

### create age range
range <- as.factor(c("-20","21-25","26-30","31-35",
                     "36-40","41-45","46-50","51-55",
                     "56-60","60-65","66+"))
age_range <- sapply(incomes_ds$age,function(age) {
  if(age <= 20) range[1]
  else if (age <= 25) range[2]
  else if (age <= 30) range[3]
  else if (age <= 35) range[4]
  else if (age <= 40) range[5]
  else if (age <= 45) range[6]
  else if (age <= 50) range[7]
  else if (age <= 55) range[8]
  else if (age <= 60) range[9]
  else if (age <= 65) range[10]
  else range[11]
})
incomes_ds <- incomes_ds %>% mutate(agerange=age_range)
incomes_ds %>% ggplot(aes(x=agerange,fill=income)) + 
  geom_bar(position = "fill")+ 
  scale_fill_discrete(labels=c("<=50k", ">50k"))+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_discrete(name="Age Range")


incomes_ds %>% ggplot(aes(x=marital.status,fill=income)) + geom_bar(position = "fill")+ scale_fill_discrete(labels=c("<=50k", ">50k"))+theme(axis.text.x = element_text(angle = 90))+scale_x_discrete(name="Marital Status")

incomes_ds <- incomes_ds %>% select(agerange,workclass,education,marital.status,occupation,native.country,sex,race,income)

incomes_ds <- incomes_ds[sample(nrow(incomes_ds), 5000), ]

### create-test-train sets
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = incomes_ds$income, times = 1, p = 0.15, list = FALSE)
train_set <- incomes_ds[-test_index,]
test_set <- incomes_ds[test_index,]

### Naive Bayes
set.seed(1, sample.kind="Rounding")
fit_naive <- train(income ~ . , method = "naive_bayes", data = train_set)
predict_naive <- predict(fit_naive,newdata = test_set)
accuracy_naive <- confusionMatrix(predict_naive,test_set$income)$overall['Accuracy']
accuracy_naive

### Glm 
set.seed(1, sample.kind="Rounding")
fit_glm <- train(income ~ . , method = "glm", data = train_set,family = binomial(logit))
predict_glm <- predict(fit_glm,newdata = test_set)
accuracy_glm <- confusionMatrix(predict_glm,test_set$income)$overall['Accuracy']
accuracy_glm

### multinom
set.seed(1, sample.kind="Rounding")
fit_multinom <- train(income ~ . , method = "multinom", data = train_set)
predict_multinom <- predict(fit_multinom,newdata = test_set)
accuracy_multinom <- confusionMatrix(predict_multinom,test_set$income)$overall['Accuracy']
accuracy_multinom

### knn
set.seed(1, sample.kind="Rounding")
fit_knn <- train(income ~ . , method = "knn", data = train_set,tuneGrid = data.frame(k = c(3,5,7,9)))
predict_knn <- predict(fit_knn,newdata = test_set)
accuracy_knn <- confusionMatrix(predict_knn,test_set$income)$overall['Accuracy']
accuracy_knn

### rpart
set.seed(1, sample.kind="Rounding")
fit_rpart <- train(income ~ . ,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                   data = train_set)
predict_rpart <- predict(fit_rpart,newdata = test_set)
accuracy_rpart <- confusionMatrix(predict_rpart,test_set$income)$overall['Accuracy']
accuracy_rpart

library(rattle)
fancyRpartPlot(fit_rpart$finalModel,cex=0.75,main="Decision Tree",palettes = "Reds")

### rf
set.seed(1, sample.kind="Rounding")
fit_rf <- train(income ~ . , method = "rf", data = train_set,tuneGrid = data.frame(mtry = c(1, 5, 10)))
predict_rf <- predict(fit_rf,newdata = test_set)
accuracy_rf <- confusionMatrix(predict_rf,test_set$income)$overall['Accuracy']
accuracy_rf

### xgbTree
set.seed(1, sample.kind="Rounding")
fit_xgbTree <- train(income ~ . , method = "xgbTree", data = train_set)
predict_xgbTree <- predict(fit_xgbTree,newdata = test_set)
accuracy_xgbTree <- confusionMatrix(predict_xgbTree,test_set$income)$overall['Accuracy']
accuracy_xgbTree

### ensemble prediction by majority vote
pos_glm <- predict_glm == 1
pos_knn <- predict_knn == 1 
pos_naive <- predict_naive == 1 
pos_rf <- predict_rf == 1 
pos_rpart <- predict_rpart == 1 
pos_multinom <- predict_multinom == 1 
pos_xgbTree <- predict_xgbTree == 1 
predictions <- cbind(pos_glm,pos_knn,pos_naive,pos_rf,pos_rpart,pos_xgbTree,pos_multinom)
votes <- rowMeans(predictions)
y_hat <- ifelse(votes > 0.5, "1", "0")
accuracy_ens <- mean(y_hat == test_set$income)
accuracy_ens
