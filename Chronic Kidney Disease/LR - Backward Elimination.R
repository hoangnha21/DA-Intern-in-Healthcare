## Step 0  - Read in Data
data=read.csv('/Users/hoangnha218/Desktop/Chronic Kidney Disease Dataset.csv')
names(data)
data=data[,-1]  ## remove ID

## Step 1 - Explore and relabel Data
y=data$CKD
class(data)
summary(data)

hold_out_sample=which(is.na(data$CKD)==1)
data_without=data[hold_out_sample,]   ## the ones without a disease status
data_with=data[-hold_out_sample,]   ## the ones with a disease status
summary(data_with)

?na.omit
data_with=na.omit(data_with)
dim(data_with)
sapply(data_with, function(x) sum(is.na(x)))

## Run the Logistic Regression with one variable
model=glm(CKD~Age,family="binomial",data=data_with)
summary(model)


# the coeffienct of age is positive indicating that an increase in age is associated
#  with an increase in the probability of someone having CKD

## Step 3  - Run the Logistic Regression on all data, explore backward elimination
dim(data)
model=glm(CKD~.,family="binomial",data=data_with)
summary(model)
# notice the logistic regression automatically separated the categorical variables
#  if a category is not listed, it is included in the intercept

model2=step(model,direction="forward")
summary(model2)

model3=step(model,direction="backward")
summary(model3)

formula(model2)
formula(model3)
summary(model3)

confint.default(model3)
confint(model)

pred<-predict(model3, newdata=data_out,type="response")
pred
class<-as.factor(ifelse(pred>=0.5,"YES","NO"))
class

train=data_in
test=data_out

train <- table(train$CKD, pred)
#trainP
# TEST DATA MODEL PREDICTING
#predtest <-predict(lm1, test, type = 'response')
#testP <- table(test$`C-MANIPULATOR`, predtest)
#testP