#Load libraries 
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(caret)
library(ROCR)
library(pROC)

#Import csv
hints<- read.csv("/Users/hoangnha218/Desktop/HINTS Analysis/hints5_cycle4_public.csv")

#Select relevant columns 

rawdata <- select(hints, Age, BirthGender, IncomeRanges, MaritalStatus, Occupation_Employed,
               ,Occupation_Homemaker, Occupation_Student,Occupation_Retired, Occupation_Disabled, GeneralHealth,
               , FreqGoProvider,Electronic_SelfHealthInfo,Electronic_TalkDoctor, Electronic_TestResults,
               Electronic_MadeAppts, TabletHealthWellnessApps, IntRsn_VisitedSocNet, IntRsn_SharedSocNet, IntRsn_SupportGroup,IntRsn_YouTube, AccessOnlineRecord)

# I. DATA CLEANNING ~~~~~~~~~~~

#Check for missing values 
sapply(rawdata,function(x) sum(is.na(x))) 
#There are no missing values in all columns.

#Remove negative value in the Target Variable column "AccessOnlineRecord" and any other column
df<-data.frame(rawdata)
df[df<0] <- NA

#Omit NA rows
df1 <- na.omit(df)
df1
# Add categorical variable to the data frame within the "AccessOnlineRecord" column - 
#"Yes" and "No" indicate have access or no access to the medical online record
df1$AccessOnlineRecord <- ifelse(df1$AccessOnlineRecord == 0, 0,1)
df1

# Check for outliers
boxplot(df1,las=2.6,vertical = TRUE,main = "Boxplot of Raw Data")
#z score tells how many standard deviations a given value is from the mean. We define an observation to be an outlier if it has a z-score less than -3 or greater than 3.
#find absolute value of z-score for each value in each column
z_scores <- as.data.frame(sapply(df1, function(df1) (abs(df1-mean(df1))/sd(df1))))
# remove rows that have at least one z-score with an absolute value greater than 3.
finaldata <- df1[!rowSums(z_scores>3), ]
boxplot(finaldata,las=2.6,vertical = TRUE,main = "Boxplot of Cleaned Data")
number_of_outliers = dim(df1) - dim(finaldata)
number_of_outliers

# II. EXPLORATORY DATA ANALYSIS ~~~~~~~~~~~

#Check for Correlation
corr_matrix <- round(cor(df1), digits = 2)
corrplot(corr_matrix, type = "upper",order = "hclust",col=brewer.pal(n=5, name= "RdYlBu"),tl.cex=0.5)

#As seen in the heatmap, "Electronic_Test Result" is the most correlated with the target variable. Variables that have moderate correlation 
#with the target variable are Electronic_TalkDoctor, Electronic_MadeAppts, TabletHealthWellnessApps and Electronic_SelfHealthInfo

# Convert target variable 'AccessOnlineRecord' column to factor
finaldata$AccessOnlineRecord <- ifelse(finaldata$AccessOnlineRecord == 0, "No","Yes")
finaldata$AccessOnlineRecord <- as.factor(finaldata$AccessOnlineRecord)
# It can be observed that the target variable is the categorical variable in the data. So we need to factorize this variable.

# III. MACHINE LEARNING MODEL ~~~~~~~~

#Split data into train and test
set.seed(120)
index<-sample(2,nrow(finaldata),replace=TRUE,prob=c(0.8,0.2))
train<-finaldata[index==1,] 
test<- finaldata[index==2,]

# 1. Develop a logistic model ~~~~~~~~
lm1 <- glm(`AccessOnlineRecord`~ .,
          data= train,family="binomial")
summary(lm1)

#run anova 
anova(lm1, test = 'Chisq')
#with 95% confidence level, a variable having p<0.05 is considered important predictors. 
#From the output, variables such as "Electronic_SelfHealthInfo",
#"Electronic_TalkDoctor","Electronic_TestResults","TabletHealthWellnessApps" should be considered for the second model since they are good predictors.

lm2 <- glm(`AccessOnlineRecord`~ Electronic_TalkDoctor + Electronic_MadeAppts 
           + TabletHealthWellnessApps + Electronic_SelfHealthInfo + Electronic_TestResults,
          data= train,family="binomial")
summary(lm2)

#compare two models
anova(lm1,lm2,test = "Chisq")

log_predict <- predict(lm2,newdata = test,type = "response")
log_predict <- ifelse(log_predict > 0.5,1,0)

#Plot ROC Curve and Calculate AUC

pr <- prediction(log_predict,test$AccessOnlineRecord)
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
auc(test$AccessOnlineRecord,log_predict) #86.47%
plot(perf)

