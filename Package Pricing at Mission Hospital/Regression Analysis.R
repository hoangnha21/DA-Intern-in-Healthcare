library(readr)

data <- read_csv("/Users/hoangnha218/Desktop/Mission Hospital Case/data.csv")
nrow(data) # 248 rows of 23 (51 expanded) variables
data <- as.data.frame(data)
attach(data)
str(data)

# Remove Columns by Index
data <- data[,-1]

data$`MARITAL STATUS`<-as.factor(data$`MARITAL STATUS`)
data$`KEY COMPLAINTS -CODE`<-as.factor(data$`KEY COMPLAINTS -CODE`)
data$`PAST MEDICAL HISTORY CODE`<-as.factor(data$`PAST MEDICAL HISTORY CODE`)
data$`MODE OF ARRIVAL`<-as.factor(data$`MODE OF ARRIVAL`)
data$`STATE AT THE TIME OF ARRIVAL`<-as.factor(data$`STATE AT THE TIME OF ARRIVAL`)
data$`TYPE OF ADMSN`<-as.factor(data$`TYPE OF ADMSN`)
data$`IMPLANT USED (Y/N)`<-as.factor(data$`IMPLANT USED (Y/N)`)
data$ACHD<-as.factor(data$ACHD)
data$`CAD-DVD`<-as.factor(data$`CAD-DVD`)
data$`CAD-SVD`<-as.factor(data$`CAD-SVD`)
data$`CAD-TVD`<-as.factor(data$`CAD-TVD`)
data$`CAD-VSD`<-as.factor(data$`CAD-VSD`)
data$`OS-ASD`<-as.factor(data$`OS-ASD`)
data$`other- heart`<-as.factor(data$`other- heart`)
data$`other- respiratory`<-as.factor(data$`other- respiratory`)
data$`other-general`<-as.factor(data$`other-general`)
data$`other-nervous`<-as.factor(data$`other-nervous`)
data$`other-tertalogy`<-as.factor(data$`other-tertalogy`)
data$`PM-VSD`<-as.factor(data$`PM-VSD`)
data$RHD<-as.factor(data$RHD)
data$`BP-LOW`<-as.numeric(data$`BP-LOW`)
data$Diabetes1<-as.factor(data$Diabetes1)
data$Diabetes2<-as.factor(data$Diabetes2)
data$hypertension1<-as.factor(data$hypertension1)
data$hypertension2<-as.factor(data$hypertension2)
data$hypertension3<-as.factor(data$hypertension3)
data$other<-as.factor(data$other)
data$GENDER<-as.factor(data$GENDER)
data$AGE<-as.numeric(data$AGE)

#Feature Engineering
#Age Group Bins
data$AGE_GROUP[data$AGE<=10]="Children"
data$AGE_GROUP[data$AGE>10 & data$AGE<=25]="Teenagers"
data$AGE_GROUP[data$AGE>26 & data$AGE<=50]="Adult"
data$AGE_GROUP[data$AGE>=50]="Elder"
data$AGE_GROUP<-as.factor(data$AGE_GROUP)
plot(data$AGE_GROUP, xlab= "Age", ylab = "Count", main = "Distribution of Age")


#Haemoglobin Bins
data$HB_LEVEL[data$HB<11]<-"Low or Abnormal"
data$HB_LEVEL[data$HB>=11]<-"Normal"
data$HB_LEVEL<-as.factor(data$HB_LEVEL)
plot(data$HB_LEVEL, xlab= "Haemoglobin Level", ylab = "Count", main = "Distribution of Haemoglobin")

#Urea Bins
data$UREA[data$UREA<=20]<-"Normal"
data$UREA[data$UREA>21]<-"Abormal"
data$UREA<-as.factor(data$UREA)
plot(data$UREA, xlab= "UREA Level", ylab = "Count", main = "Distribution of UREA")


#BMI
data$BMI[data$`BODY WEIGHT`/'^'((data$`BODY HEIGHT`/100),2)<=18.5]="Underweight"
data$BMI[data$`BODY WEIGHT`/'^'((data$`BODY HEIGHT`/100),2)>18.5 & data$`BODY WEIGHT`/'^'((data$`BODY HEIGHT`/100),2)<25]="Normal"
data$BMI[data$`BODY WEIGHT`/'^'((data$`BODY HEIGHT`/100),2)>=25 & data$`BODY WEIGHT`/'^'((data$`BODY HEIGHT`/100),2)<30]="Overweight"
data$BMI[data$`BODY WEIGHT`/'^'((data$`BODY HEIGHT`/100),2)>=30]="Obese"
data$BMI<-as.factor(data$BMI)
#REMOVING OUTLIER BMIs
data<-data[!(data$`BODY WEIGHT`/'^'((data$`BODY HEIGHT`/100),2)>100),]

plot(data$BMI, xlab= "BMI Levels", ylab = "Count", main = "Distribution of BMI")

#Blood pressure categories
data$BP_Cat[data$`BP -HIGH`<120 & data$`BP-LOW`<80]<- "Normal"
data$BP_Cat[(data$`BP -HIGH`>119 & data$`BP-LOW`<130) & data$`BP-LOW`<80]<- "Elevated"
data$BP_Cat[(data$`BP -HIGH`>129 & data$`BP -HIGH`<140) | (data$`BP-LOW`>79 & data$`BP-LOW`<90)]<- "Hypertension Stage 1"
data$BP_Cat[data$`BP -HIGH`>= 140 | data$`BP-LOW`>=90]<- "Hypertension Stage 2"
data$BP_Cat[data$`BP -HIGH`> 180 | data$`BP-LOW`>120]<- "Critical"
data$BP_Cat<-as.factor(data$BP_Cat)
plot(data$BP_Cat, xlab= "BP Levels", ylab = "Count", main = "Distribution of BP")

#CREATININE_LEVEL

data$CREATININE_LEVEL[data$AGE<=3 & data$CREATININE>=0.3 &data$CREATININE<=0.7 ]="Normal"
data$CREATININE_LEVEL[data$AGE>3 & data$AGE<=18 & data$CREATININE>=0.5 &data$CREATININE<=1.0 ]="Normal"
data$CREATININE_LEVEL[data$AGE>18 & data$FEMALE==1 & data$CREATININE>=0.6 &data$CREATININE<=1.1 ]="Normal"
data$CREATININE_LEVEL[data$AGE>18 & data$FEMALE==0 & data$CREATININE>=0.9 &data$CREATININE<=1.3 ]="Normal"
data$CREATININE_LEVEL[is.na(data$CREATININE_LEVEL)]<-"Abnormal"
data$CREATININE_LEVEL<-as.factor(data$CREATININE_LEVEL)
plot(data$CREATININE_LEVEL, xlab= "Creatinine Levels", ylab = "Count", main = "Distribution of Creatinine Levels")

data <- subset(data, select = -c(`BP -HIGH`, `BP-LOW`, CREATININE, `BODY WEIGHT`,`BODY HEIGHT`))
glimpse(data)
summary(data)

#library(dplyr)
#data<- as.data.frame(mutate(data, UREA_cat = ifelse(UREA %in% 7:20,"Normal","Abnormal" )))
#data$UREA_cat<-as.factor(data$UREA_cat)
#sum(is.na(data$UREA_cat))

#install.packages("visdat")
library(visdat)
vis_dat(data)


library(car) # for detailed correlation plot 
library(corrplot) # for correlation plot
#install.packages("Hmisc")
library(Hmisc) # for correlation test of multiple variables 

cnum <- data[,c( "TOTAL LENGTH OF STAY", "LENGTH OF STAY - ICU", "LENGTH OF STAY- WARD", "TOTAL COST TO HOSPITAL")]
cormat <- cor(cnum) # Select only numeric variables
corrplot(cormat, method="circle", addCoef.col="black") # With correlation

cnum <- data[,c("HR PULSE","RR","COST OF IMPLANT","TOTAL COST TO HOSPITAL")]
cormat <- cor(cnum) # Select only numeric variables
corrplot(cormat, method="circle", addCoef.col="black") # With correlation

sum(is.na(data))
data=na.omit(data)
summary(data)
is.null(data)

# TARGET VARIABLE

options(scipen = 999)
hist(data$`TOTAL COST TO HOSPITAL`, xlab = "Total Cost to Hospital (before Log)", main = "Distribution of Total Cost to Hospital")
summary(data$`TOTAL COST TO HOSPITAL`)
boxplot(data$`TOTAL COST TO HOSPITAL`, xlab = "Total Cost to Hospital (before Log)", main = "Distribution of Total Cost to Hospital")

hist(data$`Ln(Total Cost)`, xlab = "Total Cost to Hospital (after log)", main = "Distribution of Total Cost to Hospital")
boxplot(data$`Ln(Total Cost)`, xlab = "Total Cost to Hospital (after log)", main = "Distribution of Total Cost to Hospital")
#data$AGE_GROUP
sum(is.na(data$BP_cat))

#Significance Test
#anova
anova_oneway <- aov(data$`TOTAL LENGTH OF STAY`~data$`TOTAL COST TO HOSPITAL`, data = data)
summary(anova_oneway)
TukeyHSD(anova_oneway)

anova_oneway <- aov(data$GENDER~data$`TOTAL COST TO HOSPITAL`, data = data)
summary(anova_oneway)
TukeyHSD(anova_oneway)

#Model 1 Linear Regression
linear <- lm(data$`Ln(Total Cost)`~data$`BODY WEIGHT`)
summary.fit<- summary(linear)
summary.fit
