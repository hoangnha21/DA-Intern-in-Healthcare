library(dplyr)
install.packages("ggpcorrplot")
library(ggcorrplot)
library(ggplot2)
library(caret)
library(mice)
library(tidyr)
library(ROCR)
install.packages('varhandle')
library(varhandle)
casedata=read.csv('/Users/hoangnha218/Desktop/Chronic Kidney Disease Dataset.csv')
casedata1=casedata[,-c(1, 34)] # remove ID and disease status
str(casedata1)


#Check for Missing values
missing_values = casedata %>% summarize_all(funs(sum(is.na(.))/n())) # from package tidyr
missing_values = gather(missing_values, key="Variables", value="Percentages")
missing_values %>% 
  ggplot(aes(x=Variables,y=Percentages))+
  geom_bar(stat="identity",fill="blue")+
  ggtitle("Distribution of missing values by variables")+ 
  coord_flip()+theme_bw()

init = mice(casedata1, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("Smoker", "Racegrp", "PVD", "Female", "Fam.Hypertension", "Fam.Diabetes", "Dyslipidemia", "CareSource", "Age")]=""

meth[c("Educ", "Unmarried", "Insured", "Obese", "PoorVision", "Hypertension", 
       "Diabetes", "Stroke", "CVD", "Fam.CVD", "CHF", "Anemia", "Income")]="logreg" 
meth[c("Weight","Height", "BMI", "Waist", "SBP", "DBP", "HDL", "LDL", "Total.Chol")]="norm" 
meth[c("Activity")]="polyreg"

categorical = casedata1[,c("Income", "Educ", "Unmarried", "Insured", "Obese", "PoorVision", "Hypertension", "Diabetes", "Stroke", "CVD", "Fam.CVD", "CHF", "Anemia", "Smoker", "PVD", "Female", "Fam.Hypertension", "Fam.Diabetes", "Dyslipidemia", "Activity")]
categorical_var = lapply(categorical, as.factor)
categorical_var=as.data.frame(categorical_var)
casedata1 = cbind(casedata1[,c("Weight", "Height", "BMI", "SBP", "DBP", "Waist", "HDL", "LDL", "Total.Chol", "Racegrp", "CareSource", "Age")], categorical_var)

set.seed(1)
imputed = mice(casedata1, method=meth, predictorMatrix=predM, m = 5)
complete = mice::complete(imputed)
finaldata=mutate(complete, CKD=casedata$CKD)
sapply(finaldata, function(x) sum(is.na(x))) # Check the number of missing values after imputation
sapply(casedata, function(x) sum(is.na(x))) # Check number of missing values before imputation




