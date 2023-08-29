library(dplyr)
library(ggcorrplot)
library(ggplot2)
library(caret)
library(mice)
library(tidyr)
library(ROCR)
library(varhandle)
## Step 0  - Read in Data
data=read.csv('/Users/hoangnha218/Desktop/Chronic Kidney Disease Dataset.csv')
names(data)
dim(data)

data=data[,-1,34]  ## remove ID

class(data)
summary(data)

out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
summary(data_in)

?na.omit
data_in=na.omit(data_in)
summary(data_in)

library(mice)
library(VIM)
library(lattice)
library(ggplot2)

dat=read.csv('/Users/hoangnha218/Desktop/Chronic Kidney Disease Dataset.csv')
summary(dat)
names(dat)
dat<- dat[,-c(1,34)]
head(dat)
sapply(dat, function(x) sum(is.na(x)))
library(dplyr) 
dat <- dat %>%
  mutate(
    Unmarried = as.factor(dat$Unmarried),
    Income = as.factor(dat$Income),
    Educ = as.factor(dat$Educ),
    Insured = as.factor(dat$Insured),
    PoorVision = as.factor(dat$PoorVision),
    Hypertension = as.factor(dat$Hypertension),
    Obese = as.factor(dat$Obese),
    Diabetes = as.factor(dat$Diabetes),
    Stroke = as.factor(dat$Stroke),
    CVD = as.factor(dat$CVD),
    Fam.CVD = as.factor(dat$Fam.CVD),
    CHF = as.factor(dat$CHF),
    Anemia = as.factor(dat$Anemia),
    
    SBP = as.numeric(dat$SBP),
    DBP = as.numeric(dat$DBP),
    Weight = as.numeric(dat$Weight),
    Height = as.numeric(dat$Height),
    BMI = as.numeric(dat$BMI),
    Waist = as.numeric(dat$Waist),
    HDL = as.numeric(dat$HDL),
    LDL = as.numeric(dat$LDL),
    TotalChol = as.numeric(dat$Total.Chol),
    Activity = as.numeric(dat$Activity)
  )
    
    
  
str(dat)
library(mice)
init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix
#predM[, c("Racegrp")]=0
meth[c("SBP")]="norm" 
meth[c("DBP")]="norm" 
meth[c("Weight")]="norm"
meth[c("Height")]="norm"
meth[c("BMI")]="norm"
meth[c("Waist")]="norm"
meth[c("HDL")]="norm"
meth[c("LDL")]="norm"
meth[c("TotalChol")]="norm"
meth[c("Activity")]="polyreg"
meth[c("Unmarried")]="logreg" 
meth[c("Income")]="logreg"
meth[c("Educ")]="logreg"
meth[c("Insured")]="logreg"
meth[c("Obese")]="logreg"
meth[c("PoorVision")]="logreg"
meth[c("Hypertension")]="logreg"
meth[c("Diabetes")]="logreg"
meth[c("Stroke")]="logreg"
meth[c("CVD")]="logreg"
meth[c("Fam.CVD")]="logreg"
meth[c("CHF")]="logreg"
meth[c("CVD")]="logreg" 
meth[c("Anemia")]="logreg"
set.seed(103)
imputed = mice(dat, method=meth, predictorMatrix=predM, m=5)
imputed <- complete(imputed)

write.csv(imputed,"imputed.csv")
summary(dat)
summary(imputed)
sapply(imputed, function(x) sum(is.na(x)))
sapply(dat, function(x) sum(is.na(x)))

