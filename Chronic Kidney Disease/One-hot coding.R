#one hot coding
dv = dummyVars(~ Racegrp+CareSource, data = finaldata, sep = ".", fullRank = TRUE) 
nv = as.data.frame(predict(dv, newdata = fulldata))

finaldata=finaldata[,-c(10, 11)]
finaldata=cbind(finaldata, nv)

fd=subset(fulldata, is.na(CKD)==FALSE)
test=subset(fulldata, is.na(CKD)==TRUE)
dim(fd)