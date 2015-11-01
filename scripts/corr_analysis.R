#include dummies package for creating dummy variables
library(dummies)
##We have to do some tweaks to get the correct o/p from outlier treatment and
## and non numeric attribs
final$Weight=NULL
final$Height=NULL
final$BMI=NULL
final$Temperature=NULL
final$DiastolicBP=NULL
final$HeartRate=NULL
final$RespiratoryRate=NULL
final$SystolicBP=NULL

#After eliminating outliers
final$Weight=dataFilt$Weight
final$Height=dataFilt$Height
final$BMI=dataFilt$BMI
final$Temperature=dataFilt$Temperature
final$DiastolicBP=dataFilt$DiastolicBP
final$HeartRate=dataFilt$HeartRate
final$RespiratoryRate=dataFilt$RespiratoryRate
final$SystolicBP=dataFilt$SystolicBP

# Identifying the numeric features
final$ICD9class=as.factor(final$ICD9class)
final$MedicationNDCCode=NULL
numeric_features<-sapply(final,is.numeric)
category_features<-sapply(final,is.factor)
# Build a new DF with only these numeric input features
corr_input_DF_numeric=final[,numeric_features]
#Standardize numeric data by scaling (Standard Normalizing)
scaled_corr_DF_numeric=scale(corr_input_DF_numeric)

#Handling categorical features
corr_input_DF_category=final[,category_features]
#Creating dummy variables
# to skip ICD9class (Target Variable) we may need to exclude index 5 from dummy1
dummy1<-dummy.data.frame(corr_input_DF_category[c(1:3,5:6)])
dummy2<-dummy.data.frame(corr_input_DF_category[c(7:9)])

#binding all DFs together
final_out=cbind(scaled_corr_DF_numeric,dummy1,dummy2)
# displaying the structure of the final DF
str(final_out)

#Identify the missing value attributes
missing_value_attributes=sapply(final_out,anyNA)
#Eliminating the features with missing value 
corr_input_DF=final_out[!missing_value_attributes]
corr_miss_DF=final_out[missing_value_attributes]
str(corr_input_DF)

#Calling the correlation function
corr_out=cor(corr_input_DF,na.rm=TRUE)
#displaying the corr_out
write.csv(corr_out,file='corr_out.csv')
corr_out
corr_final=corr_out
corr_final[abs(corr_final)<0.5]<-NA
write.csv(corr_final,file='corr_final.csv')

#correlation output analysis
library(caret) # required for findCorrelation function
corr_mat=as.matrix(corr_out,dim(corr_out))
#Identifying highly correlated variables with correltion threshold > 0.6
HighCorr <- findCorrelation(corr_out, cutoff=0.50)
HighCorr
#skipping higher correlated multicollinear attributes 
corr_final_DF=corr_input_DF[-HighCorr]
corr_final_out=cor(corr_final_DF)
#Reverifying after removal of the muticollinear features
corr_final_out[abs(corr_final_out)<0.5]<-NA
write.csv(corr_final_out,file='corr_final_out.csv')
corr_final_DF1=cbind(corr_final_DF,corr_miss_DF)
#list of features considered based on the applied threshold of 0.5
corr_final_DF1$HeartRate=NULL
corr_final_DF1$StartYear.y=NULL
corr_final_DF1$EffectiveYear=NULL
names(corr_final_DF1)
write.csv(corr_final_DF1,file='corr_final_DF1.csv')
