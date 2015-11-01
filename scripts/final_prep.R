require(sqldf)
library(doBy)
#library(kmeans)
final_prep=NULL

final_prep_PatientGuid=unique(final$PatientGuid)
final_prep_Height=summaryBy(Height~PatientGuid,data=Transcript,FUN=median)
final_prep_Weight=summaryBy(Weight~PatientGuid,data=Transcript,FUN=median)
final_prep_BMI=summaryBy(BMI~PatientGuid,data=Transcript,FUN=median)
final_prep_SystolicBP=summaryBy(SystolicBP~PatientGuid,data=Transcript,FUN=median)
final_prep_DiastolicBP=summaryBy(DiastolicBP~PatientGuid,data=Transcript,FUN=median)
final_prep_Age=summaryBy(age~PatientGuid,data=final,FUN=median)
final_prep_disease_count=data.frame(table(Diagnosis$PatientGuid))
names(final_prep_disease_count)=c('PatientGuid','Disease_Count')
final_prep_medication_count=data.frame(table(Medication$PatientGuid))
names(final_prep_medication_count)=c('PatientGuid','Medication_Count')
final_prep_lab_test_count=data.frame(table(LabResult$PatientGuid))
names(final_prep_lab_test_count)=c('PatientGuid','lab_test_count')
final_prep_allergy_count=data.frame(table(Allergy$PatientGuid))
names(final_prep_allergy_count)=c('PatientGuid','allergy_count')
#final_prep_acute_count=data.frame(table(Diagnosis$PatientGuid))
#names(final_prep_acute_count)=c('PatientGuid','Acute')
Diagnosis1=Diagnosis[,c(2,7)]
Diagnosis1$Acute=as.numeric(Diagnosis1$Acute)
final_prep_acute_count=summaryBy(Acute~PatientGuid,data=Diagnosis,FUN=min)

final_prep=sqldf("SELECT * FROM final_prep_Height JOIN final_prep_Weight USING(PatientGuid) JOIN 
                 final_prep_BMI USING(PatientGuid) JOIN 
                 final_prep_SystolicBP USING(PatientGuid) JOIN
                 final_prep_DiastolicBP USING(PatientGuid) JOIN
                 final_prep_Age USING(PatientGuid) LEFT JOIN
                 final_prep_disease_count USING(PatientGuid) LEFT JOIN
                 final_prep_medication_count USING(PatientGuid) LEFT JOIN
                 final_prep_lab_test_count USING(PatientGuid) LEFT JOIN
                 final_prep_allergy_count USING(PatientGuid) LEFT JOIN
                 final_prep_acute_count USING(PatientGuid)")
#fixing the cutoff for overprescription as 6
final_prep$OverPres=NULL
final_prep$OverPres[final_prep$Medication_Count>10]=1
final_prep$OverPres[final_prep$Medication_Count<=10]=0
final_prep[is.na(final_prep)]<-0
df_1<-final_prep[2:7]
####
findOutlier <- function(data, cutoff = 3) {
  ## Calculate the sd
  sds <- apply(data, 2, sd, na.rm = TRUE)
  mns <- apply(data, 2, mean, na.rm=TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  result <- mapply(function(d, s, m) {
    which(d >  m + (cutoff * s) | d < m - (cutoff *s) )
  }, data, sds,mns)
  result
}
outliers <- findOutlier(df_1)

removeOutlier <- function(data, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <- NA
    return(res)
  }, data, outliers)
  return(as.data.frame(result))
}

dataFilt <- removeOutlier(df_1, outliers)

final_prep1=data.frame(final_prep[,c(1,8,9,10,11,12)],dataFilt)
final_prep2=na.omit(final_prep1)


#####


#corr_out=cor(corr_input_DF,na.rm=TRUE)
write.csv(final_prep,file='data_prep_final.csv')
final_prep_over=subset(final_prep,OverPres==1)

#k-means clustering
matrix1<-as.matrix(final_prep2)
#summary(corr_final_DF1)
#matrix1<-as.matrix(na.omit(corr_final_DF1))
fit <- kmeans(matrix1[,-c(1,13)],2) 
summary(fit)
#plot(fit)
aggregate(matrix1,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(matrix1, fit$cluster)
library(cluster)
library(HSAUR)
#data(pottery)
#km    <- kmeans(pottery,3)
dissE <- daisy(mydata) 
dE2   <- dissE^2
sk2   <- silhouette(fit$cl, dE2)
plot(sk2)
write.csv(final_prep2,file='data_prep_final.csv')
#fit2=glm(final_prep_over)
