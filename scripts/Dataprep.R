####################
#Set the data path
####################
getwd()
path="/Users/Chidam/Downloads/Practice Fusion"
setwd(path)
getwd()

####################
#Generic function to load the datasets
#Loads the datasets 
#inputs filename
####################
funcLoad<-function(filename){
  print(filename)
  read.csv(filename,stringsAsFactors=FALSE)
}
 
####################
#Load the datasets
#Allergy - Factors (AllergyType,ReactionName,SeverityName)
#Condition
#Diagnosis
####################
#createDF<-function(){
Allergy=funcLoad('test_SyncAllergy.csv')
Allergy$AllergyType=as.factor(Allergy$AllergyType)
Allergy$ReactionName=as.factor(Allergy$ReactionName)
Allergy$SeverityName=as.factor(Allergy$SeverityName)
Condition=funcLoad('SyncCondition.csv')
Diagnosis=funcLoad('test_SyncDiagnosis.csv')
Diagnosis$Acute=as.factor(Diagnosis$Acute)
Immunization=funcLoad('test_SyncImmunization.csv')
LabObservation=funcLoad('test_SyncLabObservation.csv')
LabObservation$HL7Text=as.factor(LabObservation$HL7Text)
LabObservation$HL7CodingSystem=as.factor(LabObservation$HL7CodingSystem)
LabObservation$AbnormalFlags=as.factor(LabObservation$AbnormalFlags)
LabObservation$ResultStatus=as.factor(LabObservation$ResultStatus)
LabObservation$IsAbnormalValue=as.factor(LabObservation$IsAbnormalValue)
LabPanel=funcLoad('test_SyncLabPanel.csv')
LabPanel$Status=as.factor(LabPanel$Status)
LabResult=funcLoad('test_SyncLabResult.csv')
Medication=funcLoad('test_SyncMedication.csv')
Patient=funcLoad('test_SyncPatient.csv')
Patient$Gender=as.factor(Patient$Gender)
Patient$State=as.factor(Patient$State)
PatientCondition=funcLoad('test_SyncPatientCondition.csv')
PatientSmokingStatus=funcLoad('test_SyncPatientSmokingStatus.csv')
Prescription=funcLoad('test_SyncPrescription.csv')
Prescription$RefillAsNeeded=as.factor(Prescription$RefillAsNeeded)
Prescription$GenericAllowed=as.factor(Prescription$GenericAllowed)
SmokingStatus=funcLoad('SyncSmokingStatus.csv')
SmokingStatus$NISTcode=as.factor(SmokingStatus$NISTcode)
Transcript=funcLoad('test_SyncTranscript.csv')
TranscriptAllergy=funcLoad('test_SyncTranscriptAllergy.csv')
TranscriptDiagnosis=funcLoad('test_SyncTranscriptDiagnosis.csv')
TranscriptMedication=funcLoad('test_SyncTranscriptMedication.csv')
#}
#createDF()

#fix_Transcript<-function(){
  library(doBy)
  #Identifying the best value for weight
  TranscriptSummary<-summaryBy(Weight~PatientGuid,data=Transcript,FUN=list(min,max,mean,median,sd))
  nrow(TranscriptSummary[which(TranscriptSummary$Weight.median==0.0),])
  #1401
  TranscriptSummary$Weightsel=ifelse(TranscriptSummary$Weight.median==0.0,TranscriptSummary$Weight.max,TranscriptSummary$Weight.median)
  TranscriptSummary=TranscriptSummary[,c(1,7)]
  Transcript_new=merge(Transcript,TranscriptSummary,byintersect(names(Transcript),names(TranscriptSummary)),by.x='PatientGuid',by.y='PatientGuid')
  Transcript_new$Weight=ifelse(Transcript_new$Weight==0.0,Transcript_new$Weightsel,Transcript_new$Weight)
  Transcript=Transcript_new
  
  #Identifying the best value for Height
  rm(TranscriptSummary)
  Transcript$Height=as.numeric(Transcript$Height)
  TranscriptSummary<-summaryBy(Height~PatientGuid,data=Transcript,FUN=list(min,max,mean,median,sd),na.rm=TRUE)
  nrow(TranscriptSummary[which(TranscriptSummary$Height.median=="NULL"),])
  #0
  nrow(TranscriptSummary[which(is.na(TranscriptSummary$Height.median)),])
  #0
  nrow(TranscriptSummary[which(TranscriptSummary$Height.median=="NULL" | is.na(TranscriptSummary$Height.median)),])
  #0
  TranscriptSummary$Heightsel=ifelse(TranscriptSummary$Height.median=="NULL" | is.na(TranscriptSummary$Height.median) ,TranscriptSummary$Height.min,TranscriptSummary$Height.median)
  TranscriptSummary=TranscriptSummary[,c(1,7)]
  Transcript_new=merge(Transcript,TranscriptSummary,byintersect(names(Transcript),names(TranscriptSummary)),by.x='PatientGuid',by.y='PatientGuid')
  Transcript_new$Height=ifelse(is.na(Transcript_new$Height),Transcript_new$Heightsel,Transcript_new$Height)
  Transcript=Transcript_new
  
  #Identifying the best value for BMI
  rm(TranscriptSummary)
  Transcript$BMI=as.numeric(Transcript$BMI)
  TranscriptSummary<-summaryBy(BMI~PatientGuid,data=Transcript,FUN=list(min,max,mean,median,sd),na.rm=TRUE)
  nrow(TranscriptSummary[which(TranscriptSummary$BMI.median=="NULL" | is.na(TranscriptSummary$BMI.median)),])
  #0
  TranscriptSummary$BMIsel=ifelse(TranscriptSummary$BMI.median=="NULL" | is.na(TranscriptSummary$BMI.median) ,TranscriptSummary$BMI.min,TranscriptSummary$BMI.median)
  TranscriptSummary=TranscriptSummary[,c(1,7)]
  Transcript_new=merge(Transcript,TranscriptSummary,byintersect(names(Transcript),names(TranscriptSummary)),by.x='PatientGuid',by.y='PatientGuid')
  Transcript_new$BMI=ifelse(is.na(Transcript_new$BMI),Transcript_new$BMIsel,Transcript_new$BMI)
  Transcript=Transcript_new

  #Identifying the best value for SystolicBP
  rm(TranscriptSummary)
  #Transcript$SystolicBP=as.numeric(Transcript$SystolicBP)
  TranscriptSummary<-summaryBy(SystolicBP~PatientGuid,data=Transcript,FUN=list(min,max,mean,median,sd),na.rm=TRUE)
  nrow(TranscriptSummary[which(TranscriptSummary$SystolicBP.median==0.0 | is.na(TranscriptSummary$SystolicBP.median)),])
  #943
  TranscriptSummary$SystolicBPsel=ifelse(TranscriptSummary$SystolicBP.median==0.0 | is.na(TranscriptSummary$SystolicBP.median) ,TranscriptSummary$SystolicBP.max,TranscriptSummary$SystolicBP.median)
  TranscriptSummary=TranscriptSummary[,c(1,7)]
  Transcript_new=merge(Transcript,TranscriptSummary,byintersect(names(Transcript),names(TranscriptSummary)),by.x='PatientGuid',by.y='PatientGuid')
  Transcript_new$SystolicBP=ifelse(Transcript_new$SystolicBP==0.0,Transcript_new$SystolicBPsel,Transcript_new$SystolicBP)
  Transcript=Transcript_new
  
  #Identifying the best value for DiastolicBP
  rm(TranscriptSummary)
  TranscriptSummary<-summaryBy(DiastolicBP~PatientGuid,data=Transcript,FUN=list(min,max,mean,median,sd),na.rm=TRUE)
  nrow(TranscriptSummary[which(TranscriptSummary$DiastolicBP.median==0.0 | is.na(TranscriptSummary$DiastolicBP.median)),])
  #943
  TranscriptSummary$DiastolicBPsel=ifelse(TranscriptSummary$DiastolicBP.median==0.0 | is.na(TranscriptSummary$DiastolicBP.median) ,TranscriptSummary$DiastolicBP.max,TranscriptSummary$DiastolicBP.median)
  TranscriptSummary=TranscriptSummary[,c(1,7)]
  Transcript_new=merge(Transcript,TranscriptSummary,byintersect(names(Transcript),names(TranscriptSummary)),by.x='PatientGuid',by.y='PatientGuid')
  Transcript_new$DiastolicBP=ifelse(Transcript_new$DiastolicBP==0.0,Transcript_new$DiastolicBPsel,Transcript_new$DiastolicBP)
  Transcript=Transcript_new
  
  #Identifying the best value for RespiratoryRate
  rm(TranscriptSummary)
  Transcript$RespiratoryRate=as.numeric(Transcript$RespiratoryRate)
  TranscriptSummary<-summaryBy(RespiratoryRate~PatientGuid,data=Transcript,FUN=list(min,max,mean,median,sd),na.rm=TRUE)
  nrow(TranscriptSummary[which(TranscriptSummary$RespiratoryRate.median=="NULL" | is.na(TranscriptSummary$RespiratoryRate.median | TranscriptSummary$RespiratoryRate.median=="Inf"| TranscriptSummary$RespiratoryRate.median=="-Inf"  )),])
  #1439
  #Retaining the NAs frm median of RespiratoryRate as min, max are bad
  #TranscriptSummary$RespiratoryRatesel=ifelse(TranscriptSummary$RespiratoryRate.median=="NULL" | is.na(TranscriptSummary$RespiratoryRate.median) ,TranscriptSummary$RespiratoryRate.median,TranscriptSummary$RespiratoryRate.median)
  TranscriptSummary$RespiratoryRatesel=TranscriptSummary$RespiratoryRate.median
  TranscriptSummary=TranscriptSummary[,c(1,7)]
  Transcript_new=merge(Transcript,TranscriptSummary,byintersect(names(Transcript),names(TranscriptSummary)),by.x='PatientGuid',by.y='PatientGuid')
  Transcript_new$RespiratoryRate=ifelse(is.na(Transcript_new$RespiratoryRate),Transcript_new$RespiratoryRatesel,Transcript_new$RespiratoryRate)
  Transcript=Transcript_new
#}

#Build_Dataset<-function(){
#Add new derived feature ICD9root based on ICD9Code as below in Diagnosis DF
Diagnosis$ICD9root=substr(Diagnosis$ICD9Code,1,3)
# Add a new feature ICD9class based on the ICD9root in Diagnosis DF
Diagnosis$ICD9class[Diagnosis$ICD9root>"000" & Diagnosis$ICD9root < "140"]="Infectious Parasite Diseases"
Diagnosis$ICD9class[Diagnosis$ICD9root>="140" & Diagnosis$ICD9root < "240"]="neoplasms" 
Diagnosis$ICD9class[Diagnosis$ICD9root>="240" & Diagnosis$ICD9root < "280"]="endocrine, nutritional and metabolic diseases, and immunity disorders"
Diagnosis$ICD9class[Diagnosis$ICD9root>="280" & Diagnosis$ICD9root < "290"]="diseases of the blood and blood-forming organs"
Diagnosis$ICD9class[Diagnosis$ICD9root>="290" & Diagnosis$ICD9root < "320"]="mental disorders"
Diagnosis$ICD9class[Diagnosis$ICD9root>="320" & Diagnosis$ICD9root < "360"]="diseases of the nervous system"
Diagnosis$ICD9class[Diagnosis$ICD9root>="360" & Diagnosis$ICD9root < "390"]="diseases of the sense organs"
Diagnosis$ICD9class[Diagnosis$ICD9root>="390" & Diagnosis$ICD9root < "460"]="diseases of the circulatory system"
Diagnosis$ICD9class[Diagnosis$ICD9root>="460" & Diagnosis$ICD9root < "520"]="diseases of the respiratory system"
Diagnosis$ICD9class[Diagnosis$ICD9root>="520" & Diagnosis$ICD9root < "580"]="diseases of the digestive system"
Diagnosis$ICD9class[Diagnosis$ICD9root>="580" & Diagnosis$ICD9root < "630"]="diseases of the genitourinary system"
Diagnosis$ICD9class[Diagnosis$ICD9root>="630" & Diagnosis$ICD9root < "680"]="complications of pregnancy, childbirth, and the puerperium"
Diagnosis$ICD9class[Diagnosis$ICD9root>="680" & Diagnosis$ICD9root < "710"]="diseases of the skin and subcutaneous tissue"
Diagnosis$ICD9class[Diagnosis$ICD9root>="710" & Diagnosis$ICD9root < "740"]="diseases of the musculoskeletal system and connective tissue"
Diagnosis$ICD9class[Diagnosis$ICD9root>="740" & Diagnosis$ICD9root < "760"]="congenital anomalies"
Diagnosis$ICD9class[Diagnosis$ICD9root>="760" & Diagnosis$ICD9root < "780"]="certain conditions originating in the perinatal period"
Diagnosis$ICD9class[Diagnosis$ICD9root>="780" & Diagnosis$ICD9root < "800"]="symptoms, signs, and ill-defined conditions"
Diagnosis$ICD9class[Diagnosis$ICD9root>="800" & Diagnosis$ICD9root <= "999"]="injury and poisoning"
Diagnosis$ICD9class[Diagnosis$ICD9root>="E00" & Diagnosis$ICD9root < "V99"]="external causes of injury and supplemental classification"
Diagnosis$ICD9root=as.factor(Diagnosis$ICD9root)

#merge Patient & Diagnosis
Patient_Diagnosis=merge(Patient,Diagnosis,by=intersect(names(Patient), names(Diagnosis)),by.x='PatientGuid',by.y='PatientGuid')

#fix_Transcript()
#merge Patient_Diagnosis and Transcript
Patient_Diagnosis_Transcript=merge(Patient_Diagnosis,Transcript,by=intersect(names(Patient_Diagnosis), names(Transcript)),by.x='PatientGuid',by.y='PatientGuid')

#merge Patient_Diagnosis_Transcript and Allergy
Patient_Diagnosis_Transcript_Allergy=merge(Patient_Diagnosis_Transcript,Allergy,by=intersect(names(Patient_Diagnosis_Transcript), names(Allergy)),by.x='PatientGuid',by.y='PatientGuid',all.x=TRUE)

#merge Patient_Diagnosis_Transcript_Allergy and SmokingStatus
Patient_Diagnosis_Transcript_Allergy_SmokingStatus=merge(Patient_Diagnosis_Transcript_Allergy,PatientSmokingStatus,by=intersect(names(Patient_Diagnosis_Transcript_Allergy), names(PatientSmokingStatus)),by.x='PatientGuid',by.y='PatientGuid',all.x=TRUE)

#merge Patient_Diagnosis_Transcript_Allergy_SmokingStatus with Smoking status
Patient_Diagnosis_Transcript_Allergy_SmokingStatus_Smoking=merge(Patient_Diagnosis_Transcript_Allergy_SmokingStatus,SmokingStatus,by=intersect(names(Patient_Diagnosis_Transcript_Allergy_SmokingStatus), names(SmokingStatus)),by.x='SmokingStatusGuid',by.y='SmokingStatusGuid',all.x=TRUE)

final=Patient_Diagnosis_Transcript_Allergy_SmokingStatus_Smoking
final$age=2010-as.numeric(final$YearOfBirth)
final$YearOfBirth=NULL
#}
#Build_Dataset()
write.csv(final,file='data_prep_final.csv')







