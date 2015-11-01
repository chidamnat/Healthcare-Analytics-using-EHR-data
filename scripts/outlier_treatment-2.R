setwd('/Users/Chidam/Downloads/Practice Fusion')
df<-final

df[,c("Weight","Height","BMI","Temperature","DiastolicBP","HeartRate","RespiratoryRate","SystolicBP")]<-sapply(df[,c("Weight","Height","BMI","Temperature","DiastolicBP","HeartRate","RespiratoryRate","SystolicBP")],as.numeric)
class(df$Temperature)
#x<-as.data.frame(as.matrix(x))  
#as.numeric(df[,c("Weight","Height","BMI","Temperature","DiastolicBP","HeartRate","RespiratoryRate","SystolicBP")])
df_1<-subset(df,select=c("Weight","Height","BMI","Temperature","DiastolicBP","HeartRate","RespiratoryRate","SystolicBP"))
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
write.csv(dataFilt,file='dataFilt.csv')

