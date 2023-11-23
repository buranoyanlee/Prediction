data<-read.csv('Data.csv',header=T,row.names = 1)
colname<-names(data)
tail(colname)
n<-ncol(data)-2
library(survminer)
library(survival)
splots<-list()
pdf("HR.pdf")
data_hr<-data.frame(HR=rep(0,n),up95=rep(0,n),
                    low95=rep(0,n),pvalue=rep(0,n),cutpoint=rep(0,n))
for( i in 1:n){
   surv_cut<-surv_cutpoint(
    data,
    time="time",
    event = "status",
    variables = colname[i] 
   )
 summary(surv_cut)
   cut_point<- surv_cut[["cutpoint"]][["cutpoint"]]
   
   surv_cat<-surv_categorize(surv_cut)
   names(surv_cat)[3]<-"group"
 
  fit<-survfit(Surv(time,status)~group,surv_cat)
  splots[[i]]<-ggsurvplot(fit,
                          risk.table = TRUE,
                          pval = TRUE,
                          conf.int = TRUE,
                          legend.title =colname[i]
  )
  print(splots[[i]])
  data.survdiff<-survdiff(Surv(time, status) ~ group,data=surv_cat)
  p.val <- 1 - pchisq(data.survdiff$chisq, length(data.survdiff$n) - 1)
  HR <-(data.survdiff$obs[1]/data.survdiff$exp[1])/(data.survdiff$obs[2]/data.survdiff$exp[2])
  up95<- exp(log(HR) + qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
  low95<- exp(log(HR) - qnorm(0.975)*sqrt(1/data.survdiff$exp[2]+1/data.survdiff$exp[1]))
  
  data_hr[i,1]<-HR
  data_hr[i,2]<-up95
  data_hr[i,3]<-low95
  data_hr[i,4]<-p.val
  data_hr[i,5]<-cut_point
  row.names(data_hr)[i]<-colname[i]
}
dev.off()
write.csv(data_hr,file="Data_HR.csv")