require(data.table)
require(xlsx)
require(plyr)
require(ggplot2)
setwd("~/Documents/Sean/work/coding/lendingclub")
data.dict<-read.xlsx('LCDataDictionary.xlsx',sheetName='LoanStats',endRow=57,colIndex=1:4)

loansa<-fread('LoanStats3a.csv',skip = 1,nrows=39786)
loansb<-fread('LoanStats3b.csv',skip = 1,nrows=188123)
loans<-rbindlist(list(loansa,loansb))
colnames(loans)
# check types
numeric_types<-c("loan_amnt","funded_amnt","funded_amnt_inv","installment","annual_inc","dti",
                 "delinq_2yrs", "inq_last_6mths", "mths_since_last_delinq",
                 "mths_since_last_record", "open_acc", "pub_rec",
                 "revol_bal", "total_acc", "out_prncp",
                 "out_prncp_inv", "total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee",
                 "recoveries", "collection_recovery_fee", "last_pymnt_amnt",  "collections_12_mths_ex_med",
                 "mths_since_last_major_derog"  )
perc_types<-c("int_rate","revol_util")
factor_types<-c("grade","sub_grade" ,"emp_title","emp_length","home_ownership","is_inc_v", "loan_status" ,"desc",
                "purpose","title","zip_code","addr_state","initial_list_status" ,"policy_code")



data.dict[numeric_types,'type']='numeric'
data.dict[perc_types,'type']='numeric'
data.dict[factor_types,'type']='factor'
data.dict['term','type']='factor'
# "last_pymnt_d" , "next_pymnt_d", "last_credit_pull_d"
for (col in numeric_types) set(loans, j=col, value=as.numeric(loans[[col]])) 

for (col in perc_types) set(loans, j=col, value=as.numeric(as.numeric(substr(loans[[col]],2,6))/100)) 
loans$term<-as.numeric(substr(loans$term,2,3)) # " 36 Months"


factor_try<-c("grade","sub_grade" ,"emp_title","emp_length","home_ownership","is_inc_v")
for (col in factor_types) set(loans, j=col, value=as.factor(loans[[col]]))
lev<- c("n/a","< 1 year",  "1 year", "2 years",   "3 years",   "4 years",   "5 years",   "6 years",   "7 years"  , "8 years", "9 years", "10+ years")

set(loans, j='emp_length', value=ordered(loans$emp_length,levels=lev))
isfac<-sapply(loans,is.factor)
factor_names<-names(isfac[isfac==TRUE])



loans_summary<-lapply(factor_try,function (f) loans[,c(N=.N, as.list(summary(int_rate))),keyby=f])

loans_summary_boxplots<-lapply(factor_try,function (f) {
ggplot(loans,aes_string(x=f,y='int_rate'))+geom_boxplot()})


z1<- loans_summary[3][[1]][order(Mean),][N>100][1:30]


z1p<-ggplot(z1,aes(y=reorder(emp_title,Mean),x=Mean))+geom_point()


#loans$int_rate<-as.numeric(substr(loans$int_rate,2,6))/100
#loans$revol_util<-as.numeric(substr(loans$revol_util,2,6))/100

z<-loans[,sum(loan_amnt),by=c('term','grade')]
install.packages(dplyr)
z2<-dcast.data.table(z,grade~term)

cs<-c('emp_length','home_ownership',"installment")
z3<-lapply(cs,function (c1){loans[,list(dim=c1,n=.N, tot=sum(loan_amnt)),by=c1]})
z4<-rbindlist(z3)
setnames(z4,cs[1],'value')
setcolorder(z4,c('dim','value','n','tot'))

