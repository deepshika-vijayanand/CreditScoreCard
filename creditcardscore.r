1.	DATA

1.1	 IMPORT DATA INTO R
#load the tidyverse package
library(tidyverse)
#download data from github
Data= read.csv(“dataset.csv”)
#print first few lines of data
head(data)
#print out structure of the data
str(data)

1.2	 DATA EXPLORATION
library(Hmisc)
describe(data) 

1.3	 DATA MANIPULATION
data<-data5>%
#rename variables in lower case
rename_with(str_to_lower, everything())%>%
#rename “pay_0” to “pay_1” and “default payment next month” to “ gb_flag”
rename(“pay_1=”pay_0”,”gb_flag”=”default.payment.next.month”)
#set up a function to recoil delinquency status
new_deq<-function(x)
{
     ifelse(x %in% c(-2,-1), 0, x)
}
#recode delinquency status
dat<-data%>%
mutate(across (pay_1:pay_6, new_deq, .names=”{col}_recode”))
#check
table(data$pay_1, data$pay_1_recode)

1.4	  VARIABLE GENERATION
#deliquency status
#un-needed “pay_x” variables removed
var_data<-data%>%
select(-(pay_1:pay_6))
#identify position of variables
deq_pos_l3m<-which(str_detect(manes(var_data),”pay_(1|2|3)_recode”))
deq_pos_l6m<- which(str_detect(manes(var_data),”pay_(1|2|3|4|5|6)_recode”))
#generate new variables
var_data<-var_data%>%
mutate(
#average delinquency
avg_deq_lm3=apply(var_data[,deq_lm3], 1, mean, na.rm=true),
avg_deq_lm6=apply(var_data[,deq_lm6], 1, mean, na.rm=true),
#maximum delinquency
max_deq_lm3=apply(var_data[,deq_lm3], 1, max, na.rm=true),
max_deq_lm6=apply(var_data[,deq_lm6], 1, max, na.rm=true),
#minimum delinquency
min_deq_lm3=apply(var_data[,deq_lm3], 1, min, na.rm=true),
min_deq_lm6=apply(var_data[,deq_lm6], 1, min, na.rm=true)
)
#utilization rate
#function to compute utilization rate
uti_rate<-function(x)
{
     ifelse(x<0, 0, x/var_data$limit_bal)
}
#compute utilization rate
var_data<-var_data%>%
mutate(across(contains(“bill_amt”), .fns=list(util=uti_rate)))
#identify position of variable 
util_pos_l3m<-which(str_detect(names(var_data),”bill_amt(1|2|3)_util”))
util_pos_l6m<-which(str_detect(names(var_data),”bill_amt(1|2|3|4|5|6)_util”))
#generate new variables
var_data<-var_dat%>%
mutate(
#average utilization
avg_util_l3m=apply(var_data[, util_pos_l3m], 1, mean, na.rm=true),
avg_util_l6m=apply(var_data[, util_pos_l6m], 1, mean, na.rm=true),
#maximun utilization
max_util_l3m=apply(var_data[, util_pos_l3m], 1, max, na.rm=true),
max_util_l6m=apply(var_data[, util_pos_l6m], 1, max, na.rm=true),
#minimum utilization
min_util_l3m=apply(var_data[, util_pos_l3m], 1, min, na.rm=true),
min_util_l6m=apply(var_data[, util_pos_l6m], 1, min, na.rm=true)
)

1.5	 SAMPLING
#set seed for random sample
set.seed(1234)
#randomly select from id column
dev_ind<-sample(var_data$id, 24000, replace=false)
#use random select id to form development sample
dev<-var_data[dev_ind,]
#use non select id to form validation sample
oot<-var_data[-dev_ind,]

2. UNIVARIATE ANALYSIS
2.1	 FINE CLASSING
library(scorecard)
#select all variables for computation
var_list<-dev%>%
select(-id, -gb_flag)%>%
names()
#invoke woebin function in package scorecard
fine_class<-woebin(
dev,
y= ”gb_flag”,
x= var_list,
positive= 1,
method= ”freq”,
bin_num_limit= 20
)
#collection of variables
iv<-map_df(fine_class, ~pluck(.x, 10, 1))%>%
pivot_longer(everything(), names_to=”var”, values_to=”iv”)

2.2	 INITAIL VARIABLES REMOVAL
#keep relavant variables
dev<-dev%>%
select(id, age, gb_flag:max_util_l6m, avg_deq_l3m:min_deq_l3m, bill_amt6_util)

2.3	 VARIABLE CLUSTERING
library(ClusOfVar)
#perform variable clustering
tree<-dev%>%
select(-id, -gb_flag)%>%
hclustvar()
#stability plot
set.seed(345)
stab<-stability(tree, B=30)
boxplot(stab$matCR, main= “dispersion of the adjusted rand index)
#generate the final cluster output
clus<-cutree(tree, 4)

3. COARSE CLASSING
plot<-woebin_plot(fine_class_final)
plot[[1]]
plot[[2]]
plot[[3]]
plot[[4]]
plot[[5]]
plot[[6]]
plot[[7]]
breaks_list<-list(
age= c(“25”, “45”),
avd_deq_l3m= c(“0.67”, “2”),
pay_1_recode= c(“1”, “2”),
pay_4_recode= c(“1”),
pay_5_recode= c(“2”),
avg_util_l6m= c(“0.45”, “0.83”),
max_util_l6m= c(“o,43”, “1”)
)
#set positive= 0
coarse_class<-woebin(
dev,
y= “gb_flag”,
x= short_var_list,
positive= 0,method= “freq”,
break_list= break_list
)
#transform variable values to woe values
dev_woe<-woebin_ply(dev, coarse_class)

4. REGRESSION ANALYSIS
#logistic regression
logistic<-glm(
I(gb_flag==0)~.,
family= binomial(),
data= dev_woe%>% select(-id)
)
#setwise regression
logistic_step<-step(logistic, direction= “both”, trace= false)
#print output
summary(logistic_step)
#generate VIF
vif(logistic_step, merge_coef= true)

5. SCORECARD CREATION, SCALING NAD VALIDATION
#select variables in final regression
dev_final<- dev%>%
select (id, age, gb_flag, avg_deq_l3m, pay_1_recode, pay_4_recode, pay_5_recode, avg_util_l6m)
var_select<-c(“age”, “avg_deq_l3m”, “pay_1_recode”, “pay_4_recode”, “pay_5_recode”, “avg_util_l6m”)
break_list<-list(
age= c(“25”, “45”),
avd_deq_l3m= c(“0.67”, “2”),
pay_1_recode= c(“1”, “2”),
pay_4_recode= c(“1”),
pay_5_recode= c(“2”),
avg_util_l6m= c(“0.45”, “0.83”)
)
bins<-woebin(
dev_final,
y= “gb_flag),
x= var_select,
positive= 0,
method= “freq”,
breaks_list= breaks_list
)
score_card<-score_card(
bins,
logistic_step,
points0= 500,
pdo= -30,
oods0= 100
basepoints_eq0= true
) 
#display results
score_card 
#compute score
score<-scorecard_ply(dev_final, score_card, only_total_score= f)
#validation
#select variables from oot sample
oot_final<-oot%>%
select(id, age, gb_flag, avg_deq_l3m, pay_1_recode, pay_4_recode, pay_5_recode, avg_util_l6m)
#generate reports
#report(
list(dt1= dev_final, dt_2= oot_final),
y= “gb_flag”,
x= var_select,
breaks_list= break_list,
seed= nul,
basepoints_eq0= true,
method= “freq”,
positive= 0,
points0= 500,
oods0= 100,
pdo= -30
)
