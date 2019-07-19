{
  setwd("C:/Users/swaroop.mondal/Documents/R-Studio")
library("tidyverse")
library("caret")
library("car")
library("caTools")
library("corrplot")
library("ROSE")
library("rpart")
library("rpart.plot")
library("RColorBrewer")
library("e1071")
library(rattle)
library("CatEncoders")
library(dplyr)
library(ggplot2)
library(purrr)
library(lubridate)
library(stringr)
library(tidyr)
library(readr)
library(vtreat)
library(magrittr)
library(xgboost)
library(ROCR)
}
train = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Active_riskx_cases.csv", header = T, sep = ",")
test = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Active_riskx_cases_test_data.csv", header = T, sep = ",")

sapply(train, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
# colSums(is.na(train))
sum(is.na(train))


str(train)
summary(train)
dim(train)

# missing values in :-  sex, CUSTOMER_CATG_DESC, SOURCE, INDUSTRYID, INDUSTRY, PURPOSE_DESC,
#  DEALTYPE, RISKCATEGORY, DEAL_TYPE, CAS_SCORE, SECTORIAL_CLASSIFICATION, BORROWERSEGMENT.
# Treating the missing values with mean, median and mode

{                              # Replacing missing value in Sex with mode
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result2 = getmode(train$Sex)
  print(result2)
  train$Sex[is.na(train$Sex)] = result2
  
  # Replacing missing value in INDUSTRYID with mode
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result3 = getmode(train$INDUSTRYID)
  print(result3)
  train$INDUSTRYID[is.na(train$INDUSTRYID)] = result3
  
  # Replacing the missing value in industry with corresponding name included in industryid 
  
  train$INDUSTRY[is.na(train$INDUSTRY)] = "SERVICES"
  
  # Replacing the missing value in Qualification with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result4 = getmode(train$Qualification)
  print(result4)
  train$Qualification[is.na(train$Qualification)] = result4
  
  
  # Replacing the missing value in PURPOSE_DESC with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result6 = getmode(train$PURPOSE_DESC)
  print(result6)
  train$PURPOSE_DESC[is.na(train$PURPOSE_DESC)] = result6
  
  # Replacing the missing value in RISKCATEGORY with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result8 = getmode(train$RISKCATEGORY)
  print(result8)
  train$RISKCATEGORY[is.na(train$RISKCATEGORY)] = result8
  
  # Replacing the missing value in SECTORIAL_CLASSIFICATION with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result9 = getmode(train$SECTORIAL_CLASSIFICATION)
  print(result9)
  train$SECTORIAL_CLASSIFICATION[is.na(train$SECTORIAL_CLASSIFICATION)] = result9
  
  # Replacing the missing value in BORROWERSEGMENT with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result10 = getmode(train$BORROWERSEGMENT)
  print(result10)
  train$BORROWERSEGMENT[is.na(train$BORROWERSEGMENT)] = result10
  
  # Removing the missing value in age,cas_score and cibil_score by mean
  
  train$Age[is.na(train$Age)] = mean(train$Age, na.rm = T)
  train$CAS_SCORE[is.na(train$CAS_SCORE)] = mean(train$CAS_SCORE, na.rm = T)
  train$Cibil_Score[is.na(train$Cibil_Score)] = mean(train$Cibil_Score, na.rm = T)
  
  # Converting to factor, NUMERIC AND DATE FORMAT as required
  
  options(scipen = 999)
  
  train$Went_to_riskx_or_not = ifelse(train$Went_to_riskx_or_not == "Yes" , 1, 0)
  train$Went_to_riskx_or_not = as.factor(train$Went_to_riskx_or_not)
  #train$INTERESTTYPE = ifelse(train$INTERESTTYPE == "FIXED", 1, 2)
  #train$INTERESTTYPE = as.factor(train$INTERESTTYPE)
  
  train$AMTFIN = as.numeric(train$AMTFIN)
  train$LAA_CALCULATED_INCOME = as.numeric(train$LAA_CALCULATED_INCOME)
}

# to cross check if any missing values are present

sum(is.na(train))  # to see no. of NA cells in variables

# EDA
# for factor variable (UNIVARIATE)

bar2 = ggplot(train, aes(x = INTERESTTYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "Interest Type", y ="Frequency") + facet_wrap( ~ Went_to_riskx_or_not)
bar2

bar3 = ggplot(train, aes(x = PRODUCTFLAG, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "PRODUCTFLAG", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
bar3

bar4 = ggplot(train, aes(x = PROPERTY_TYPE_NEW, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "PROPERTY TYPE NEW OR NOT", y ="Frequency") +
  facet_wrap( ~ Went_to_riskx_or_not)
bar4

bar5 = ggplot(train, aes(x = BORROWERSEGMENT, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "BORROWER SEGMENT", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar5

bar6 = ggplot(train, aes(x = CONSTITUTION, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "CONSTITUTION", y ="Frequency") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar6

bar7 = ggplot(train, aes(x = RISKCATEGORY, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "RISK CATEGORY", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
bar7

bar8 = ggplot(train, aes(x = DEALTYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "DEAL TYPE", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar8

bar9 = ggplot(train, aes(x = PURPOSE_DESC, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "Purpose Described", y ="Frequency") #+
#  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar9

bar10 = ggplot(train, aes(x = INTTYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "INT TYPE", y ="Frequency") +
  facet_wrap( ~ Went_to_riskx_or_not)
bar10

bar11 = ggplot(train, aes(x = SOURCE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "SOURCE", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
bar11

bar12 = ggplot(train, aes(x = ENTITY_TYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "ENTITY TYPE", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar12

bar13 = ggplot(train, aes(x = CUSTOMER_CATG_DESC, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "Customer Category Description", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
bar13

bar14 = ggplot(train, aes(x = Marital_Status, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "Marital Status", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
bar14

bar15 = ggplot(train, aes(x = Sex, fill = Went_to_riskx_or_not )) + geom_bar(position = "dodge") + theme_light() + 
  labs(fill = "client went to train or not", x = "Sex", y ="Frequency")+
  facet_wrap( ~ Went_to_riskx_or_not)
bar15

bar16 = ggplot(train, aes(x = REGION, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to train or not", x = "REGION", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
bar16

# For continuous variable

hist(train$Age)
rug(train$Age)

box1 = ggplot(train, aes(x = Went_to_riskx_or_not, y = Age, fill = Went_to_riskx_or_not)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 4) + geom_jitter(width = 0.1, aes(color = Age))
box1

summary(train$INTRATE)
hist(train$INTRATE, col ="blue")
rug(train$INTRATE)

bar17 = ggplot(train, aes(x = Went_to_riskx_or_not, y = TENURE, fill = Went_to_riskx_or_not)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 4) + theme_light() +
  labs(fill = "Client went to risk x or not", x = " ", y = "frequency") +
  geom_jitter(width = 0.1, aes(color = Age))
bar17

summary(train$CAS_SCORE)
hist(train$CAS_SCORE, col = "red")
rug(train$CAS_SCORE)


################## TEST train TREATMENT #################################

sapply(test, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
# colSums(is.na(train))
sum(is.na(test))

dim(test)

# missing values in :-  sex, CUSTOMER_CATG_DESC, SOURCE, INDUSTRYID, INDUSTRY, PURPOSE_DESC,
#  DEALTYPE, RISKCATEGORY, DEAL_TYPE, CAS_SCORE, SECTORIAL_CLASSIFICATION, BORROWERSEGMENT.
# Treating the missing values with mean, median and mode

{                              # Replacing missing value in Sex with mode
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result2 = getmode(test$Sex)
  print(result2)
  test$Sex[is.na(test$Sex)] = result2
  
  # Replacing the missing value in Qualification with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result4 = getmode(test$Qualification)
  print(result4)
  test$Qualification[is.na(test$Qualification)] = result4
  
  
  # Replacing the missing value in Marital_Status with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result6 = getmode(test$Marital_Status)
  print(result6)
  test$Marital_Status[is.na(test$Marital_Status)] = result6
  
  # Replacing the missing value in BORROWERSEGMENT with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result10 = getmode(test$BORROWERSEGMENT)
  print(result10)
  test$BORROWERSEGMENT[is.na(test$BORROWERSEGMENT)] = result10
  
  # Removing the missing value in age,cas_score and cibil_score by mean
  
  test$Age[is.na(test$Age)] = mean(test$Age, na.rm = T)
  
  test$AMTFIN = as.numeric(test$AMTFIN)
  test$LAA_CALCULATED_INCOME = as.numeric(test$LAA_CALCULATED_INCOME)
}

# to cross check if any missing values are present

sum(is.na(test))  # to see no. of NA cells in variables

########################################################################
######## BINNING THE DATA ACCORDINGLY & FEATURE ENGINEERING

#Target variable went to riskx
prop.table(table(train$Went_to_riskx_or_not)) * 100
# went to riskx 63.48 %

# disbursed amount
ggplot(train, aes(x = log(AMTFIN), fill = factor(Went_to_riskx_or_not))) + geom_histogram()
ggplot(train, aes(x = factor(Went_to_riskx_or_not), y = log(AMTFIN))) + geom_boxplot()
summary(train$AMTFIN)

sum(train$AMTFIN > 1267862)

#Get outliers
OutVals = boxplot(train$AMTFIN, plot=FALSE)$out
# which(train$AMTFIN %in% OutVals)
1.5*IQR(train$AMTFIN) # 1140000

# finding lower and upper outlier limit
lout = quantile(train$AMTFIN, 0.25) - 1.5*IQR(train$AMTFIN)
hout = quantile(train$AMTFIN, 0.75) + 1.5*IQR(train$AMTFIN)
test$Went_to_riskx_or_not = vector("numeric", nrow(test))

all = rbind(train, test)


# create a variable on distribution of Financed amount
all$fin_amt_slab = vector(mode = "character", length = nrow(all))

all$fin_amt_slab[all$AMTFIN < lout] = "slab_lout"
all$fin_amt_slab[all$AMTFIN >= lout & 
                   all$AMTFIN < median(train$AMTFIN)] = "slab_proper_LM"
all$fin_amt_slab[all$AMTFIN >= median(train$AMTFIN) & 
                   all$AMTFIN < hout ] = "slab_proper_HM"
all$fin_amt_slab[all$AMTFIN >= hout ] = "slab_hout"

all$fin_amt_slab = factor(all$fin_amt_slab, 
                          levels = c("slab_lout", "slab_proper_LM", "slab_proper_HM", "slab_hout"))
prop.table(table(all$fin_amt_slab)) * 100

ggplot(all[1:nrow(train), ], aes(fin_amt_slab, fill = factor(Went_to_riskx_or_not))) + geom_bar()

# ltv variable is some ratio between disbursed amount and asset cost
summary(all$GROSS_LTV)
ggplot(all[1:nrow(train), ], aes(GROSS_LTV, fill = factor(Went_to_riskx_or_not))) + geom_histogram()

# create a variable GROSS_LTV
all$ltv_slab = vector(mode = "character", length = nrow(all))

all$ltv_slab[all$GROSS_LTV < 50] = "very_low"
all$ltv_slab[all$GROSS_LTV >= 50 & all$GROSS_LTV < 70] = "low"
all$ltv_slab[all$GROSS_LTV >= 70 & all$GROSS_LTV < 85] = "med"
all$ltv_slab[all$GROSS_LTV >= 85] = "high"
all$ltv_slab = factor(all$ltv_slab, levels = c("very_low", "low", "med", "high"))

prop.table(table(all$ltv_slab)) * 100

ggplot(all, aes(log(AMTFIN), fill = ltv_slab)) + geom_histogram(alpha = 0.5)
ggplot(all[(1:nrow(train)), ], aes(ltv_slab, fill = factor(Went_to_riskx_or_not))) + geom_bar()
ggplot(all[(1:nrow(train)), ], aes(fin_amt_slab, fill = ltv_slab)) + geom_bar() + facet_wrap(~ Went_to_riskx_or_not)

##################################################################################################

branch_count = all %>% group_by(BRANCHID) %>% summarise(num = n()) %>% arrange(num)
summary(branch_count$num)

# create a variable BRANCHID sales

branch_count$sales = vector(mode = "character", length = nrow(branch_count))
branch_count$sales[branch_count$num < quantile(branch_count$num, 0.25)] = "low_sales"
branch_count$sales[branch_count$num >= quantile(branch_count$num, 0.25) & 
                     branch_count$num < quantile(branch_count$num, 0.5)] = "med_sales"
branch_count$sales[branch_count$num >= quantile(branch_count$num, 0.5) & 
                     branch_count$num < quantile(branch_count$num, 0.75)] = "high_sales"
branch_count$sales[branch_count$num >= quantile(branch_count$num, 0.75)] = "Very_high_sales"

prop.table(table(branch_count$sales)) * 100

all = left_join(all, branch_count, by = "BRANCHID")
all$num = NULL
all$branch_sales = all$sales
all$sales = NULL
all$branch_sales = factor(all$branch_sales, levels = c("low_sales", "med_sales", "high_sales", "Very_high_sales"))


##################################################################################################
industry_count = all %>% group_by(INDUSTRYID) %>% summarise(num = n()) %>% arrange(num)
summary(industry_count$num)

# create a variable INDUSTRYID sales

industry_count$Industry_sales = vector(mode = "character", length = nrow(industry_count))

industry_count$Industry_sales[industry_count$num < quantile(industry_count$num, 0.25)] = "low_sales"
industry_count$Industry_sales[industry_count$num >= quantile(industry_count$num, 0.25) & 
                                industry_count$num < quantile(industry_count$num, 0.5)] = "med_sales"
industry_count$Industry_sales[industry_count$num >= quantile(industry_count$num, 0.5) & 
                                industry_count$num < quantile(industry_count$num, 0.75)] = "high_sales"
industry_count$Industry_sales[industry_count$num >= quantile(industry_count$num, 0.75)] = "Very_high_sales"

prop.table(table(industry_count$Industry_sales)) * 100

all = left_join(all, industry_count, by = "INDUSTRYID")
all$num = NULL

##################################################################################################
all$Age = ifelse(all$Age < 0, all$Age + 100 , all$Age)
head(all$Age)
summary(all$Age)

ggplot(all[1:nrow(train), ], aes(Age, fill = factor(Went_to_riskx_or_not))) + geom_histogram()

# create a variable on distribution of Age 

all$age_cat <- vector(mode = "character", length = nrow(all))
all$age_cat[all$Age < 20] <- "very_young"
all$age_cat[all$Age >= 20 & all$Age < 35] <- "young"
all$age_cat[all$Age >= 35 & all$Age < 50] <- "middle_age"
all$age_cat[all$Age >= 50] <- "old"

all$age_cat<- factor(all$age_cat, levels = c("very_young", "young", "middle_age", "old"))
prop.table(table(all$age_cat)) * 100
##################################################################################################

all = all %>% mutate(Disdursed_Pct = round((AMTFIN/PROPERTY_VALUE)*100, digits = 2))

##################################################################################################

#Get outliers
OutVals = boxplot(train$LAA_CALCULATED_INCOME, plot=FALSE)$out
# which(train$LAA_CALCULATED_INCOME %in% OutVals)
1.5*IQR(train$LAA_CALCULATED_INCOME) # 40500

# finding lower and upper outlier limit
lout = quantile(train$LAA_CALCULATED_INCOME, 0.25) - 1.5*IQR(train$LAA_CALCULATED_INCOME)
hout = quantile(train$LAA_CALCULATED_INCOME, 0.75) + 1.5*IQR(train$LAA_CALCULATED_INCOME)

# create a variable on distribution of INCOME 
all$income_slab = vector(mode = "character", length = nrow(all))

all$income_slab[all$LAA_CALCULATED_INCOME < lout] = "income_lout"
all$income_slab[all$LAA_CALCULATED_INCOME >= lout & 
                  all$LAA_CALCULATED_INCOME < median(train$LAA_CALCULATED_INCOME)] = "income_proper_LM"
all$income_slab[all$LAA_CALCULATED_INCOME >= median(train$LAA_CALCULATED_INCOME) & 
                  all$LAA_CALCULATED_INCOME < hout ] = "income_proper_HM"
all$income_slab[all$LAA_CALCULATED_INCOME >= hout ] = "income_hout"

all$income_slab = factor(all$income_slab, 
                         levels = c("income_lout", "income_proper_LM", "income_proper_HM", "income_hout"))
prop.table(table(all$income_slab)) * 100

ggplot(all[1:nrow(train), ], aes(income_slab, fill = factor(Went_to_riskx_or_not))) + geom_bar()

##################################################################################################

summary(all$TENURE)
ggplot(all[1:nrow(train), ], aes(TENURE, fill = factor(Went_to_riskx_or_not))) + geom_histogram()

# create a variable TENURE
all$tenure_slab = vector(mode = "character", length = nrow(all))

all$tenure_slab[all$TENURE < 75] = "very_low_tenure"
all$tenure_slab[all$TENURE >= 75 & all$TENURE < 150] = "low_tenure"
all$tenure_slab[all$TENURE >= 150 & all$TENURE < 225] = "med_tenure"
all$tenure_slab[all$TENURE >= 225] = "high_tenure"
all$tenure_slab = factor(all$tenure_slab, levels = c("very_low_tenure", "low_tenure", "med_tenure", "high_tenure"))

prop.table(table(all$tenure_slab)) * 100

ggplot(all, aes(log(AMTFIN), fill = tenure_slab)) + geom_histogram(alpha = 0.5)
ggplot(all[(1:nrow(train)), ], aes(tenure_slab, fill = factor(Went_to_riskx_or_not))) + geom_bar()
ggplot(all[(1:nrow(train)), ], aes(fin_amt_slab, fill = tenure_slab)) + geom_bar() + facet_wrap(~ Went_to_riskx_or_not)

##################################################################################################

summary(all$MOB)
hist(all$MOB)
ggplot(all[1:nrow(train), ], aes(MOB, fill = factor(Went_to_riskx_or_not))) + geom_histogram(binwidth = 5)

# create a variable MOB
all$MOB_slab = vector(mode = "character", length = nrow(all))

all$MOB_slab[all$MOB < 25] = "Relatively_new"
all$MOB_slab[all$MOB >= 25 & all$MOB < 50] = "new"
all$MOB_slab[all$MOB >= 50 & all$MOB < 75] = "long_time"
all$MOB_slab[all$MOB >= 75] = "Very_old"
all$MOB_slab = factor(all$MOB_slab, levels = c("Relatively_new", "new", "long_time", "Very_old"))

prop.table(table(all$MOB_slab)) * 100

ggplot(all, aes(log(AMTFIN), fill = MOB_slab)) + geom_histogram(alpha = 0.5)
ggplot(all[(1:nrow(train)), ], aes(MOB_slab, fill = factor(Went_to_riskx_or_not))) + geom_bar()
ggplot(all[(1:nrow(train)), ], aes(fin_amt_slab, fill = MOB_slab)) + geom_bar() + facet_wrap(~ Went_to_riskx_or_not)

##################################################################################################

city_count_1 = all %>% 
  group_by(CITY) %>% 
  filter(Went_to_riskx_or_not == 1) %>% 
  summarise(num = n()) %>% 
  arrange(CITY)

city_count_total = all %>% 
  group_by(CITY) %>% 
  summarise(num = n()) %>% 
  arrange(CITY)

city_count_1 = left_join(city_count_1, city_count_total, by = "CITY")

city_count_1 = city_count_1 %>% mutate(City_performance = round((num.x/num.y)*100, digits = 2))

city_count_1$Performance_of_city = vector(mode = "character", length = nrow(city_count_1))

city_count_1$Performance_of_city[city_count_1$City_performance < 55] = "Good"
city_count_1$Performance_of_city[city_count_1$City_performance >= 55 & city_count_1$City_performance < 80] = "Bad"
city_count_1$Performance_of_city[city_count_1$City_performance >= 80] = "Worst"
city_count_1$Performance_of_city = factor(city_count_1$Performance_of_city, levels = c("Good", "new", "Bad", "Worst"))

prop.table(table(city_count_1$Performance_of_city)) * 100

all = left_join(all, city_count_1, by = "CITY")

all$num.x = NULL
all$num.y = NULL
all$City_performance = NULL


##################################################################################################

{
  sapply(all, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result44 = getmode(all$Performance_of_city)
  print(result44)
  all$Performance_of_city[is.na(all$Performance_of_city)] = result44
}

all_log = all

all_log$APP_ID =NULL
all_log$BRANCHID =NULL
all_log$Age =NULL
all_log$Login.to.disbursed.conversion.days =NULL
all_log$INDUSTRYID =NULL
all_log$CITY =NULL
all_log$INDUSTRY = NULL

train = all_log[1:nrow(train), ]
test = all_log[-(1:nrow(train)), ]

test$Went_to_riskx_or_not = NULL
train_ss = train
train$Went_to_riskx_or_not = NULL

Dummy = dummyVars("~.",data = train)
Dummy_data = data.frame(predict(Dummy, train))

train$Went_to_riskx_or_not = train_ss$Went_to_riskx_or_not
Dummy_data$Went_to_riskx_or_not = train_ss$Went_to_riskx_or_not

# FEATURE SCALING

Dummy_data[ -182] = scale(Dummy_data[ -182])

Dummy_data$Performance_of_city.new = NULL
Dummy_data$income_slab.income_lout = NULL
Dummy_data$fin_amt_slab.slab_lout = NULL
Dummy_data$PROPERTY_TYPE.LAND..RESIDENTIAL = NULL
Dummy_data$PURPOSE_DESC.XXXPURCHASE.OF.HOUSE.AND.LOT = NULL
Dummy_data$PURPOSE_DESC.RHDFCL.LARP = NULL
Dummy_data$PURPOSE_DESC.RHDFCL.HOME.EXTENSION = NULL
Dummy_data$PURPOSE_DESC.RESALE.OF.RESI.PROPERTY = NULL
Dummy_data$REGION.KAUSHAMBI = NULL
Dummy_data$CONSTITUTION.MANUFACTURING = NULL


# write.csv(Dummy_data, file = "Dummy_data.csv", row.names = F)
# Dummy_data = read.csv("Dummy_data.csv")

# Splitting the dataset into training set and test set 

set.seed(123)
split = sample.split(Dummy_data$Went_to_riskx_or_not, SplitRatio = 0.70)
training_set = subset(Dummy_data , split == TRUE)
test_set = subset(Dummy_data, split == FALSE)

# training_set
# test_set


######################################################################################################


# install_log.packages("class")
library(class)

# Fitting K-NN to the training set and Predicting the Test Set results

y_pred_knn = knn(train = training_set[ , -172],
             test = test_set[ , -172],
             cl = training_set[ , 172],
             k = 5)

# Confusion Matrix

cm = table(test_set[ , 172], y_pred_knn)
cm

######################################################################################################

# Fitting SVM 

# install_log.packages("e1071")
# library(e1071)

classifier_svm = svm( formula = Went_to_riskx_or_not ~ . , 
                  data = training_set,
                  type = "C-classification",
                  kernal = "linear")

# Predecting the test set result
y_pred_svm = predict(classifier_svm, newdata = test_set[-172])

# Confusion Matrix

cm = table(test_set[ , 172], y_pred_svm)
cm

######################################################################################################



# kernal SVM 

classifier_ksvm = svm(formula = Went_to_riskx_or_not ~ . , 
                 data = training_set,
                 type = "C-classification",
                 kernel = "radial")

# Predecting the test set result

y_pred_ksvm = predict(classifier_ksvm, newdata = test_set[-172])

# Confusion Matrix

cm = table(test_set[ , 172], y_pred_ksvm)
cm


######################################################################################################


# Naive Bayes

# library(e1071)

# encoding the target feature as factor
training_set$Went_to_riskx_or_not = factor(training_set$Went_to_riskx_or_not, levels = c(0,1))
test_set$Went_to_riskx_or_not = factor(test_set$Went_to_riskx_or_not, levels = c(0,1))


classifier_nb = naiveBayes(x = training_set[-172],
                        y = training_set$Went_to_riskx_or_not)


# Predecting the test set result

y_pred_nb = predict(classifier_nb, newdata = test_set[-172])

# Confusion Matrix

cm = table(test_set[ , 172], y_pred_nb)
cm

######################################################################################################


# Decission Tree Classification

########### WITH SCALING

classifier_dt = rpart(formula = Went_to_riskx_or_not ~ ., 
                   data = training_set)
# Predecting the test set result
y_pred_dt = predict(classifier_dt, newdata = test_set[-172], type = "class")
# Confusion Matrix
cm = table(test_set[ , 172], y_pred_dt)
cm

##############WITHOUT SCALING

# classifier = rpart(formula = Went_to_riskx_or_not ~ ., 
#                    data = training_set)
# # Predecting the test set result
# y_pred = predict(classifier, newdata = test_set[-172], type = "class")
# # Confusion Matrix
# cm = table(test_set[ , 172], y_pred)
# cm                                          # very bad model

# Ploting the Decission tree

fancyRpartPlot(classifier_dt)
######################################################################################################


# Random forest Classifier
library(randomForest)

classifier_rf = randomForest(x = training_set[-172],
                          y = training_set$Went_to_riskx_or_not,
                          ntree = 1000)
# Predecting the test set result
y_pred_rf = predict(classifier_rf, newdata = test_set[-172])
# Confusion Matrix
cm = table(test_set[ , 172], y_pred_rf)
cm 

######################################################################################################

set.seed(123)
split = sample.split(Dummy_data$Went_to_riskx_or_not, SplitRatio = 0.70)
training_set = subset(Dummy_data , split == TRUE)
test_set = subset(Dummy_data, split == FALSE)

# Principal Componant Analysis

# training_set
# test_set

# Applying PCA

pca = preProcess(x = training_set[-172],
                 method = "pca",
                 thresh = .50,
                 pcaComp = 10)
training_set = predict(pca, training_set)
training_set = training_set[c(2:11,1)]

test_set = predict(pca, test_set)
test_set = test_set[c(2:11,1)]

# Fitting SVM

# instaa_log.packages("e1071")
# library(e1071)

classifier_pcasvm = svm( formula = Went_to_riskx_or_not ~ . ,
                  data = training_set,
                  type = "C-classification",
                  kernal = "linear")

# Predecting the test set result
y_pred_pcasvm = predict(classifier_pcasvm, newdata = test_set[-11])

# Confusion Matrix

cm = table(test_set[ , 11], y_pred_pcasvm)
cm

############################################################################
# XG Boosting fit method
# train data
Dummy = dummyVars("~.",data = train)
Dummy_data = data.frame(predict(Dummy, train))

train$Went_to_riskx_or_not = train_ss$Went_to_riskx_or_not
Dummy_data$Went_to_riskx_or_not = train_ss$Went_to_riskx_or_not

Dummy_data$Performance_of_city.new = NULL
Dummy_data$income_slab.income_lout = NULL
Dummy_data$fin_amt_slab.slab_lout = NULL
Dummy_data$PROPERTY_TYPE.LAND..RESIDENTIAL = NULL
Dummy_data$PURPOSE_DESC.XXXPURCHASE.OF.HOUSE.AND.LOT = NULL
Dummy_data$PURPOSE_DESC.RHDFCL.LARP = NULL
Dummy_data$PURPOSE_DESC.RHDFCL.HOME.EXTENSION = NULL
Dummy_data$PURPOSE_DESC.RESALE.OF.RESI.PROPERTY = NULL
Dummy_data$REGION.KAUSHAMBI = NULL
Dummy_data$CONSTITUTION.MANUFACTURING = NULL

set.seed(123)
split = sample.split(Dummy_data$Went_to_riskx_or_not, SplitRatio = 0.70)
training_set = subset(Dummy_data , split == TRUE)
test_set = subset(Dummy_data, split == FALSE)

# fitting xg boosting to the training set

classifier_xg = xgboost(data = as.matrix(training_set[-172]), 
                        label = training_set$Went_to_riskx_or_not,
                        nrounds = 55)
         # Applying K-fold cross validation 
library(caret)
folds = createFolds(training_set$Went_to_riskx_or_not, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier_xg = xgboost(data = as.matrix(training_set[-172]), 
                          label = training_set$Went_to_riskx_or_not,
                          nrounds = 55)
  y_pred_xg = predict(classifier_xg, newdata = as.matrix(test_fold[-172]))
  y_pred_xg = (y_pred_xg >= 0.7)
  cm = table(test_fold[, 172], y_pred_xg)
  # accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  # return(accuracy)
  })

accuracy = mean(as.numeric(cv))






