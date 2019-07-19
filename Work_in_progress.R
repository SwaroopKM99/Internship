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
#install.packages("CatEncoders")
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


riskx = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Active_riskx_cases.csv", header = T, sep = ",")
test = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Active_riskx_cases_test_data.csv", header = T, sep = ",")

sapply(riskx, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
# colSums(is.na(train))
sum(is.na(riskx))


str(riskx)
summary(riskx)
dim(riskx)

# missing values in :-  sex, CUSTOMER_CATG_DESC, SOURCE, INDUSTRYID, INDUSTRY, PURPOSE_DESC,
#  DEALTYPE, RISKCATEGORY, DEAL_TYPE, CAS_SCORE, SECTORIAL_CLASSIFICATION, BORROWERSEGMENT.
# Treating the missing values with mean, median and mode

{                              # Replacing missing value in Sex with mode
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result2 = getmode(riskx$Sex)
  print(result2)
  riskx$Sex[is.na(riskx$Sex)] = result2
  
  # Replacing missing value in INDUSTRYID with mode
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result3 = getmode(riskx$INDUSTRYID)
  print(result3)
  riskx$INDUSTRYID[is.na(riskx$INDUSTRYID)] = result3
  
  # Replacing the missing value in industry with corresponding name included in industryid 
  
  riskx$INDUSTRY[is.na(riskx$INDUSTRY)] = "SERVICES"
  
  # Replacing the missing value in Qualification with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result4 = getmode(riskx$Qualification)
  print(result4)
  riskx$Qualification[is.na(riskx$Qualification)] = result4
  
  
  # Replacing the missing value in PURPOSE_DESC with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result6 = getmode(riskx$PURPOSE_DESC)
  print(result6)
  riskx$PURPOSE_DESC[is.na(riskx$PURPOSE_DESC)] = result6
  
  # Replacing the missing value in RISKCATEGORY with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result8 = getmode(riskx$RISKCATEGORY)
  print(result8)
  riskx$RISKCATEGORY[is.na(riskx$RISKCATEGORY)] = result8
  
  # Replacing the missing value in SECTORIAL_CLASSIFICATION with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result9 = getmode(riskx$SECTORIAL_CLASSIFICATION)
  print(result9)
  riskx$SECTORIAL_CLASSIFICATION[is.na(riskx$SECTORIAL_CLASSIFICATION)] = result9
  
  # Replacing the missing value in BORROWERSEGMENT with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result10 = getmode(riskx$BORROWERSEGMENT)
  print(result10)
  riskx$BORROWERSEGMENT[is.na(riskx$BORROWERSEGMENT)] = result10
  
  # Removing the missing value in age,cas_score and cibil_score by mean
  
  riskx$Age[is.na(riskx$Age)] = mean(riskx$Age, na.rm = T)
  riskx$CAS_SCORE[is.na(riskx$CAS_SCORE)] = mean(riskx$CAS_SCORE, na.rm = T)
  riskx$Cibil_Score[is.na(riskx$Cibil_Score)] = mean(riskx$Cibil_Score, na.rm = T)
  
  # Converting to factor, NUMERIC AND DATE FORMAT as required
  
  options(scipen = 999)
  
  riskx$Went_to_riskx_or_not = ifelse(riskx$Went_to_riskx_or_not == "Yes" , 1, 0)
  riskx$Went_to_riskx_or_not = as.factor(riskx$Went_to_riskx_or_not)
  #riskx$INTERESTTYPE = ifelse(riskx$INTERESTTYPE == "FIXED", 1, 2)
  #riskx$INTERESTTYPE = as.factor(riskx$INTERESTTYPE)
  
  riskx$AMTFIN = as.numeric(riskx$AMTFIN)
  riskx$LAA_CALCULATED_INCOME = as.numeric(riskx$LAA_CALCULATED_INCOME)
}

# to cross check if any missing values are present

sum(is.na(riskx))  # to see no. of NA cells in variables

# making a duplicate file

file = riskx

# EDA
# for factor variable (UNIVARIATE)

  bar2 = ggplot(file, aes(x = INTERESTTYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Interest Type", y ="Frequency") + facet_wrap( ~ Went_to_riskx_or_not)
  bar2
  
  bar3 = ggplot(file, aes(x = PRODUCTFLAG, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "PRODUCTFLAG", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
  bar3
  
  bar4 = ggplot(file, aes(x = PROPERTY_TYPE_NEW, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "PROPERTY TYPE NEW OR NOT", y ="Frequency") +
    facet_wrap( ~ Went_to_riskx_or_not)
  bar4
  
  bar5 = ggplot(file, aes(x = BORROWERSEGMENT, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "BORROWER SEGMENT", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar5
  
  bar6 = ggplot(file, aes(x = CONSTITUTION, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "CONSTITUTION", y ="Frequency") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar6
  
  bar7 = ggplot(file, aes(x = RISKCATEGORY, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "RISK CATEGORY", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
  bar7
  
  bar8 = ggplot(file, aes(x = DEALTYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "DEAL TYPE", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar8
  
  bar9 = ggplot(file, aes(x = PURPOSE_DESC, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Purpose Described", y ="Frequency") #+
  #  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar9
  
  bar10 = ggplot(file, aes(x = INTTYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "INT TYPE", y ="Frequency") +
    facet_wrap( ~ Went_to_riskx_or_not)
  bar10
  
  bar11 = ggplot(file, aes(x = SOURCE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "SOURCE", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
  bar11
  
  bar12 = ggplot(file, aes(x = ENTITY_TYPE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "ENTITY TYPE", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar12
  
  bar13 = ggplot(file, aes(x = CUSTOMER_CATG_DESC, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Customer Category Description", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar13
  
  bar14 = ggplot(file, aes(x = Marital_Status, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Marital Status", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
  bar14
  
  bar15 = ggplot(file, aes(x = Sex, fill = Went_to_riskx_or_not )) + geom_bar(position = "dodge") + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Sex", y ="Frequency")+
    facet_wrap( ~ Went_to_riskx_or_not)
  bar15
  
  bar16 = ggplot(file, aes(x = REGION, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "REGION", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  bar16
  
  # For continuous variable
  
  hist(file$Age)
  rug(file$Age)
  
  box1 = ggplot(file, aes(x = Went_to_riskx_or_not, y = Age, fill = Went_to_riskx_or_not)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 4) + geom_jitter(width = 0.1, aes(color = Age))
  box1
  
  summary(file$INTRATE)
  hist(file$INTRATE, col ="blue")
  rug(file$INTRATE)
  
  bar17 = ggplot(file, aes(x = Went_to_riskx_or_not, y = TENURE, fill = Went_to_riskx_or_not)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 4) + theme_light() +
    labs(fill = "Client went to risk x or not", x = " ", y = "frequency") +
    geom_jitter(width = 0.1, aes(color = Age))
  bar17
  
  summary(file$CAS_SCORE)
  hist(file$CAS_SCORE, col = "red")
  rug(file$CAS_SCORE)
  

################## TEST FILE TREATMENT #################################

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

######## BINNING THE DATA ACCORDINGLY & FATURE ENGINEERING

train = file

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

# create a variable GROSS_LTV
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

# create a variable GROSS_LTV
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

city_count = all %>% group_by(CITY , Went_to_riskx_or_not) %>% summarise(count = n()) %>% arrange(CITY)


##################################################################################################

all_log = all


train_ss = all[1:nrow(train), ]
test_ss = all[-(1:nrow(train)), ]
train_ss$Went_to_riskx_or_not = NULL
train_ss$Went_to_riskx_or_not = train$Went_to_riskx_or_not

test_ss$Went_to_riskx_or_not = NULL


vars = names(train_ss)[-48]

# Create the treatment plan for train data
treatplan = designTreatmentsZ(train_ss, vars, verbose = FALSE)
summary(treatplan)

# Get the "clean" and "lev" variables from the scoreFrame
newvars = treatplan %>%
  use_series(scoreFrame) %>%        
  filter(code %in% c("clean", "lev")) %>%  # get the rows you care about
  use_series(varName)

# Prepare the training data
train.treat = prepare(treatplan, train_ss,  varRestriction = newvars)

# Prepare the test data
test.treat = prepare(treatplan, test_ss,  varRestriction = newvars)


train_ss$Went_to_riskx_or_not = as.integer(train_ss$Went_to_riskx_or_not) - 1

# Run xgb.cv
cv = xgb.cv(data = as.matrix(train.treat), 
             nrounds = 200,
             nfold = 10,
             label = train_ss$Went_to_riskx_or_not,
             objective = "binary:logistic",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0    # silent
)


# Get the evaluation log 
elog = as.data.frame(cv$evaluation_log)

trees = elog %>% 
  summarise(ntrees.train = which.min(elog$train_error_mean), 
            ntrees.test  = which.min(elog$test_error_mean))

# The number of trees to use, as determined by xgb.cv
ntrees = trees$ntrees.test
ntrees


# Run xgboost
model_xgb = xgboost(data = as.matrix(train.treat), # training data as matrix
                     label = train_ss$Went_to_riskx_or_not,  # column of outcomes
                     nrounds = ntrees,       # number of trees to build
                     objective = "binary:logistic", # objective
                     eta = 0.3,
                     depth = 6,
                     verbose = 0)  # silent
test_ss$pred = predict(model_xgb, as.matrix(test.treat))

submit.df = data.frame(APP_ID= test$APP_ID, Went_to_riskx_or_not = rep(0, times = length(test$APP_ID)))
submit.df$Went_to_riskx_or_not = test_ss$pred
head(submit.df)

# write.csv(submit.df, file = "sub_default.csv", row.names = FALSE)







