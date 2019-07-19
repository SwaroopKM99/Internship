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

riskx = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Active_riskx_cases.csv", header = T, sep = ",")

sapply(riskx, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
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
  
  # Replacing the missing value in SOURCE with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result4 = getmode(riskx$SOURCE)
  print(result4)
  riskx$SOURCE[is.na(riskx$SOURCE)] = result4
  
  # Replacing the missing value in CUSTOMER_CATG_DESC with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result5 = getmode(riskx$CUSTOMER_CATG_DESC)
  print(result5)
  riskx$CUSTOMER_CATG_DESC[is.na(riskx$CUSTOMER_CATG_DESC)] = result5
  
  # Replacing the missing value in PURPOSE_DESC with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result6 = getmode(riskx$PURPOSE_DESC)
  print(result6)
  riskx$PURPOSE_DESC[is.na(riskx$PURPOSE_DESC)] = result6
  
  # Replacing the missing value in DEALTYPE with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result7 = getmode(riskx$DEALTYPE)
  print(result7)
  riskx$DEALTYPE[is.na(riskx$DEALTYPE)] = result7
  
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
  
  # Replacing the missing value in PROPERTY_DESC with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result11 = getmode(riskx$PROPERTY_DESC)
  print(result11)
  riskx$PROPERTY_DESC[is.na(riskx$PROPERTY_DESC)] = result11
  
  # Removing the missing value in age,cas_score and cibil_score by mean
  
  riskx$Age[is.na(riskx$Age)] = mean(riskx$Age, na.rm = T)
  riskx$CAS_SCORE[is.na(riskx$CAS_SCORE)] = mean(riskx$CAS_SCORE, na.rm = T)
  riskx$Cibil_Score[is.na(riskx$Cibil_Score)] = mean(riskx$Cibil_Score, na.rm = T)
  
  # Converting to factor, NUMERIC AND DATE FORMAT as required
  
  options(scipen = 999)
  
  riskx$Went.to.riskx.or.not = ifelse(riskx$Went.to.riskx.or.not == "Yes" , 1, 0)
  riskx$Went.to.riskx.or.not = as.factor(riskx$Went.to.riskx.or.not)
  #riskx$INTERESTTYPE = ifelse(riskx$INTERESTTYPE == "FIXED", 1, 2)
  #riskx$INTERESTTYPE = as.factor(riskx$INTERESTTYPE)
  
  riskx$AMOUNT = as.numeric(riskx$AMOUNT)
  riskx$REQ_AMOUNT = as.numeric(riskx$REQ_AMOUNT)
  riskx$DISBURSED_AMOUNT = as.numeric(riskx$REQ_AMOUNT)
  riskx$AMTFIN = as.numeric(riskx$AMTFIN)
  riskx$ZIPCODE = as.numeric(riskx$ZIPCODE)
  riskx$LAA_CALCULATED_INCOME = as.numeric(riskx$LAA_CALCULATED_INCOME)
}

# to cross check if any missing values are present

sapply(riskx, function(x)sum(is.na(x)))  # to see no. of NA cells in variables

# making a duplicate file

file = riskx

{
Cat_P = LabelEncoder.fit(file$PRODUCTFLAG)
file$PRODUCTFLAG = transform(Cat_P, file$PRODUCTFLAG)

Cat_Z = LabelEncoder.fit(file$ZONE)
file$ZONE = transform(Cat_Z, file$ZONE)

Cat_R = LabelEncoder.fit(file$REGION)
file$REGION = transform(Cat_R, file$REGION)

Cat_S = LabelEncoder.fit(file$Sex)
file$Sex = transform(Cat_S, file$Sex)

Cat_M = LabelEncoder.fit(file$Marital_Status)
file$Marital_Status = transform(Cat_M, file$Marital_Status)

Cat_C = LabelEncoder.fit(file$CUSTOMER_CATG_DESC)
file$CUSTOMER_CATG_DESC = transform(Cat_C, file$CUSTOMER_CATG_DESC)

Cat_E = LabelEncoder.fit(file$ENTITY_TYPE)
file$ENTITY_TYPE = transform(Cat_E, file$ENTITY_TYPE)

Cat_SO = LabelEncoder.fit(file$SOURCE)
file$SOURCE = transform(Cat_SO, file$SOURCE)

Cat_I = LabelEncoder.fit(file$INDUSTRY)
file$INDUSTRY = transform(Cat_I, file$INDUSTRY)

Cat_IN = LabelEncoder.fit(file$INTTYPE)
file$INTTYPE = transform(Cat_IN, file$INTTYPE)

Cat_PD = LabelEncoder.fit(file$PURPOSE_DESC)
file$PURPOSE_DESC = transform(Cat_PD, file$PURPOSE_DESC)

Cat_DT = LabelEncoder.fit(file$DEALTYPE)
file$DEALTYPE = transform(Cat_DT, file$DEALTYPE)

Cat_RC = LabelEncoder.fit(file$RISKCATEGORY)
file$RISKCATEGORY = transform(Cat_RC, file$RISKCATEGORY)

Cat_SC = LabelEncoder.fit(file$SECTORIAL_CLASSIFICATION)
file$SECTORIAL_CLASSIFICATION = transform(Cat_SC, file$SECTORIAL_CLASSIFICATION)

Cat_RF = LabelEncoder.fit(file$RECONSIDER_FLAG)
file$RECONSIDER_FLAG = transform(Cat_RF, file$RECONSIDER_FLAG)

Cat_CO = LabelEncoder.fit(file$CONSTITUTION)
file$CONSTITUTION = transform(Cat_CO, file$CONSTITUTION)

Cat_BS = LabelEncoder.fit(file$BORROWERSEGMENT)
file$BORROWERSEGMENT = transform(Cat_BS, file$BORROWERSEGMENT)

Cat_PTY = LabelEncoder.fit(file$PROPERTY_TYPE)
file$PROPERTY_TYPE = transform(Cat_PTY, file$PROPERTY_TYPE)

Cat_PD = LabelEncoder.fit(file$PROPERTY_DESC)
file$PROPERTY_DESC = transform(Cat_PD, file$PROPERTY_DESC)

Cat_PTN = LabelEncoder.fit(file$PROPERTY_TYPE_NEW)
file$PROPERTY_TYPE_NEW = transform(Cat_PTN, file$PROPERTY_TYPE_NEW)

Cat_INTY = LabelEncoder.fit(file$INTERESTTYPE)
file$INTERESTTYPE = transform(Cat_INTY, file$INTERESTTYPE)

Cat_CITY = LabelEncoder.fit(file$CITY)
file$CITY = transform(Cat_CITY, file$CITY)

Cat_STATE = LabelEncoder.fit(file$STATE)
file$STATE = transform(Cat_STATE, file$STATE)
}
#####################

split = sample.split(file$Went.to.riskx.or.not, SplitRatio = 0.70)
train = subset(file, split == "TRUE")
test = subset(file, split == "FALSE")

{
  
  train$Sex = NULL #
  train$Marital_Status = NULL #
  train$Age = NULL #
  train$LOGIN_DATE = NULL #
  train$APPROVAL_DATE = NULL
  train$DISBURSAL_DATE = NULL
  train$INTTYPE = NULL #
  train$SECTORIAL_CLASSIFICATION = NULL #
  train$RECONSIDER_FLAG = NULL #
  train$REQ_AMOUNT = NULL
  train$AMTFIN = NULL
  train$ZIPCODE = NULL
  train$PROPERTY_DESC = NULL
  # train$CITY = NULL
  
}

{
  test$Sex = NULL #
  test$Marital_Status = NULL #
  test$Age = NULL #
  test$LOGIN_DATE = NULL #
  test$APPROVAL_DATE = NULL
  test$DISBURSAL_DATE = NULL
  test$INTTYPE = NULL #
  test$SECTORIAL_CLASSIFICATION = NULL #
  test$RECONSIDER_FLAG = NULL #
  test$REQ_AMOUNT = NULL
  test$AMTFIN = NULL
  test$ZIPCODE = NULL
  test$PROPERTY_DESC = NULL
  # test$CITY = NULL
}

M = subset(train, select = -c(Went.to.riskx.or.not))
cr = cor(M)
corrplot(cr, type = "lower", method = "number")

###########################################################################################

# building and trainig the model
#install.packages("nnet")
library(nnet)
#install.packages("magrittr")
library(magrittr)

model1001 = nnet::multinom(Went.to.riskx.or.not ~., data = train)

# Summarize the model

summary(model1001)


# Make predictions
predicted.classes = model1001 %>% predict(test)
head(predicted.classes)


# Model accuracy
mean(predicted.classes == test$Went.to.riskx.or.not)

#############################################################################################


model_999 = glm(Went.to.riskx.or.not ~ ., data = train, family = binomial(link = "logit"))
summary(model_999)

model999 = glm(Went.to.riskx.or.not ~ APP_ID, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ APP_ID+PRODUCTFLAG, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ APP_ID+PRODUCTFLAG+BRANCHID, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT+MOB, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT+MOB+IRR, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT+MOB+IRR+ENTITY_TYPE, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT+MOB+IRR+ENTITY_TYPE+
                 INDUSTRY, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT+MOB+IRR+ENTITY_TYPE+
                 INDUSTRY+DEALTYPE, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT+MOB+IRR+ENTITY_TYPE+
                 INDUSTRY+DEALTYPE+DBR, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ PRODUCTFLAG+BRANCHID+AMOUNT+MOB+IRR+ENTITY_TYPE+
                 INDUSTRY+DEALTYPE+DBR+BORROWERSEGMENT, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ BRANCHID+AMOUNT+MOB+IRR+ENTITY_TYPE+
                 DBR+BORROWERSEGMENT+LAA_CALCULATED_INCOME, data = train, family = binomial(link = "logit"))
summary(model999)

model999 = glm(Went.to.riskx.or.not ~ BRANCHID+MOB+IRR+ENTITY_TYPE+
                 DBR+BORROWERSEGMENT+LAA_CALCULATED_INCOME+STATE, data = train, family = binomial(link = "logit"))
summary(model999)


##########################################################################

res = predict(model999, train, type = "response")
table(Actualvalue=train$Went.to.riskx.or.not, Predictedvalue=res>0.5)

ROCRpred = prediction(res, train$Went.to.riskx.or.not)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_train = (377+3021)/(377+3021+263+1512)
accuracy_train


res = predict(model999, test, type = "response")
table(Actualvalue=test$Went.to.riskx.or.not, Predictedvalue=res>0.5)

ROCRpred = prediction(res, test$Went.to.riskx.or.not)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_test = (164+1292)/(164+1292+116+646)
accuracy_test

error = 1 - accuracy_test
error

# TP/P

Sensitivity_accuracy = 1292/(116+1292)
Sensitivity_accuracy

# TN/N

Specificity = 164/(164+646)
Specificity

# FP/N

False_positive_rate = 646/(164+646)
False_positive_rate


# Testing data from january 2019 to april 2019 and find the probability of the customer missing their Emi's


riskx_test = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Active_riskx_cases_test_data.csv", header = T, sep = ",")

sapply(riskx_test, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
sum(is.na(riskx_test))

str(riskx_test)
summary(riskx_test)
dim(riskx_test)


{                              # Replacing missing value in Sex with mode
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result2 = getmode(riskx_test$Sex)
  print(result2)
  riskx_test$Sex[is.na(riskx_test$Sex)] = result2
  
  # Replacing missing value in INDUSTRYID with mode
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result3 = getmode(riskx_test$INDUSTRYID)
  print(result3)
  riskx_test$INDUSTRYID[is.na(riskx_test$INDUSTRYID)] = result3
  
  # Replacing the missing value in industry with corresponding name included in industryid 
  
  riskx_test$INDUSTRY[is.na(riskx_test$INDUSTRY)] = "SERVICES"
  
  # Replacing the missing value in  Marital_Status with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result4 = getmode(riskx_test$Marital_Status)
  print(result4)
  riskx_test$Marital_Status[is.na(riskx_test$Marital_Status)] = result4
  
  # Replacing the missing value in BORROWERSEGMENT with mode
  
  getmode = function(v) {
    uniqv = unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  result10 = getmode(riskx_test$BORROWERSEGMENT)
  print(result10)
  riskx_test$BORROWERSEGMENT[is.na(riskx_test$BORROWERSEGMENT)] = result10
  
  # Removing the missing value in age,cas_score and cibil_score by mean
  
  riskx_test$Age[is.na(riskx_test$Age)] = mean(riskx_test$Age, na.rm = T)
}

sapply(riskx_test, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
sum(is.na(riskx_test))


{
  Cat_P = LabelEncoder.fit(riskx_test$PRODUCTFLAG)
  riskx_test$PRODUCTFLAG = transform(Cat_P, riskx_test$PRODUCTFLAG)
  
  Cat_Z = LabelEncoder.fit(riskx_test$ZONE)
  riskx_test$ZONE = transform(Cat_Z, riskx_test$ZONE)
  
  Cat_R = LabelEncoder.fit(riskx_test$REGION)
  riskx_test$REGION = transform(Cat_R, riskx_test$REGION)
  
  Cat_S = LabelEncoder.fit(riskx_test$Sex)
  riskx_test$Sex = transform(Cat_S, riskx_test$Sex)
  
  Cat_M = LabelEncoder.fit(riskx_test$Marital_Status)
  riskx_test$Marital_Status = transform(Cat_M, riskx_test$Marital_Status)
  
  Cat_C = LabelEncoder.fit(riskx_test$CUSTOMER_CATG_DESC)
  riskx_test$CUSTOMER_CATG_DESC = transform(Cat_C, riskx_test$CUSTOMER_CATG_DESC)
  
  Cat_E = LabelEncoder.fit(riskx_test$ENTITY_TYPE)
  riskx_test$ENTITY_TYPE = transform(Cat_E, riskx_test$ENTITY_TYPE)
  
  Cat_SO = LabelEncoder.fit(riskx_test$SOURCE)
  riskx_test$SOURCE = transform(Cat_SO, riskx_test$SOURCE)
  
  Cat_I = LabelEncoder.fit(riskx_test$INDUSTRY)
  riskx_test$INDUSTRY = transform(Cat_I, riskx_test$INDUSTRY)
  
  Cat_IN = LabelEncoder.fit(riskx_test$INTTYPE)
  riskx_test$INTTYPE = transform(Cat_IN, riskx_test$INTTYPE)
  
  Cat_PD = LabelEncoder.fit(riskx_test$PURPOSE_DESC)
  riskx_test$PURPOSE_DESC = transform(Cat_PD, riskx_test$PURPOSE_DESC)
  
  Cat_DT = LabelEncoder.fit(riskx_test$DEALTYPE)
  riskx_test$DEALTYPE = transform(Cat_DT, riskx_test$DEALTYPE)
  
  Cat_RC = LabelEncoder.fit(riskx_test$RISKCATEGORY)
  riskx_test$RISKCATEGORY = transform(Cat_RC, riskx_test$RISKCATEGORY)
  
  Cat_SC = LabelEncoder.fit(riskx_test$SECTORIAL_CLASSIFICATION)
  riskx_test$SECTORIAL_CLASSIFICATION = transform(Cat_SC, riskx_test$SECTORIAL_CLASSIFICATION)
  
  Cat_RF = LabelEncoder.fit(riskx_test$RECONSIDER_FLAG)
  riskx_test$RECONSIDER_FLAG = transform(Cat_RF, riskx_test$RECONSIDER_FLAG)
  
  Cat_CO = LabelEncoder.fit(riskx_test$CONSTITUTION)
  riskx_test$CONSTITUTION = transform(Cat_CO, riskx_test$CONSTITUTION)
  
  Cat_BS = LabelEncoder.fit(riskx_test$BORROWERSEGMENT)
  riskx_test$BORROWERSEGMENT = transform(Cat_BS, riskx_test$BORROWERSEGMENT)
  
  Cat_PTY = LabelEncoder.fit(riskx_test$PROPERTY_TYPE)
  riskx_test$PROPERTY_TYPE = transform(Cat_PTY, riskx_test$PROPERTY_TYPE)
  
  Cat_PD = LabelEncoder.fit(riskx_test$PROPERTY_DESC)
  riskx_test$PROPERTY_DESC = transform(Cat_PD, riskx_test$PROPERTY_DESC)
  
  Cat_PTN = LabelEncoder.fit(riskx_test$PROPERTY_TYPE_NEW)
  riskx_test$PROPERTY_TYPE_NEW = transform(Cat_PTN, riskx_test$PROPERTY_TYPE_NEW)
  
  Cat_INTY = LabelEncoder.fit(riskx_test$INTERESTTYPE)
  riskx_test$INTERESTTYPE = transform(Cat_INTY, riskx_test$INTERESTTYPE)
  
  Cat_CITY = LabelEncoder.fit(riskx_test$CITY)
  riskx_test$CITY = transform(Cat_CITY, riskx_test$CITY)
  
  Cat_STATE = LabelEncoder.fit(riskx_test$STATE)
  riskx_test$STATE = transform(Cat_STATE, riskx_test$STATE)
}

{
  riskx_test$Sex = NULL #
  riskx_test$Marital_Status = NULL #
  riskx_test$Age = NULL #
  riskx_test$LOGIN_DATE = NULL #
  riskx_test$APPROVAL_DATE = NULL
  riskx_test$DISBURSAL_DATE = NULL
  riskx_test$INTTYPE = NULL #
  riskx_test$SECTORIAL_CLASSIFICATION = NULL #
  riskx_test$RECONSIDER_FLAG = NULL #
  riskx_test$REQ_AMOUNT = NULL
  riskx_test$AMTFIN = NULL
  riskx_test$ZIPCODE = NULL
  riskx_test$PROPERTY_DESC = NULL
  riskx_test$CONSTITUTION = NULL
  # riskx_test$CITY = NULL
}

prediction = predict(model999, riskx_test, type = "response")
Went.to.riskx.or.not = ifelse( prediction > .70, "Yes", "No")
output = cbind(riskx_test, Went.to.riskx.or.not)


write.csv(output, file = "RiskX_prediction.csv", row.names = FALSE)









