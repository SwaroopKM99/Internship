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

riskx$Went_to_riskx_or_not = ifelse(riskx$Went_to_riskx_or_not == "Yes" , 1, 0)
riskx$Went_to_riskx_or_not = as.factor(riskx$Went_to_riskx_or_not)
#riskx$INTERESTTYPE = ifelse(riskx$INTERESTTYPE == "FIXED", 1, 2)
#riskx$INTERESTTYPE = as.factor(riskx$INTERESTTYPE)

riskx$AMTFIN = as.numeric(riskx$AMTFIN)
riskx$LAA_CALCULATED_INCOME = as.numeric(riskx$LAA_CALCULATED_INCOME)
}

# to cross check if any missing values are present

sapply(riskx, function(x)sum(is.na(x)))  # to see no. of NA cells in variables

# making a duplicate file

file = riskx

# EDA
# for factor variable (UNIVARIATE)
{
bar1 = ggplot(file, aes(x = ZONE, fill = Went_to_riskx_or_not )) + geom_bar() + theme_light() + 
  labs(fill = "client went to riskx or not", x = "Zone", y ="Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went_to_riskx_or_not)
bar1

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
}

#################################################################################################
# Splitting the data into 75 - 25 

split = sample.split(file$Went_to_riskx_or_not, SplitRatio = 0.70)
train_data = subset(file, split == "TRUE")
test_data = subset(file, split == "FALSE")

train = train_data
test = test_data

# Finding the correlation and removing the multi correlated variable from both train and test file

M = subset(train, select = c(APP_ID,Age,Cibil_Score,IRR,AMTFIN,TENURE,BOARDING_RATE,
                   INDUSTRYID,INTRATE,GROSS_LTV,CAS_SCORE,DBR,PROPERTY_VALUE,LAA_CALCULATED_INCOME))

cr = cor(M)
corrplot(cr, type = "lower", method = "number")

N = subset(train, select = c(APP_ID,Age,Cibil_Score,IRR,AMTFIN,TENURE,
                             INDUSTRYID,GROSS_LTV,CAS_SCORE,DBR,PROPERTY_VALUE,LAA_CALCULATED_INCOME))
cr = cor(N)
corrplot(cr, type = "lower", method = "number")


# Checking the proportion

table(train$Went_to_riskx_or_not)
prop.table(table(train$Went_to_riskx_or_not))

#

# Creating the Decision tree model

modelDT = rpart(Went_to_riskx_or_not ~ ., data = train, method = "class", parms = list(split = "information"))
fancyRpartPlot(modelDT)

#Confusion Matrix
Pred=predict(modelDT,newdata=test,type="class")
confusionMatrix(table(Pred,test$Went_to_riskx_or_not))

#creating dummy variables

Dummy = dummyVars("~.",data = train)
Dummy_data1 = data.frame((predict(Dummy,train)))

# Creating the logistic model 

      # Base model

model1 = glm(Went_to_riskx_or_not ~ . , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model1)

res = predict(model1, Dummy_data1, type = "response")
table(Actualvalue=Dummy_data1$Went_to_riskx_or_not, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data1$Went_to_riskx_or_not)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

 # Actual model building

model100 = glm(Went_to_riskx_or_not ~ BRANCHID , data = Dummy_data1, family = binomial(link = "logit"))
summary(model100)

model101 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score  , data = Dummy_data1,
           family = binomial(link = "logit") )
summary(model101)

model102 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH  , data = Dummy_data1,
           family = binomial(link = "logit") )
summary(model102)

model103 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH + Age , data = Dummy_data1,
           family = binomial(link = "logit") )
summary(model103)

model104 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH + Age +
           Login.to.disbursed.conversion.days, data = Dummy_data1, 
           family = binomial(link = "logit") )
summary(model104)

model105 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
           Login.to.disbursed.conversion.days + AMTFIN, data = Dummy_data1,
           family = binomial(link = "logit") )
summary(model105)

model106 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
           Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS,
           data = Dummy_data1, family = binomial(link = "logit") )
summary(model106)

model107 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
           Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS +
           ENTITY_TYPE.LOW.INCOME.HOUSING ,
           data = Dummy_data1, family = binomial(link = "logit") )
summary(model107)

model108 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model108)

model109 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E ,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model109)

model110 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model110)

model111 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model111)

model112 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR + BORROWERSEGMENT.SALARIED,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model112)

model113 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN + CUSTOMER_CATG_DESC.OTHERS +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL ,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model113)

model114 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL + PROPERTY_TYPE.LAND..RESIDENTIAL ,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model114)

model115 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL + PROPERTY_TYPE.LAND..RESIDENTIAL+
                 PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model115)

model116 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL + PROPERTY_TYPE.LAND..RESIDENTIAL+
                 PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE +INTERESTTYPE.FIXED,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model116)

model117 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL + PROPERTY_TYPE.LAND..RESIDENTIAL+
                 PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE +INTERESTTYPE.FIXED+
                 STATE.DELHI,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model117)

model118 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL + PROPERTY_TYPE.LAND..RESIDENTIAL+
                 PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE +INTERESTTYPE.FIXED+
                 STATE.DELHI + STATE.GUJARAT ,
               data = Dummy_data1, family = binomial(link = "logit") )
summary(model118)

model119 = glm(Went_to_riskx_or_not ~ BRANCHID + Cibil_Score + ZONE.SOUTH +
                 Login.to.disbursed.conversion.days + AMTFIN +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED + INTTYPE.E + RISKCATEGORY.HIGH +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL + PROPERTY_TYPE.LAND..RESIDENTIAL+
                 PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE +INTERESTTYPE.FIXED+MOB+
                 STATE.DELHI + STATE.MADHYA.PRADESH , data = Dummy_data1, family = binomial(link = "logit") )
summary(model119)

model120 = glm(Went_to_riskx_or_not ~ ZONE.SOUTH +
                 AMTFIN +
                 ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED +
                 DBR +BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL + PROPERTY_TYPE.LAND..RESIDENTIAL+
                 PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE +MOB+
                 STATE.DELHI + STATE.MADHYA.PRADESH , data = Dummy_data1, family = binomial(link = "logit") )
summary(model120)

vif(model120)

####################################################################################################################

res = predict(model120, Dummy_data1, type = "response")
table(Actualvalue=Dummy_data1$Went_to_riskx_or_not, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data1$Went_to_riskx_or_not)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_train = (427+2960)/(427+2960+324+1462)
accuracy_train

# TESTING DATA

test$Went_to_riskx_or_not = NULL
Dummy = dummyVars("~.",data = test)
Dummy_data2 = data.frame((predict(Dummy,test)))
Dummy_data2$Went_to_riskx_or_not = test_data$Went_to_riskx_or_not

res = predict(model120, Dummy_data2, type = "response")
table(Actualvalue=Dummy_data2$Went_to_riskx_or_not, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data2$Went_to_riskx_or_not)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_test = (193+1264)/(193+1264+144+617)
accuracy_test

############################################################################

# after using boruta pacakage for feature reduction i found 8 variable not required
# so building the model again using important variable and removing non-important variables


train = train_data
test = test_data

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

train$Went_to_riskx_or_not = NULL
Dummy = dummyVars("~.",data = train)
Dummy_data3 = data.frame((predict(Dummy,train)))
Dummy_data3$Went_to_riskx_or_not = train_data$Went_to_riskx_or_not

# Creating the logistic model 

# Base model

model10 = glm(Went_to_riskx_or_not ~ . , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model10)

model11 = glm(Went_to_riskx_or_not ~ APP_ID , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model11)

model12 = glm(Went_to_riskx_or_not ~ APP_ID +BRANCHID, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model12)

model13 = glm(Went_to_riskx_or_not ~ APP_ID +BRANCHID+Cibil_Score, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model13)

model14 = glm(Went_to_riskx_or_not ~ APP_ID +BRANCHID+Cibil_Score+ZONE.SOUTH, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model14)

model15 = glm(Went_to_riskx_or_not ~ APP_ID +BRANCHID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model15)

model16 = glm(Went_to_riskx_or_not ~ APP_ID +BRANCHID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.HYDERABAD, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model16)

model17 = glm(Went_to_riskx_or_not ~ APP_ID +BRANCHID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.HYDERABAD+REGION.INDORE, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model17)

model18 = glm(Went_to_riskx_or_not ~ APP_ID +BRANCHID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.HYDERABAD+REGION.INDORE+REGION.JANAKPURI, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model18)

model19 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.HYDERABAD+REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR, data =  Dummy_data3, family = binomial(link = "logit"))
summary(model19)

model20 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.HYDERABAD+REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+REGION.KOTA
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model20)

model21 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+REGION.KOTA+REGION.MADURAI
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model21)

model22 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+REGION.KOTA+REGION.MADURAI+REGION.NAGPUR
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model22)

model23 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+REGION.KOTA+REGION.MADURAI+REGION.NAGPUR+
                REGION.TRICHY
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model23)

model24 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+REGION.KOTA+REGION.MADURAI+REGION.NAGPUR+
                REGION.TRICHY+REGION.UDAIPUR
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model24)

model25 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+REGION.KOTA+REGION.MADURAI+REGION.NAGPUR+
                REGION.TRICHY+REGION.UDAIPUR+AMOUNT 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model25)

model26 = glm(Went_to_riskx_or_not ~ APP_ID+Cibil_Score+ZONE.SOUTH+REGION.COIMBATORE+
                REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+REGION.KOTA+REGION.MADURAI+REGION.NAGPUR+
                REGION.TRICHY+REGION.UDAIPUR+AMOUNT+Login.to.disbursed.conversion.days 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model26)

model27 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+
                REGION.KOTA+REGION.TRICHY+REGION.UDAIPUR+AMOUNT+Login.to.disbursed.conversion.days+
                CUSTOMER_CATG_DESC.FIRST.TIME.BUYER
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model27)

model28 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+
                REGION.KOTA+REGION.TRICHY+REGION.UDAIPUR+AMOUNT+Login.to.disbursed.conversion.days+
                CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+BOARDING_RATE
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model28)

model29 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+
                REGION.KOTA+REGION.TRICHY+REGION.UDAIPUR+AMOUNT+Login.to.disbursed.conversion.days+
                CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model29)

model30 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JANAKPURI+REGION.JODHPUR+
                REGION.KOTA+REGION.TRICHY+REGION.UDAIPUR+AMOUNT+Login.to.disbursed.conversion.days+
                CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                ENTITY_TYPE.RHDFCL...I.T.R.SEMI.DOCUMENTED
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model30)

model31 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model31)

model32 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+SOURCE.REFERRAL
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model32)

model33 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+SOURCE.REFERRAL+INDUSTRY.CONTRACTOR 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model33)

model34 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+SOURCE.REFERRAL+INDUSTRY.CONTRACTOR+
                INDUSTRY.EDUCATION.CONSULTING
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model34)

model35 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+SOURCE.REFERRAL+INDUSTRY.CONTRACTOR+
                INDUSTRY.EDUCATION.CONSULTING+INDUSTRY.ELECTRONIC.EQUIPMENT
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model35)

model36 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VAMILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model36)

model37 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model37)

model38 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM+INDUSTRY.PAINTS.AND.VARNISHES 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model38)

model39 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM+INDUSTRY.PAINTS.AND.VARNISHES+INDUSTRY.SERVICES...COMMUNICATIONS
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model39)

model40 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM+INDUSTRY.PAINTS.AND.VARNISHES+INDUSTRY.SERVICES...COMMUNICATIONS+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model40)

model41 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM+INDUSTRY.PAINTS.AND.VARNISHES+INDUSTRY.SERVICES...COMMUNICATIONS+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INDUSTRY.TELECOM.DISTRIBUTOR
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model41)

model42 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM+INDUSTRY.PAINTS.AND.VARNISHES+INDUSTRY.SERVICES...COMMUNICATIONS+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INDUSTRY.TELECOM.DISTRIBUTOR+
                INTRATE     
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model42)

model43 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM+INDUSTRY.PAINTS.AND.VARNISHES+INDUSTRY.SERVICES...COMMUNICATIONS+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INDUSTRY.TELECOM.DISTRIBUTOR+
                INTRATE+GROSS_LTV   
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model43)

model44 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.KOTA+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+INDUSTRY.EDUCATION.CONSULTING+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TRAVEL.AND.TOURISM+INDUSTRY.PAINTS.AND.VARNISHES+INDUSTRY.SERVICES...COMMUNICATIONS+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INDUSTRY.TELECOM.DISTRIBUTOR+
                INTRATE+GROSS_LTV+PURPOSE_DESC.BALANCE.TRANSFER   
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model44)

model45 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+
                PURPOSE_DESC.OTHERS        
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model45)

model46 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+
                PURPOSE_DESC.OTHERS+PURPOSE_DESC.RHDFCL.BUSINESS.USE                 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model46)

model47 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+
                PURPOSE_DESC.OTHERS+PURPOSE_DESC.RHDFCL.BUSINESS.USE+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.                 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model47)

model48 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+
                PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+PURPOSE_DESC.RHDFCL.LARP+
                PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY  
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model48)

model49 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+
                PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+PURPOSE_DESC.RHDFCL.LARP+
                PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE   
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model49)

model50 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+
                PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+PURPOSE_DESC.RHDFCL.LARP+
                PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+
                PURPOSE_DESC.XXXPURCHASE.OF.HOUSE.AND.LOT 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model50)

model51 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.INDUSTRIES.NOT.CLASSIFIED.ELSEWHERE.SERVICES.+
                INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+
                PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+PURPOSE_DESC.RHDFCL.LARP+
                PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+
                PURPOSE_DESC.XXXPURCHASE.OF.HOUSE.AND.LOT+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.    
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model51)

model52 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP  
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model52)

model53 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR         
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model53)

model54 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP              
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model54)

model55 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED                 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model55)

model56 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+BORROWERSEGMENT.SALARIED                     
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model56)

model57 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model57)

model58 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL        
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model58)

model59 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+
                ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+INDUSTRY.CONTRACTOR+
                INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+INTRATE+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE    
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model59)

model60 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+PROPERTY_TYPE.TOWN.HOUSE      
              , data =  Dummy_data3, family = binomial(link = "logit"))

summary(model60)

model61 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+PROPERTY_TYPE.TOWN.HOUSE+
                INTERESTTYPE.FIXED  
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model61)

model62 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXCONSTRUCTION.OF.HOUSE+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+PROPERTY_TYPE.TOWN.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI         
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model62)

model63 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI                                        
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model63)

model64 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH                                                                                                                        
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model64)

model65 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE                                                                                              
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model65)

model66 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE+CITY.DINDIGUL                                                                                   
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model66)

model67 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE+CITY.DINDIGUL+CITY.GADAG                                                                                                 
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model67)

model68 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.SELF.CONSTRUCTION...RESI.PROPERTY+
                PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE+CITY.DINDIGUL+CITY.GADAG+
                CITY.JAIPUR                                                                       
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model68)

model69 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE+CITY.DINDIGUL+CITY.GADAG+
                CITY.JAIPUR+CITY.KOTA                                                                                                                                                           
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model69)

model70 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE+CITY.DINDIGUL+CITY.GADAG+
                CITY.JAIPUR+CITY.KOTA+CITY.NAGPUR                                                                                                                                                                                                                  
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model70)

model71 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DEALTYPE.RHDFCL.LAP+DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE+CITY.DINDIGUL+CITY.GADAG+
                CITY.JAIPUR+CITY.KOTA+CITY.NAGPUR+CITY.SURAT                                                                                                                                                                                                                                                                                                                                                                                                                         
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model71)

model72 = glm(Went_to_riskx_or_not ~ APP_ID+ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+Login.to.disbursed.conversion.days+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+INDUSTRY.TECHNICAL.CONSULTANCY.AND.ENGG.SERVICES+
                INTRATE+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.COMPOSITE.LOAN..PLOT...CONSTRUCTION.+
                PURPOSE_DESC.RHDFCL.LARP+PURPOSE_DESC.XXXRENOVATION...HOUSE.IMPROVMENT.+
                DBR+CONSTITUTION.PARTNERSHIP+CONSTITUTION.PVT.LIMITED+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                PROPERTY_TYPE.RESI..UNDER.CONSTRUCTION.INDEPENDENT.HOUSE+MOB+
                INTERESTTYPE.FIXED+STATE.DELHI+CITY.BHARUCH+CITY.COIMBATORE+CITY.DINDIGUL+CITY.GADAG+
                CITY.JAIPUR+CITY.KOTA+CITY.NAGPUR+CITY.SURAT+CITY.THANJAVUR                                                                                                                                                           
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model72)

model73 = glm(Went_to_riskx_or_not ~ ZONE.SOUTH+REGION.INDORE+REGION.JODHPUR+REGION.UDAIPUR+
                AMOUNT+ENTITY_TYPE.RHDFCL...VANILLA.INCOME.DOCUMENTED+
                INDUSTRY.CONTRACTOR+INDUSTRY.ELECTRONIC.EQUIPMENT+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.LARP+DBR+
                BORROWERSEGMENT.SELF.EMPLOYED.NON.PROFESSIONAL+PROPERTY_TYPE.LAND..RESIDENTIAL+
                MOB+STATE.DELHI+CITY.COIMBATORE+CITY.JAIPUR+CITY.SURAT                                                                                                                                                          
              , data =  Dummy_data3, family = binomial(link = "logit"))
summary(model73)


vif(model73)

####################################################################################################################

res = predict(model73, Dummy_data3, type = "response")
table(Actualvalue=Dummy_data3$Went_to_riskx_or_not, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data3$Went_to_riskx_or_not)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_train = (546+2887)/(546+2887+397+1343)
accuracy_train

# TESTING DATA

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

test$Went_to_riskx_or_not = NULL
Dummy = dummyVars("~.",data = test)
Dummy_data4 = data.frame((predict(Dummy,test)))
Dummy_data4$Went_to_riskx_or_not = test_data$Went_to_riskx_or_not

res = predict(model73, Dummy_data4, type = "response")
table(Actualvalue=Dummy_data4$Went_to_riskx_or_not, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data4$Went_to_riskx_or_not)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_test = (228+1235)/(228+1235+173+582)
accuracy_test

error = 1 - accuracy_test
error

# TP/P

Sensitivity_accuracy = 1233/(175+1233)
Sensitivity_accuracy

# TN/N

Specificity = 213/(213+597)
Specificity

# FP/N

False_positive_rate = 597/(213+597)
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

Dummy = dummyVars("~.",data = riskx_test)
Dummy_data_test = data.frame(predict(Dummy, riskx_test))


prediction = predict(model73, Dummy_data_test, type = "response")
Went_to_riskx_or_not = ifelse( prediction > .70, "Yes", "No")
output = cbind(riskx_test, Went_to_riskx_or_not)


write.csv(output, file = "RiskX_prediction.csv", row.names = FALSE)







