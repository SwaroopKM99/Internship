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
  bar1 = ggplot(file, aes(x = ZONE, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Zone", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went.to.riskx.or.not)
  bar1
  
  bar2 = ggplot(file, aes(x = INTERESTTYPE, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Interest Type", y ="Frequency") + facet_wrap( ~ Went.to.riskx.or.not)
  bar2
  
  bar3 = ggplot(file, aes(x = PRODUCTFLAG, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "PRODUCTFLAG", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went.to.riskx.or.not)
  bar3
  
  bar4 = ggplot(file, aes(x = PROPERTY_TYPE_NEW, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "PROPERTY TYPE NEW OR NOT", y ="Frequency") +
    facet_wrap( ~ Went.to.riskx.or.not)
  bar4
  
  bar5 = ggplot(file, aes(x = BORROWERSEGMENT, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "BORROWER SEGMENT", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar5
  
  bar6 = ggplot(file, aes(x = CONSTITUTION, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "CONSTITUTION", y ="Frequency") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar6
  
  bar7 = ggplot(file, aes(x = RISKCATEGORY, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "RISK CATEGORY", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went.to.riskx.or.not)
  bar7
  
  bar8 = ggplot(file, aes(x = DEALTYPE, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "DEAL TYPE", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar8
  
  bar9 = ggplot(file, aes(x = PURPOSE_DESC, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Purpose Described", y ="Frequency") #+
  #  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar9
  
  bar10 = ggplot(file, aes(x = INTTYPE, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "INT TYPE", y ="Frequency") +
    facet_wrap( ~ Went.to.riskx.or.not)
  bar10
  
  bar11 = ggplot(file, aes(x = SOURCE, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "SOURCE", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went.to.riskx.or.not)
  bar11
  
  bar12 = ggplot(file, aes(x = ENTITY_TYPE, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "ENTITY TYPE", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar12
  
  bar13 = ggplot(file, aes(x = CUSTOMER_CATG_DESC, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Customer Category Description", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  bar13
  
  bar14 = ggplot(file, aes(x = Marital_Status, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Marital Status", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap( ~ Went.to.riskx.or.not)
  bar14
  
  bar15 = ggplot(file, aes(x = Sex, fill = Went.to.riskx.or.not )) + geom_bar(position = "dodge") + theme_light() + 
    labs(fill = "client went to riskx or not", x = "Sex", y ="Frequency")+
    facet_wrap( ~ Went.to.riskx.or.not)
  bar15
  
  bar16 = ggplot(file, aes(x = REGION, fill = Went.to.riskx.or.not )) + geom_bar() + theme_light() + 
    labs(fill = "client went to riskx or not", x = "REGION", y ="Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  bar16
  
  # For continuous variable
  
  hist(file$Age)
  rug(file$Age)
  
  box1 = ggplot(file, aes(x = Went.to.riskx.or.not, y = Age, fill = Went.to.riskx.or.not)) +
    geom_boxplot(outlier.color = "red", outlier.shape = 4) + geom_jitter(width = 0.1, aes(color = Age))
  box1
  
  summary(file$INTRATE)
  hist(file$INTRATE, col ="blue")
  rug(file$INTRATE)
  
  bar17 = ggplot(file, aes(x = Went.to.riskx.or.not, y = TENURE, fill = Went.to.riskx.or.not)) +
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

split = sample.split(file$Went.to.riskx.or.not, SplitRatio = 0.70)
train_data = subset(file, split == "TRUE")
test_data = subset(file, split == "FALSE")

train = train_data
test = test_data

# Finding the correlation and removing the multi correlated variable from both train and test file

M = subset(train, select = c(Age,Cibil_Score,IRR,AMTFIN,TENURE,BOARDING_RATE,MOB,
                             INDUSTRYID,INTRATE,GROSS_LTV,CAS_SCORE,DBR,PROPERTY_VALUE,LAA_CALCULATED_INCOME))

cr = cor(M)
corrplot(cr, type = "lower", method = "number")

N = subset(train, select = c(APP_ID,Age,Cibil_Score,IRR,AMTFIN,TENURE,
                             INDUSTRYID,GROSS_LTV,CAS_SCORE,DBR,PROPERTY_VALUE,LAA_CALCULATED_INCOME))
cr = cor(N)
corrplot(cr, type = "lower", method = "number")


# Checking the proportion

as.data.frame(table(train$Went.to.riskx.or.not))
as.data.frame(prop.table(table(file$Went.to.riskx.or.not))*100)

#

# Creating the Decision tree model

modelDT = rpart(Went.to.riskx.or.not ~ ., data = train, method = "class", parms = list(split = "information"))
fancyRpartPlot(modelDT)

#Confusion Matrix
Pred=predict(modelDT,newdata=test,type="class")
confusionMatrix(table(Pred,test$Went.to.riskx.or.not))

#creating dummy variables

Dummy = dummyVars("~.",data = train)
Dummy_data1 = data.frame((predict(Dummy,train)))
View(Dummy_data1)

# Creating the logistic model 

# Base model

model1 = glm(Went.to.riskx.or.not.1 ~ . , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model1)

model2 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1 , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model2)

model3 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model3)

model4 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model4)

model5 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model5)

model6 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
               REGION.COIMBATORE , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model6)

model7 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
               REGION.COIMBATORE+REGION.HYDERABAD , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model7)

model8 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
               REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model8)

model9 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
               REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model9)

model10 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
               REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model10)

model11 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT, data =  Dummy_data1, family = binomial(link = "logit"))
summary(model11)

model12 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+REGION.THANE , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model12)

model13 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+REGION.THANE+REGION.TRICHY , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model13)

model14 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+Cibil_Score+ZONE.NORTH+ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+REGION.THANE+REGION.TRICHY+REGION.VIZAG , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model14)

model15 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+ZONE.NORTH+ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+REGION.TRICHY+MOB , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model15)

model16 = glm(Went.to.riskx.or.not.1 ~ PRODUCTFLAG.MHDFCHL1+ZONE.NORTH+ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+REGION.TRICHY+MOB+Login.to.disbursed.conversion.days , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model16)

model17 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR
                , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model17)

model18 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model18)

model19 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model19)

model20 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model20)

model21 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model21)

model22 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model22)

model23 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model23)

model24 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model24)

model25 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model25)

model26 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model26)

model27 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model27)

model28 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model28)

model29 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model29)

model30 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+DEALTYPE.RHDFCL.LAP
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model30)

model31 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+DEALTYPE.RHDFCL.LAP+SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model31)

model32 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+DEALTYPE.RHDFCL.LAP+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.OTHERS 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model32)

model33 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+DEALTYPE.RHDFCL.LAP+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model33)

model34 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.FIRST.TIME.BUYER+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+DEALTYPE.RHDFCL.LAP+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                CONSTITUTION.INDIVIDUAL.PERSON
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model34)

model35 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+DEALTYPE.RHDFCL.LAP+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                CONSTITUTION.INDIVIDUAL.PERSON+BORROWERSEGMENT.SALARIED
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model35)

model36 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                CONSTITUTION.INDIVIDUAL.PERSON+BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model36)

model37 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                CONSTITUTION.INDIVIDUAL.PERSON+BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+
                PROPERTY_TYPE.INDEPENDENT.HOUSE
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model37)

model38 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.HYDERABAD+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+
                PROPERTY_TYPE.INDEPENDENT.HOUSE+CITY.DELHI  
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model38)

model39 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+
                PROPERTY_TYPE.INDEPENDENT.HOUSE+CITY.DELHI+CITY.GADAG 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model39)

model40 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+PROPERTY_TYPE.INDEPENDENT.HOUSE+CITY.DELHI+
                CITY.GADAG+CITY.HYDERABAD 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model40)

model41 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+PROPERTY_TYPE.INDEPENDENT.HOUSE+CITY.DELHI+
                CITY.GADAG+CITY.HYDERABAD+CITY.JODHPUR 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model41)

model42 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+PROPERTY_TYPE.INDEPENDENT.HOUSE+CITY.DELHI+
                CITY.GADAG+CITY.HYDERABAD+CITY.JODHPUR+CITY.MADURAI    
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model42)

model43 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+SECTORIAL_CLASSIFICATION.RETAIL.HOUSING+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+PROPERTY_TYPE.INDEPENDENT.HOUSE+CITY.DELHI+
                CITY.GADAG+CITY.HYDERABAD+CITY.JODHPUR+CITY.MADURAI+CITY.MYSORE
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model43)

model44 = glm(Went.to.riskx.or.not.1 ~ ZONE.SOUTH+
                REGION.COIMBATORE+REGION.INDORE+REGION.JAIPUR+REGION.JANAKPURI+
                REGION.SURAT+MOB+IRR+AMTFIN+CUSTOMER_CATG_DESC.XXX.PRIORITY+
                ENTITY_TYPE.OTHERS+ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+
                +SOURCE.REFERRAL+INDUSTRY.FINANCIAL.SERVICES+
                INTTYPE.E+PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+
                SECTORIAL_CLASSIFICATION.CRE.AND.BUILDER+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+PROPERTY_TYPE.INDEPENDENT.HOUSE+CITY.DELHI+
                CITY.GADAG+CITY.HYDERABAD+CITY.JODHPUR+CITY.MYSORE                                 
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model44)

model45 = glm(Went.to.riskx.or.not.1 ~ REGION.COIMBATORE+REGION.JAIPUR+
                REGION.SURAT+MOB+AMTFIN+ENTITY_TYPE.OTHERS+
                ENTITY_TYPE.RHDFCL...ASSESSED.INCOME.PROGRAM.UNDOCUMENTED+INDUSTRY.FINANCIAL.SERVICES+
                PURPOSE_DESC.BALANCE.TRANSFER+PURPOSE_DESC.RHDFCL.HOME.RENOVATION...IMPROVMENT+
                PURPOSE_DESC.RHDFCL.SELF.CONSTRUCTION.RESI.PROPERTY+DBR+
                BORROWERSEGMENT.SALARIED+PROPERTY_TYPE.APARTMENT+CITY.DELHI
              , data =  Dummy_data1, family = binomial(link = "logit"))
summary(model45)

vif(model45)

####################################################################################################################

res = predict(model45, Dummy_data1, type = "response")
table(Actualvalue=Dummy_data1$Went.to.riskx.or.not.1, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data1$Went.to.riskx.or.not.1)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_train = (527+2914)/(527+2914+1362+370)
accuracy_train

###############TEST###########################

Dummy = dummyVars("~.",data = test)
Dummy_data2 = data.frame((predict(Dummy,test)))

res = predict(model45, Dummy_data2, type = "response")
table(Actualvalue=Dummy_data2$Went.to.riskx.or.not.1, Predictedvalue=res>0.5)

ROCRpred = prediction(res, Dummy_data2$Went.to.riskx.or.not.1)
ROCRpref = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpref, colorize = T, print.cutoffs.at=seq(0.1,by=0.2))

accuracy_test = (213+1235)/(213+1235+173+597)
accuracy_test

error = 1 - accuracy_test
error

# TP/P

Sensitivity_accuracy = 1270/(138+1270)
Sensitivity_accuracy

# TN/N

Specificity = 216/(216+594)
Specificity

# FP/N

False_positive_rate = 594/(216+594)
False_positive_rate

################################################################################


riskx_test = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/Active_riskx_cases_test_data.csv", header = T, sep = ",")

sapply(riskx_test, function(x)sum(is.na(x)))  # to see no. of NA cells in variables
sum(is.na(riskx_test))


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

riskx_test = riskx_test %>% mutate(Disdursed_Pct = round((AMTFIN/PROPERTY_VALUE)*100, digits = 2))


str(riskx_test)

riskx_test$CONSTITUTION = NULL
riskx_test$INTTYPE = NULL

Dummy = dummyVars("~.",data = riskx_test)
Dummy_data_test = data.frame(predict(Dummy, riskx_test))

prediction = predict(model45, Dummy_data_test, type = "response")
Went.to.riskx.or.not = ifelse( prediction > .75, "Yes", "No")
output = cbind(riskx_test, Went.to.riskx.or.not)


write.csv(output, file = "RiskX_prediction.CSV", row.names = F)


























