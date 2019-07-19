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

riskx = riskx %>% mutate(Disdursed_Pct = round((AMTFIN/PROPERTY_VALUE)*100, digits = 2))

file = riskx
str(file)

############################################################################################################################
Dummy = dummyVars("~.",data = file)
Dummy_data1 = data.frame((predict(Dummy,file)))

## All date values are ok; no wrong values
### Analysing the information value of the variables # Filter variables based on the information value 
##Excluding the Date.of.Birth , DisbursalDate and App_ID


Infotable = create_infotables(data=Dummy_data1, y="Went.to.riskx.or.not.1")

##Removing the following since the IV is very low
#Removing APP_ID and Date.of.Birth (age is used instead) and DisbursalDate, Login Date, Approved date

training = Dummy_data1[, -c(1,66,23,46,173,220,194,131,296,280,214,260,284,7,157,279,132,49,50,
                                 27,166,218,236,191,247,79,245,120,99,202,137,186,244,297,193,16,178,
                                 213,233,306,73,217,146,134,298,114,237,15,121,195,104,9,36,122,249,
                                 55,71,159,83,116,184,210,259,267,119,177,182,188,291,192,277,135,294,
                                 138,56,3,113,176,215,223,268,65,303,312,238,95,68,25,45,14,130,72,11,
                                 47,48,139,42,293,196,67,276,292,128,133,147,251,264,44,84,160,257,41,
                                 200,155,295,248,290,28,126,172,69,163,174,212,225,226,253,307,33,110,
                                 170,180,189,197,201,209,227,308,87,111,129,156,158,161,169,198,204,221,
                                 230,262,246,115,124,164,165,167,168,179,181,185,207,211,216,219,229,231,
                                 234,235,240,243,250,256,261,265,269,270,275,281,283,287,289,299,98,313)]



binning = woe.tree.binning(training,'Went.to.riskx.or.not.1',training)
# woe.binning.plot(binning,multiple.plots = "FALSE")

training_woe = woe.binning.deploy(training,binning,add.woe.or.dum.var="woe")

############################################################################################################################
file = riskx

tot = nrow(file)
n_train = round(0.75*tot)
train_indices = sample(1:tot, n_train)
train_default = file[train_indices, ]
test_default = file[-train_indices, ]

# XGBoost

vars =  c("APP_ID","PRODUCTFLAG","Cibil_Score","ZONE","REGION","Sex","Marital_Status","Age","IRR","AMTFIN","CUSTOMER_CATG_DESC",
          "TENURE","BOARDING_RATE","ENTITY_TYPE","SOURCE","INDUSTRYID","INDUSTRY","INTRATE","INTTYPE","GROSS_LTV",
          "PURPOSE_DESC","DEALTYPE","RISKCATEGORY","CAS_SCORE","SECTORIAL_CLASSIFICATION","DBR","CONSTITUTION","BORROWERSEGMENT",
          "PROPERTY_TYPE","PROPERTY_VALUE","PROPERTY_TYPE_NEW","LAA_CALCULATED_INCOME","INTERESTTYPE","CITY","STATE",
          "Disdursed_Pct")

treatplan = designTreatmentsZ(train_default, vars)

newvars = treatplan %>%  use_series(scoreFrame) %>% filter(code %in% c("clean", "lev")) %>%
  use_series(varName)


train_default.treat = prepare(treatplan, train_default, varRestriction  = newvars)
test_default.treat = prepare(treatplan, test_default, varRestriction  = newvars)


train_default$Went.to.riskx.or.not = as.integer(train_default$Went.to.riskx.or.not) - 1

cv = xgb.cv(data = as.matrix(train_default.treat),
             label = train_default$Went.to.riskx.or.not,
             nrounds  = 100,
             nfold = 5,
             objective = "binary:logistic",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             gamma = 7,
             verbose = FALSE)

elog = as.data.frame(cv$evaluation_log)

elog %>% 
  summarize(ntrees.train = which.min(elog$train_error_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(elog$test_error_mean))  # find the index of min(test_rmse_mean)

ntrees = 14

child_weight = 0:50
gama = 0:20

hyper_grid = expand.grid(child_weight = child_weight, gama = gama)
aucs = c()
for(i in 1:nrow(hyper_grid)){
  xgb_model = xgboost(data = as.matrix(train_default.treat),
                       label = train_default$Went.to.riskx.or.not,
                       nrounds  = 39,
                       objective = "binary:logistic",
                       eta = 0.3,
                       max_depth = 6,
                       gamma = hyper_grid$gama[i],
                       min_child_weight = hyper_grid$child_weight[i],
                       verbose = FALSE) 
  
  
  pred = predict(xgb_model, as.matrix(test_default.treat))
  aucs[i] = auc(actual = test_default$Went.to.riskx.or.not, predicted = pred)
  
}

opt_i = which.max(aucs)

print(hyper_grid[opt_i,])

hyper_grid = cbind(hyper_grid, aucs)

xgb_model = xgboost(data = as.matrix(train_default.treat),
                     label = train_default$Went.to.riskx.or.not,
                     nrounds  = 39,
                     objective = "binary:logistic",
                     eta = 0.3,
                     max_depth = 6,
                     gamma = 3,
                     min_child_weight = 41,
                     verbose = FALSE) 

pred = predict(xgb_model, as.matrix(test_default.treat))
auc(actual = test_default$Went.to.riskx.or.not, predicted = pred)


head(pred)
pred_labels = ifelse(pred > 0.37, 1, 0)

confusionMatrix(as.factor(pred_labels), test_default$Went.to.riskx.or.not)

importance_matrix = xgb.importance(names(train_default.treat), xgb_model)
xgb.plot.importance(importance_matrix)


# Final Predictions

train_1 <- file[, c("APP_ID","PRODUCTFLAG","Cibil_Score","ZONE","REGION","Sex","Marital_Status","Age","IRR","AMTFIN","CUSTOMER_CATG_DESC",
                       "TENURE","BOARDING_RATE","ENTITY_TYPE","SOURCE","INDUSTRYID","INDUSTRY","INTRATE","INTTYPE","GROSS_LTV",
                       "PURPOSE_DESC","DEALTYPE","RISKCATEGORY","CAS_SCORE","SECTORIAL_CLASSIFICATION","DBR","CONSTITUTION","BORROWERSEGMENT",
                       "PROPERTY_TYPE","PROPERTY_VALUE","PROPERTY_TYPE_NEW","LAA_CALCULATED_INCOME","INTERESTTYPE","CITY","STATE",
                       "Disdursed_Pct")]


test_1.treat <- prepare(treatplan, riskx_test, varRestriction  = newvars)
train_1.treat <- prepare(treatplan, train_1, varRestriction  = newvars)

file$Went.to.riskx.or.not <- as.integer(file$Went.to.riskx.or.not) - 1


cv <- xgb.cv(data = as.matrix(train_1.treat),
             label = file$Went.to.riskx.or.not,
             nrounds  = 100,
             nfold = 5,
             objective = "binary:logistic",
             eta = 0.3,
             max_depth = 6,
             gamma = 3,
             min_child_weight = 41,
             verbose = FALSE)

elog <- as.data.frame(cv$evaluation_log)

elog %>% 
  summarize(ntrees.train = which.min(elog$train_error_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(elog$test_error_mean))  # find the index of min(test_rmse_mean)



xgb_model <- xgboost(data = as.matrix(train_1.treat),
                     label = file$Went.to.riskx.or.not,
                     nrounds  = 42,
                     objective = "binary:logistic",
                     eta = 0.4,
                     max_depth = 6,
                     gamma = 7,
                     min_child_weight = 35,
                     verbose = FALSE) 

pred <- predict(xgb_model, as.matrix(test_1.treat))


test_export <- cbind(riskx_test, pred) 
Went.to.riskx.or.not = ifelse( pred > .75, "Yes", "No")
output = cbind(riskx_test, Went.to.riskx.or.not)


write.csv(output, file = "RiskX_prediction.CSV", row.names = F)
