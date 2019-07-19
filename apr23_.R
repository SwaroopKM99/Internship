

apr23 = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/apr23.csv", header = T, sep = ",")


#install.packages("CGPfunctions")
library(CGPfunctions)
#install.packages("devtools")
library(devtools)
View(apr23)
str(apr23)

#apr23$Day<-as.factor(apr23$Day)
apr23$Bucket<-as.factor(apr23$Bucket)
#apr23$Resolution<-round(apr23$Resolution,digits=2)

apr23$Day <- factor(apr23$Day,levels = c("2019-04-23", "2019-03-23", "2019-02-23", "2019-01-23", "2018-12-23"),
                         labels = c("April","March","Februray","January", "December"), ordered = TRUE)

# SLOPE CHART

newggslopegraph(dataframe =apr23,
                Times =Day,
                Measurement =Resolution,
                Grouping =Bucket,
                Title = "Last 5 months 23rd day",
                SubTitle = "Change in Absolute values of resolution",
                Caption = "RHDFC DATA ANALYTICS"
)









