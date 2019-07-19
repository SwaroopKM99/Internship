library("cluster")
library("factoextra")

book = read.csv("C:/Users/parasurampuram.h/Desktop/New folder/Book4.csv", header = T,
                sep = ",")
View(book)
str(book)

name = c("HYDERABAD","VIJAYAWADA","VIZAG","GURGAON","JANAKPURI","KAUSHAMBI","AHMEDABAD",
         "INDORE","RAJKOT","SURAT 2","VADODARA","BANGALORE","HUBLI","MYSORE","THANE",
         "VIRAR","JAIPUR","JODHPUR","KOTA","UDAIPUR","NAGPUR","NASHIK","PUNE","CHENNAI",
         "COIMBATORE","KUMBAKONAM","MADURAI","SALEM","TRICHY")
book$Branch = NULL
rownames(book) = name
str(book)

sbook = scale(book)
head(sbook)
summary(sbook)


km1 = kmeans(sbook, 3)
str(km1)

km1$cluster
km1$withinss
km1$tot.withinss
km1$totss
km1$betweenss

fviz_cluster(km1, data = sbook)

#Elbow method#
set.seed(111)
wss = 1:10
number = 1:10

for(i in 1:10)
{
  wss[i] = kmeans(sbook,i)$tot.withinss
}
plot(number, wss, xlab = "no. of cluster", pch = 19, type = "b",
     ylab = "total within - cluster sum of squares")

km2 = kmeans(sbook, 5)
km2

#library("cluster")
#library("factoextra")

fviz_cluster(km2, data = sbook)

book$cluster = km2$cluster

cr = cor(book)
corrplot(cr, method = "number", type = "lower")

final<-book

write.csv(final.csv, file = "clustering of march", row.names =T)
rm(final)

##############################################################################



swa = read.csv("C:/Users/parasurampuram.h/Desktop/New folder/swa.csv", header = T,
                sep = ",")
View(swa)

name = c("AHMEDABAD","BANGALORE","CHENNAI","COIMBATORE","GURGAON","HUBLI","HYDERABAD",
         "INDORE","JAIPUR","JANAKPURI","JODHPUR","KAUSHAMBI","KOTA","KUMBAKONAM",
         "MADURAI","MYSORE","NAGPUR","NASHIK","PUNE","RAJKOT","SALEM","SURAT 2",
         "THANE","TRICHY","UDAIPUR","VADODARA","VIJAYAWADA","VIRAR","VIZAG")

swa$Branch = NULL
rownames(swa) = name

sbook = scale(swa)
head(sbook)
summary(sbook)

km1 = kmeans(sbook, 3)
km1

km1$cluster
km1$withinss
km1$tot.withinss
km1$totss
km1$betweenss

fviz_cluster(km1, data = sbook)

#Elbow method#
set.seed(111)
wss = 1:10
number = 1:10

for(i in 1:10)
{
  wss[i] = kmeans(sbook,i)$tot.withinss
}
plot(number, wss, xlab = "no. of cluster", pch = 19, type = "b",
     ylab = "total within - cluster sum of squares")

km2 = kmeans(sbook, 4)
km2
fviz_cluster(km2, data = sbook)

km3 = kmeans(sbook, 5)
km3
fviz_cluster(km3, data = sbook)

km4 = kmeans(sbook, 6)
km4
fviz_cluster(km4, data = sbook)

swa$cluster = km4$cluster

cr = cor(swa)
corrplot(cr, method = "number", type = "lower")

final = swa

write.csv(final, file = "clustering_of_march.csv", row.names =T)

##################################################################################


riskx = read.csv("C:/Users/parasurampuram.h/Documents/riskx.csv", header = T,
               sep = ",")
View(riskx)
str(riskx)


name = c("AHMEDABAD","BENGALURU","CHENNAI","CONNAUGHT","INDORE","JAIPUR","JANAKPURI",
         "JODHPUR","KOTA","LAXMI NAGAR","LUDHIANA","PUNE","RAJKOT","SURAT","SURAT 2",
         "UDAIPUR","VADODARA","VAPI")
rownames(riskx) = name
riskx$Branches = NULL

scaled_riskx = scale(riskx)
View(scaled_riskx)

km1 = kmeans(scaled_riskx, 3)
km1

km1$cluster
km1$withinss
km1$tot.withinss
km1$totss
km1$betweenss

fviz_cluster(km1, data = scaled_riskx)

#Elbow method#
set.seed(111)
wss = 1:10
number = 1:10

for(i in 1:10)
{
  wss[i] = kmeans(scaled_riskx,i)$tot.withinss
}
plot(number, wss, xlab = "no. of cluster", pch = 19, type = "b",
     ylab = "total within - cluster sum of squares")

km2 = kmeans(scaled_riskx, 6)
km2
fviz_cluster(km2, data = scaled_riskx)











