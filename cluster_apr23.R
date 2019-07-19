setwd("C:/Users/swaroop.mondal/Documents/R-Studio")
savehistory("C:/Users/swaroop.mondal/Documents/R-Studio/work.Rhistory")


apr23 = read.csv("C:/Users/swaroop.mondal/Documents/R-Studio/cluster_swaroop.csv", header = T, sep = ",")

#create_report(apr23)

names = apr23$X
apr23$X = NULL
rownames(apr23) = names

summary(apr23)
dim(apr23)
str(apr23)
View(apr23)


sresolution = scale(apr23)
head(sresolution)
head(apr23)

km1 = kmeans(sresolution, 3)
str(km1)

km1$cluster
km1$withinss
km1$tot.withinss
km1$totss
km1$betweenss

fviz_cluster(km1, data = sresolution)

#Elbow method#
set.seed(111)
wss = 1:10
number = 1:10

for(i in 1:10)
{
  wss[i] = kmeans(sresolution,i)$tot.withinss
}
plot(number, wss, xlab = "no. of cluster", pch = 19, type = "b",
     ylab = "total within - cluster sum of squares")

km2 = kmeans(sresolution, 4)
km2

fviz_cluster(km2, data = sresolution)

km3 = kmeans(sresolution, 7)
km3

fviz_cluster(km3, data = sresolution)

km4 = kmeans(sresolution, 8)
km4

fviz_cluster(km4, data = sresolution)


apr23$cluster = km3$cluster

write.csv(apr23, file = "clustering of april.csv", row.names =T)








