#principal component analysis

prin_comp = prcomp(Dummy_data1,scale. = F)
names(prin_comp) = c("sdev","rotation","center","scale","x")


#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale


prin_comp$rotation

prin_comp$rotation[1:5,1:4]

dim(prin_comp$x)


biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained

prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


train.data <- data.frame(Dummy_data1 = file1$Went.to.riskx.or.not, prin_comp$x)

train.data = train.data[,1:30]

rpart.model <- rpart(Dummy_data1 ~ .,data = train.data, method = "anova")
rpart.model