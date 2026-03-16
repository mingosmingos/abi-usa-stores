library(rminer)
library(rpart.plot)
library(tictoc)

ba=read.table("baltimore.csv",header=TRUE,sep=",")
la=read.table("lancaster.csv",header=TRUE,sep=",")

summary(ba)

acf(ba)

hist(ba$Pct_On_Sale)
plot(density(ba$Sales))
pie(table(ba$TouristEvent))

plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch="o",
     col=kmeans.result$cluster, cex=0.3)

hist(la$Pct_On_Sale)

barange=diff(range(ba$Num_Employees))
cat("range:",srange,"\n")

pairs(ba[,c("Num_Employees","Num_Customers","Pct_On_Sale","Sales")])
pairs(ba[,c("Num_Employees","Sales")])

boxplot(Sales ~ TouristEvent, data=la,
        col=c("lightgray","lightblue"),
        main="Sales during Tourist Events")

ba$Date = as.Date(ba$Date)
plot(ba$Date, ba$Sales, type="l",
     main="Sales Over Time",
     xlab="Date",
     ylab="Sales")

z = scale(ba$Sales)
outliers = which(abs(z) > 3)

ba[outliers,]