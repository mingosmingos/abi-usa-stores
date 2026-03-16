library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

fshow=function(Y,Pred1,Pred2,Pred3,method,name1,name2,name3)
{
  par(mfrow = c(1, 3)) # three graphs inside a plot
  
  mae=round(mmetric(Y[,1],Pred1,metric="MAE"),1)
  main=paste(method," ",name1," (MAE=",mae,")",sep="")
  mgraph(Y[,1],Pred1,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target",method)))
  
  mae=round(mmetric(Y[,2],Pred2,metric="MAE"),1)
  main=paste(method," ",name2," (MAE=",mae,")",sep="")
  mgraph(Y[,2],Pred2,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target",method)))
  
  mae=round(mmetric(Y[,3],Pred3,metric="MAE"),1)
  main=paste(method," ",name3," (MAE=",mae,")",sep="")
  mgraph(Y[,3],Pred3,main=main,graph="REG",Grid=10,col=c("black","blue"),leg=list(pos="topleft",leg=c("target",method)))
}

ba=read.table("baltimore.csv",header=TRUE,sep=",")
ba$Date = as.Date(ba$Date)
ba$TouristEvent <- ifelse(ba$TouristEvent == "Yes",1,0)
colSums(is.na(ba))
ba <- na.omit(ba)

K=4 # seasonal frequency: 4 time periods per year
LTS=K #  1 year, used for the forecasting range, thus 4 forecasts
toureven=ba[,"TouristEvent"] # employment
sale=ba[,"Pct_On_Sale"] # productivity
cust=ba[,"Num_Customers"]   # real wage

hd=holdout(sale,ratio=LTS,mode="order")
cdata=cbind(toureven,sale,cust)
mtr=ts(cdata[hd$tr,],frequency=K) # TS training object, uses forecast library model
Y=cdata[hd$ts,] # target values

mvar=autoVAR(mtr,season=K) # season is the seasonal period
# get multi-step ahead forecasts
FV=forecastVAR(mvar,h=LTS) # similar to the forecast library function, multi-step ahead forecasts
Pred1=FV[[1]] # predict e
Pred2=FV[[2]] # predict prod
Pred3=FV[[3]] # predict rw
fshow(Y,Pred1,Pred2,Pred3,"VAR","toureven","sale","cust")

summary(ba)

mtr <- as.matrix(ba[,c("Sales")])
exogen <- as.matrix(ba[,c("Date","Num_Employees","Pct_On_Sale","TouristEvent")])

# Predictions

LTS <- 30

hd <- holdout(mtr[,1], ratio=LTS, mode="order")

train_endog <- mtr[hd$tr,]
test_endog  <- mtr[hd$ts,]

train_exog <- exogen[hd$tr,]
test_exog  <- exogen[hd$ts,]