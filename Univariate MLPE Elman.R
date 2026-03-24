library(rminer)

# Based on first time series example.

ba=read.table("baltimore.csv",header=TRUE,sep=",")
NPRED=10 # number of predictions
summary(ba)

range=diff(range(ba$Num_Customers))
plot(ba$Num_Customers,type="l",col="blue")

acf(ba$Num_Customers)
pacf(ba$Num_Customers)

D=CasesSeries(ba$Num_Customers,c(1:6))
summary(D)
N=nrow(D)

NTR=N-NPRED
TR=1:NTR # training row elements of D (oldest elements), excluding last NPRED rows
TS=(NTR+1):N #  test row elements of D (more recent elements), total of NPRED rows
print("TR:")
print(TR)
print("TS:")
print(TS)

NN=fit(y~.,D[TR,],model="mlpe",search="heuristic")
RF=fit(y~.,D[TR,],model="randomForest",search="heuristic")

LTS=length(TS) # length of the test set
START=nrow(D)-LTS+1 # START is the row from D of the first test example

PNN=lforecast(NN,D,start=START,horizon=LTS)
PRF=lforecast(RF,D,start=START,horizon=LTS)

Y=D[TS,]$y # real observed values

# show forecasting measures and graph:
cat("NN (MLP) predictions:\n")
print(PNN)
cat("MAE:",mmetric(Y,PNN,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PNN,metric="NMAE",val=range),"\n")
cat("RMSE:",mmetric(Y,PNN,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PNN,metric="RRSE"),"\n")
cat("R2:",mmetric(Y,PNN,metric="R22"),"\n") # press R measure

cat("RF predictions:\n")
print(PRF)
cat("MAE:",mmetric(Y,PRF,metric="MAE"),"\n")
cat("NMAE:",mmetric(Y,PRF,metric="NMAE",val=range),"\n")
cat("RMSE:",mmetric(Y,PRF,metric="RMSE"),"\n")
cat("RRSE:",mmetric(Y,PRF,metric="RRSE"),"\n")
cat("R2:",mmetric(Y,PRF,metric="R22"),"\n") # press R measure

# graph: REG - simple Regression Plot
mae=mmetric(Y,PNN,metric="MAE")
nmae=mmetric(Y,PNN,metric="NMAE",val=range)
r2=mmetric(Y,PNN,metric="R22")
print("Graph with NN predictions (up to 10-ahead predictions):")
main=paste("NN pred. (MAE=",round(mae,digits=1),", NMAE=",round(nmae,digits=0),"%, R^2=",round(r2,digits=2),")",sep="")
mgraph(Y,PNN,main=main,graph="REG",Grid=10,lty=1,col=c("black","blue"),leg=list(pos="topright",leg=c("target","predictions")))

mae=mmetric(Y,PRF,metric="MAE")
nmae=mmetric(Y,PRF,metric="NMAE",val=range)
r2=mmetric(Y,PRF,metric="R22")
print("Graph with RF predictions (up to 10-ahead predictions):")
main=paste("RF pred. (MAE=",round(mae,digits=1),", NMAE=",round(nmae,digits=0),"%, R^2=",round(r2,digits=2),")",sep="")
mgraph(Y,PRF,main=main,graph="REG",Grid=10,lty=1,col=c("black","blue"),leg=list(pos="topright",leg=c("target","predictions")))

# Based on second time series example.

library(RSNNS)

rec_lforecast=function(RN,tinputs,horizon)
{
  Pred=vector(length=horizon)
  for (i in 1:horizon)
  {
    Pred[i]=as.numeric(predict(RN,tinputs[i,]))
    if(i<horizon) tinputs[i+1,1]=Pred[i]
  }
  return(Pred)
}

# MAX=250;MIN=0
MAX = max(ba$Num_Customers); MIN = min(ba$Num_Customers)
RANGE=(MAX-MIN)
Sba=(ba$Num_Customers-MIN)/RANGE
H=holdout(ba$Num_Customers,ratio=NPRED,mode="order")

lags=6
D=CasesSeries(Sba,c(1:lags))
summary(D)

N=nrow(D)
NTR=N-NPRED
TR=1:NTR
TS=(NTR+1):N

HD=holdout(D$y,ratio=NPRED,mode="order")

inputs=D[,1:lags]
output=D[,(lags+1)]

EL=elman(inputs[HD$tr,"lag1"],output[HD$tr],size=c(4,2),learnFuncParams=c(0.1),maxit=300)
plotIterativeError(EL)

Y=ba$Num_Customers[H$ts]

vector1=vector(length=NPRED)
vector1[1]=inputs[TS[1],"lag1"]
tinputs=data.frame(lag1=vector1)
PEL=rec_lforecast(EL,tinputs,horizon=length(HD$ts))

PEL=(PEL*RANGE)+MIN


D=CasesSeries(ba$Num_Customers,c(1:lags))
# fit a Neural Network (NN) - multilayer perceptron ensemble: 
NN=rminer::fit(y~.,D[HD$tr,],model="mlpe",search="heuristic") # 11 time lags
NN1=rminer::fit(y~.,D[HD$tr,c("lag1","y")],model="mlpe",search="heuristic")

START=nrow(D)-length(HD$ts)+1 # first test example from D
PNN=lforecast(NN,D,start=START,horizon=length(HD$ts))

# only 1 input is used, predict wants a data.frame or matrix (10x1):
D1=D[,c("lag1","y")] # lag1 = the input and y - the output
PNN1=lforecast(NN1,D1,start=START,horizon=length(HD$ts))

# show forecasting measures and graph:
cat("NN predictions: ")
cat("NMAE=",round(mmetric(Y,PNN,metric="NMAE",val=range),digits=1),"%\n")
cat("NN1 predictions: ")
cat("NMAE=",round(mmetric(Y,PNN1,metric="NMAE",val=range),digits=1),"%\n")

# graph:
print("Graph with NN predictions (multi step-ahead):")
# plot(1:length(Y),Y,ylim=c(min(PEL,PNN,Y),max(PEL,PNN,Y)),type="b",col="black")
plot(1:length(Y), Y, ylim=c(min(PEL,PNN,PNN1,PRF,Y), max(PEL,PNN,PNN1,PRF,Y)), type="b", col="black")
lines(PEL,type="b",col="blue",pch=2)
lines(PNN,type="b",col="red",pch=3)
lines(PNN1,type="b",col="green",pch=3)
lines(PRF, type="b", col="purple", pch=4)
legend("topright",c("Original","EL","NN","NN1","RF"),pch=c(1,2,3,3,4),col=c("black","blue","red","green","purple"))