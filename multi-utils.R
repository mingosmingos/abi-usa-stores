# auxiliary conversion functions

conv_ts=function(x,frequency=1,labels=NULL)
{
  if(is.vector(x)) { x=ts( as.matrix(cbind(x)), frequency=frequency )
                     if(is.null(labels)) labels="x_1"
                   }
  else if(is.data.frame(x) || is.matrix(x)) 
  { 
    if(is.null(labels) && !is.null(names(x))) labels=names(x) else labels=paste("x_",1:ncol(x),sep="")
    if(is.data.frame(x)) x=as.matrix(x) 
    x=ts(x, frequency=frequency)
    
  }
  attr(x,"dimnames")[[2]]=labels
  return(x)
}

## autoVAR function - create a VAR model from a mtr training data
# mtr - matrix with the training data, one column per to be predicted variable.
# lag.max - maximum lag order considered, default to NULL
# type - type of deterministic regressors to include.
# season - inclusion of centered seasonal dummy variables (integer value of frequency).
# exogen - inclusion of exogenous variables (matrix).
autoVAR=function(mtr,lag.max=10,type="const",season=NULL,exogen=NULL)
{  
  if(!is.ts(mtr)) mtr=conv_ts(mtr)
  if(!is.null(exogen)) 
  {
    if(!is.ts(exogen)) exogen=conv_ts(exogen)
    mvar=VAR(mtr,type=type,season=season,exogen=exogen,lag.max=lag.max,ic="SC") # BIC
  }
  else mvar=VAR(mtr,type=type,season=season,lag.max=lag.max,ic="SC") # BIC
  return(mvar) # returns a VAR model
}

## forecastVAR - get multi-step ahead forecasts for all mtr variables
# model - VAR model
# h - horizon
# exogen - matrix with exogenous data for the predict period
forecastVAR=function(model,h=10,exogen=NULL)
{
  models=length(model$varresult)
  res=vector("list",length=models)
  #cat("h:",h,"\n")
  if(!is.null(exogen)) 
  { 
    if(!is.ts(exogen)) exogen=conv_ts(exogen)
  }
  F1=predict(model,n.ahead=h,dumvar=exogen) # multi-step ahead forecasts
  for(i in 1:models) res[[i]]=as.numeric(F1$fcst[[i]][,1]) 
  return(res)  # res is a vector list of length ncol(mtr)
}
#------------------------------------------------------------------------------

### Recipe for ARIMAX  -------------------------------------
## autoARIMAX function - create an ARIMAX model from a mtr training data
# mtr - is a ts matrix with the training data, one column per to be predicted variable
# frequency - if 1, no seasonal model is assumed, if > 1, SARIMA is assumed.
# exogen - inclusion of exogenous variables (matrix).
autoARIMAX=function(mtr,frequency=1,exogen=NULL)
{
  if(!is.ts(mtr)) mtr=conv_ts(mtr)
  models=ncol(mtr)
  nr=nrow(mtr)
  nmtr=attr(mtr,"dimnames")[[2]]
  
  # create one arimax model per variable
  res=vector("list",length=models) # one arimax per variable
  res2=vector("list",length=models) # one arimax per variable
  
  if(!is.null(exogen)) 
  { 
    if(!is.ts(exogen)) exogen=conv_ts(exogen,frequency=frequency)
  }
  
  for(i in 1:models)
  {
    if(frequency>1) s_tr=ts(mtr[1:nr,i],frequency=frequency) else s_tr=ts(mtr[1:nr,i])
    # arimax:
    xreg=ts(mtr[1:nr,-i],frequency=frequency)
    if(!is.null(exogen)) xreg=cbind(xreg,exogen)
    attr(xreg,"dimnames")[[2]]=c(nmtr[-i],attr(exogen,"dimnames")[[2]])
    res[[i]]=auto.arima(s_tr,xreg=xreg)
    # pure arima: (heuristic)
    res2[[i]]=auto.arima(s_tr)
  }
  
  # returns a list with arimax=res and arima=res2
  return(list(arimax=res,arima=res2,frequency2=frequency))
}  

## forecastARIMAX - get multi-step ahead forecasts for all mtr variables
# model - arimax model
# h - horizon
# exogen - matrix with exogenous data for the predict period
forecastARIMAX=function(model,h=10,exogen=NULL)
{
  models=length(model$arimax)
  res=vector("list",length=models)
  # get xreg estimates: heuristic -> pure auto.arima for each variable
  xreg=matrix(ncol=models,nrow=h)
  for(i in 1:models) xreg[,i]=forecast(model$arima[[i]],h=h)$mean
 
  if(!is.null(exogen)) 
  { 
    if(!is.ts(exogen)) exogen=conv_ts( exogen,frequency=model$frequency2)
  }
  
  # get arimax forecasts:  
  for(i in 1:models)
  {
    # if needed, convert xaux to matrix:
    if(length(xreg[1,-i])==1) {xaux=matrix(ncol=1,nrow=h);xaux[,1]=xreg[,-i]}  
    else xaux=as.matrix(xreg[,-i]) # already is a matrix
    narimax=names(model$arimax[[i]]$coef)
    xaux=ts(xaux,frequency=model$frequency2)
  
    if(!is.null(exogen)) 
      { xaux=as.matrix(cbind(xaux,exogen))
      }
    attr(xaux,"dimnames")[[2]]=narimax[(length(narimax)-ncol(xaux)+1):length(narimax)]
    
    F1=forecast(model$arimax[[i]],h=h,xreg=xaux)
    res[[i]]=as.numeric(F1$mean)
  }
  return(res)
}

#------------------------------------------------------------------------------
# auxiliary function
vreverse=function(x) x[length(x):1]

### Recipe for "entwined" Machine Learning models:
# mtr - matrix with the training data, one column per to be predicted variable.
# model -- rminer fit model.
# VINP -- vector list with time lags for each variable, see demo example.
#         new note: VINP works any subset of time lags :)
# exogen - inclusion of exogenous variables (matrix).
mfit=function(mtr,model,VINP,exogen=NULL)
{
   # mtr is matrix or data.frame
  nmodels=ncol(mtr) # number of predicted models
  # get maximum lag!
  lagmax=1
  for(i in 1:nmodels) 
    for(j in 1:nmodels) 
      lagmax=max(lagmax,VINP[[i]][[j]])
  
  mdata=vector("list",length=nmodels) # get all lags from all multi-series:
  for(i in 1:nmodels) mdata[[i]]=CasesSeries(mtr[,i],1:lagmax)
  
  # process if needed exogen:
  if(!is.null(exogen))
    { 
     NR=nrow(mtr)
     if(is.vector(exogen))
     { if(NR==length(exogen)) exogen=cbind(exogen[(lagmax+1):NR])
       else exogen=cbind(exogen)
     }
     else if(NR==nrow(exogen)) exogen=exogen[(lagmax+1):NR,]
     exogen=as.data.frame(exogen)
    }
  
  # create mdata2 datasets:
  mdata2=vector("list",length=nmodels)
  for(i in 1:nmodels) # 1 dataset per variable 
  {  
    for(j in 1:nmodels) # cycle all lags for VINP
    { if(j==1) 
      { 
        mnames=names(mdata[[j]])
        inames=paste("lag",VINP[[i]][[j]],sep="")
        I=NULL
        for(k in inames) I=c(I,which(mnames==k))
        I=sort(I)
        D=mdata[[j]][,I]
        if(is.vector(D)) 
          { D=data.frame(cbind(D))
          }
       NCD=ncol(D)
       dnames=vreverse(inames)
       ini=1;end=ini+length(dnames)-1
       names(D)[ini:end]=paste("x",j,dnames,sep="")
      }
      else { NCD=ncol(D)
             inames=paste("lag",VINP[[i]][[j]],sep="")
             dnames=vreverse(inames)
             D=cbind(D,mdata[[j]][,dnames])
             ini=NCD+1;end=ini+length(dnames)-1
             names(D)[ini:end]=paste("x",j,dnames,sep="")
           }
    }
    if(!is.null(exogen)) D=cbind(D,exogen,y=mdata[[i]]$y)
    else D=cbind(D,y=mdata[[i]]$y)
    mdata2[[i]]=D
  }
  # train all ML models:
  mmodels=vector("list",length=nmodels)
  for(i in 1:nmodels) # 1 dataset per variable 
  { 
    mmodels[[i]]=fit(y~.,mdata2[[i]],model=model)
  }
  return(list(mdata=mdata2,mmodels=mmodels,vinp=VINP))
}

## lforecastm - get multi-step ahead forecasts for all mtr variables
# model - mfit model -- multi-variate ML model (several ML models, one per variable)
# h - horizon
# exogen - matrix with exogenous data for the predict period
lforecastm=function(model,h=10,exogen=NULL)
{
  models=length(model$mmodels)
  # model$mmodels
  # model$mdata
  # model$vinp
  
  # init res
  res=vector("list",length=models)
  for(j in 1:models) res[[j]]=vector(length=h)
  
  if(!is.null(exogen) && is.vector(exogen)) exogen=cbind(exogen)
  exogen=as.data.frame(exogen)
  if(!is.null(exogen)) { nexo=names(exogen) }
  
  # init VEX
  VEX=vector("list",length=models)
  vnames=vector("list",length=models)
  VI=vector("list",length=models)
  for(i in 1:h)
  { 
    for(j in 1:models)
    { start=nrow(model$mdata[[j]])-h+1
      VEX[[j]]=model$mdata[[j]][start+i-1,]
      # change VEX if needed:
      if(i>1){
        for(k in 1:models)
        {
          VI[[k]]=intersect( 1:(i-1), model$vinp[[j]][[k]] )  
          vnames[[k]]=paste("x",k,"lag",VI[[k]],sep="")
          vnames[[k]]=vreverse(vnames[[k]])
          # compute right the res values:
          VI[[k]]=as.numeric(VI[[k]])
          VEX[[j]][,vnames[[k]] ]=res[[k]][ (i-1) - vreverse(VI[[k]]) +1 ]
        }   
      }
      #cat("i:",i,"j:",j,"\n")
      #print(VEX[[j]])
      if(!is.null(exogen)) { INP=VEX[[j]]
                             INP[,nexo]=exogen[i,]
                           }
      else INP=VEX[[j]]    
      #print(INP)
      res[[j]][i]=predict(model$mmodels[[j]],INP)
    } # for j
  } # i horizon cycle
  return(res)
} 
#-------------------------------