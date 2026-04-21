### hill.R file ###

# pure hill climbing:
#    par - initial solution
#    fn - evaluation function
#    change - function to generate the next candidate 
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    control - list with stopping and monitoring method:
#       $maxit - maximum number of iterations
#       $REPORT - frequency of monitoring information
#       $digits - (optional) round digits for reporting
#    type - "min" or "max"
#    ... - extra parameters for fn
hclimbing=function(par,fn,change,lower,upper,control,
                   type="min",...)
{ fpar=fn(par,...)
for(i in 1:control$maxit) 
{ 
  par1=change(par,lower,upper) 
  fpar1=fn(par1,...)
  if(control$REPORT>0 &&(i==1||i%%control$REPORT==0)) 
    report_iter(i,par,fpar,par1,fpar1,control)
  b=best(par,fpar,par1,fpar1,type) # select best
  par=b$par;fpar=b$fpar
}
if(control$REPORT>=1) 
  report_iter("best:",par,fpar,control=control)
return(list(sol=par,eval=fpar))
}

# report iteration details:
# i, par, fpar, par1 and fpar1 are set in the search method
# control - list with optional number of digits: $digits
report_iter=function(i,par,fpar,par1=NULL,fpar1=NULL,control)
{ 
  if(is.null(control$digits)) digits=2 # default value
  else digits=control$digits
  if(i=="best:") cat(i,round(par,digits=digits),"f:",round(fpar,digits=digits),"\n")
  else cat("i:",i,"s:",round(par,digits=digits),"f:",round(fpar,digits=digits),
           "s'",round(par1,digits=digits),"f:",round(fpar1,digits=digits),"\n")
}

# slight random change of vector par: (improved function @2024)
#    par - initial solution
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    dist - random distribution function
#    operator - "+" additive change or "*" multiplicative change 
#    round - use integer (TRUE) or continuous (FALSE) search
#    ... - extra parameters for dist
#    examples of parameters: additive change: dist=rnorm, mean=0, sd=0.5, lower=0, upper=1
#              multiplicative (% change): dist=rnorm, mean=1, sd=0.1, lower=0, upper=1
hchange=function(par,lower,upper,operator="+",dist=rnorm,round=FALSE,...)
{ D=length(par) # dimension
step=dist(D,...) # slight step
if(operator=="+") 
{ if(round) par1=par+round(step) else par1=par+step }
else if(operator=="*") 
{
  I=which(par==0) # 0 x something is always 0, thus an additive perturbation is needed
  par[I]=par[I]+dist(length(I),0,0.05) # median 0, small sd.
  par1=par*step
  if(round) par1=round(par1)
}
# return par1 within [lower,upper]:
return(ifelse(par1<lower,lower,ifelse(par1>upper,upper,par1)))
}

# return best solution and its evaluation function value
#    par - first solution
#    fpar - first solution evaluation 
#    par2 - second solution
#    fpar2 - second solution evaluation 
#    type - "min" or "max"
#    fn - evaluation function
#    ... - extra parameters for fn
best=function(par,fpar,par2,fpar2,type="min",...)
{ if(   (type=="min" && fpar2<fpar) 
        || (type=="max" && fpar2>fpar)) { par=par2;fpar=fpar2 }
  return(list(par=par,fpar=fpar))
}
