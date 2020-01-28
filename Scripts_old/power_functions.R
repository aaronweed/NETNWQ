# S. Hafner
# Modified: 2014 APR 3

# Calculates power or required sample size for detecting trends in some variable over time, using Peter Dalgaard's power.t.test function
regPwrOne<-function(
  mn,                   # Mean response
  s,                    # Standard deviation among sampling points
  se,                   # Or just provide standard error of the mean
  n.pt=NA,              # Number of points sampled each year
  alpha=0.05,           # Alpha, i.e., P of type I error
  n.yr.period=10,       # Period over which detection is taking place
  n.yr.samp,            # Number of sampled years, including start year
  samp.int=NA,          # Or, specify a sampling interval for regular samples
  achange=rchange*mn,   # Absolute change over these years
  rchange=achange/mn,   # Relative change over these years
  pwr=NA,               # Required power, i.e., 1 - beta
  nsigd=3               # Number of significant digits to return
) {

  # Check for achange of zero
  if(achange==0) return(NA)

  # Select sampled years
  if(is.na(samp.int)) {
    syrs<-round(seq(0,n.yr.period,length.out=n.yr.samp))
  } else {
    syrs<-seq(0,n.yr.period,samp.int)
    n.yr.samp<-length(syrs)
  }

  # Calculate power using power.t.test
  if(!is.na(n.pt)|!is.na(se)) {
    # Calculate the se of the slope estimate
    if(is.na(se)) se<-s/sqrt(n.pt) # Standard error of the mean--sd of the mean if sampled over multiple years (when constant)
    sse<-se/(sqrt(sum((syrs - mean(syrs))^2))) # Standard error of the slope estimate, assuming a linear trend
    # Check for high cv
    if(sse/achange/n.yr.period>100) return(NA)
    pwr<-power.t.test(n=n.yr.samp-1,delta=achange/n.yr.period,sd=sse*sqrt(n.yr.samp - 1),sig.level=alpha,type='one.sample')$power
    return(signif(pwr,nsigd))
  }
  if(!is.na(pwr)) {
    tsd<-power.t.test(n=n.yr.samp-1,delta=achange/n.yr.period,sd=NULL,sig.level=alpha,power=pwr,type='one.sample')$sd
    sse<-tsd/(sqrt(n.yr.samp - 1))
    se<-sse*(sqrt(sum((syrs - mean(syrs))^2)))
    n.pt<-ceiling((s/se)^2)
    return(n.pt)
  } else stop('Must provide either n.pt (or se), or pwr')
  # DOUBLE-CHECK N AND SD

}

# Vectorizes regPwr, repeating values when needed
# Possibly this could be done more efficiently.
regPwr<-function(
  mn,
  s=NA,
  se=NA,
  n.pt=NA,
  alpha=0.05,
  n.yr.period=10,
  n.yr.samp=NA,
  samp.int=NA,
  achange=rchange*mn,
  rchange=achange/mn,
  pwr=NA,
  nsigd=3
) {

  # Determine the length of the longest argument
  nr<-max(len.args<-sapply(arglist<-list(mn=mn,s=s,se=se,n.pt=n.pt,alpha=alpha,n.yr.period=n.yr.period,n.yr.samp=n.yr.samp,samp.int=samp.int,achange=achange,rchange=rchange,pwr=pwr,nsigd=nsigd),length))

  # Check for more than one length > 1
  if(length(unique(len.args[len.args>1]))>1) stop('Check your argument list--there are arguments with length > 1 but less than the maximum (',nr,')\n The error may be in ',names(arglist)[len.args>1 & len.args<nr])

  # Find which arguments are short
  which.short<-which(sapply(arglist,length)==1)

  # And repeat them
  for(i in which.short) {
    arglist[[i]]<-rep(arglist[[i]][1],nr)
  }

  # Call up regPwrOne for each set of argument values
  res<-numeric(nr)
  for(i in 1:nr) {
    res[i]<-do.call("regPwrOne",lapply(arglist,`[[`,i))
  }

  return(res)

}


# Calculates power for detecting a linear trend in a response by generating simulated data
simPwr<-function(
  mn,
  se,
  alpha=0.05,
  nyr=10,           # Difference in years
  nsamp=4,          # Number of sampled years, including start
  rchange,          # Relative change over these years
  ntrial=100,
  method='rnorm',   # 'rnorm','arima.sim'
  plot=FALSE
) {

  # Empty vector for detection results
  det<-logical(ntrial)

  # Select years
  # There are other ways to position these
  syrs<-round(seq(0,nyr,length.out=nsamp))

  for(trial in 1:ntrial) {

    if(method=='rnorm') noise<-rnorm(nsamp,0,se) else if(method=='arima.sim') noise<-c(arima.sim(list(ar=c(0.4,0.2),ma=0),nsamp,sd=se))
    y<-rchange*mn/nyr*syrs + mn + noise

    # Truncate y--crude and should be possible to improve with appropriate interpretation of se of lambda
    #y[y<0]<-0
    if(any(y<0)) y<-y - min(y)

    # Check sign and attempt to detect trend with lm
    if(method=='rnorm') {
      mod<-lm(y ~ syrs)
      trend<-sign(coef(mod)['syrs'])
      pval<-summary(mod)$coeff['syrs',4]
    } else
      if(method=='arima.sim') {
        mod<-gls(y ~ syrs, correlation=corARMA(q=1))
        trend<-sign(coef(mod)['syrs'])
        pval<-summary(mod)$tTable['syrs',4]
      }
    if(trend==sign(rchange)) det[trial]<-pval < alpha

    # Plot, if requested
    if(plot) {
      plot(syrs,y,xlab='Year',ylab='Response',las=1)
      abline(mod)
      legend('topleft',as.character(signif(summary(mod)$coeff['syrs',4],3)),pch=0,bty='n')
    }
  }

  return(sum(det)/ntrial)

}

# Vectorizes simPwr
# Probably this could be done more efficiently.
# See simPwr for more information
simPwrV<-function(
  mn,
  se,
  alpha=0.05,
  nyr=10,
  nsamp=4,
  rchange,
  ntrial=100,
  method='rnorm',
  plot=FALSE
) {

  n<-max(sapply(arglist<-list(mn=mn,s=s,alpha=alpha,nyr=nyr,nsamp=nsamp,rchange=rchange,ntrial=ntrial,method=method,pdf=pdf),length))

  which.short<-which(sapply(arglist,length)==1)

  for(i in which.short) {
    arglist[[i]]<-rep(arglist[[i]][1],n)
  }

  pwr<-numeric(n)
  if(plot) {
    pdf(paste0('reg_plots',Sys.time(),'.pdf'),height=11,width=8.5)
    par(mfrow=c(4,3))
  }
  for(i in 1:n) {
    pwr[i]<-do.call("simPwr",lapply(arglist,`[[`,i))
  }
  if(plot) dev.off()

  return(pwr)

}

# Summary function for data frames
dfsumm<-function(x) {
   if(!class(x)[1]%in%c("data.frame","matrix"))
     stop("You can't use dfsumm on ",class(x)," objects!")
   cat("\n",nrow(x),"rows and",ncol(x),"columns")
   cat("\n",nrow(unique(x)),"unique rows\n")
   s<-matrix(NA,nrow=7,ncol=ncol(x))
   for(i in 1:ncol(x)) {
     iclass<-class(x[,i])[1]
     s[1,i]<-paste(class(x[,i]),collapse=" ")
     y<-x[,i]
     yc<-na.omit(y)
     if(iclass%in%c("factor","ordered"))
       s[2:3,i]<-levels(yc)[c(1,length(levels(yc)))] else
     if(iclass=="numeric")
       s[2:3,i]<-as.character(signif(c(min(yc),max(yc)),3)) else
     if(iclass=="logical")
       s[2:3,i]<-as.logical(c(min(yc),max(yc))) else
       s[2:3,i]<-as.character(c(min(yc),max(yc)))
     if(iclass%in%c("numeric","integer"))
       s[4,i]<-signif(mean(yc),3) else
       s[4,i]<-NA
     s[5,i]<-length(unique(yc))
     s[6,i]<-sum(is.na(y))
     s[7,i]<-!is.unsorted(yc)
   }
   s<-as.data.frame(s)
   rownames(s)<-c("Class","Minimum","Maximum","Mean","Unique (excld. NA)","Missing values","Sorted")
   colnames(s)<-colnames(x)
   print(s)
}

#summary functions
#summary function creates table of n, mean, var, SD, and SE
summaryFunction <- function(DataIn, factor, response){
  require(plyr)
  summaryOut <- ddply(DataIn, factor, .fun = function(xx){
    c(n = length(xx[,response]),
      mean = mean(xx[,response],na.rm=TRUE),
      var = var(xx[,response],na.rm=TRUE),
      SD = sd(xx[,response],na.rm=TRUE),
      SE = sqrt(var(xx[,response])/length(xx[,response])))
  })
  return(summaryOut)
  dev.off()
}



#Function to get summary of abundance by park
parkSummaryFunction.2 <- function(DataIn, factor, response){
  newData<-summaryFunction(DataIn=b,factor=factor, response=response)

  mean=ddply(DataIn,factor, summarize,
             mean(Predicted))
  SE=ddply(DataIn ,factor,summarize,
           SE=mean(SE))
  newTable<-data.frame(newData[,1:3], SE[,2])

  #newTable$SD<-newTable$SE*sqrt(newTable$n)
  colnames(newTable)<-c(as.character(factor), "n", "Predicted", "SE")

  colnames(newTable)<-c(as.character(factor), "n.years","Predicted","SE")
  newTable.out<-subset(newTable, n.years==1)
  return(newTable.out)
}
