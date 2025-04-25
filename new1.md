---
title: "Weibull-DMMLE"
output: html_document
date: "2025-04-22"
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r packgages}
#install.packages('latex2exp')
library(latex2exp)
```


## 3-parameter Weibull: PDF and CDF

```{r pdf}
#################################################
### PDF
#param=(shape, scale, location)
dweibull3p=function(x, param){
  shape=param[1]
  scale=param[2]
  loc=param[3]
  out=ifelse(x>loc, (shape/scale)*((x-loc)^(shape-1))*exp(-(1/scale)*(x-loc)^shape), 0)
  return(out)
}


dweibull3p(1, c(1,1,1))
dweibull3p(1.5, c(1,1,1))
dweibull3p(seq(0,2,0.05), c(1,1,1))
```


```{r cdf}
#################################################
####### CDF

#param=(shape, scale, location)
pweibull3p=function(x, param){
  shape=param[1]
  scale=param[2]
  loc=param[3]
  out=ifelse(x<=loc, 0,  1-exp(-(1/scale)*(x-loc)^shape))
  return(out)
}


pweibull3p(1, c(1,1,1))
pweibull3p(1.5, c(1,1,1))
pweibull3p(seq(0,2,0.5), c(1,1,1))
```


## Including Plots for PDF and CDF
```{r plota}
####################################################
x_points=seq(0,8, 0.1)


############################################
### variation of shape parameter
par1_2_1=c(1, 4, 0)
par2_2_1=c(2, 4, 0)
par3_2_1=c(3, 4, 0)
par4_2_1=c(4, 4, 0)
par5_2_1=c(5, 4, 0)


g1_2=dweibull3p(x_points,par1_2_1)
g2_2=dweibull3p(x_points,par2_2_1)
g3_2=dweibull3p(x_points,par3_2_1)
g4_2=dweibull3p(x_points,par4_2_1)
g5_2=dweibull3p(x_points,par5_2_1)

par(cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
plot(x_points, g1_2,
     ylim = c(0,2),
     main=TeX("$sigma=4, mu=0$"),
     ylab = TeX("$f(x)$"),xlab ="x" ,type="l",
     pch=15, lty=1, lwd=2.0)


lines(x_points, g2_2, type="l", 
      pch=16, 
      lty=2, 
      lwd=2.0,
      col="red") 

lines(x_points, g3_2, type="l", 
      pch=17, 
      lty=3, 
      lwd=2.0,
      col="blue") 
lines(x_points, g4_2, type="l", 
      pch=18, 
      lty=4, 
      lwd=2.0,
      col="gray") 
lines(x_points, g5_2, type="l", 
      pch=19, 
      lty=5, 
      lwd=2.0,
      col="green") 

legend("topright", 
       y=0.5,
       title=TeX("$alpha$"), 
       c(1,2,3,4,5), 
       lty=1:5, col=c("black",  "red","blue", "gray", "green"),
       cex=1.5) 



g1_2=pweibull3p(x_points,par1_2_1)
g2_2=pweibull3p(x_points,par2_2_1)
g3_2=pweibull3p(x_points,par3_2_1)
g4_2=pweibull3p(x_points,par4_2_1)
g5_2=pweibull3p(x_points,par5_2_1)




plot(x_points, g1_2,
     ylim = c(0,1),
     main=TeX("$sigma=4, mu=0$"),
     ylab = TeX("$F(x)$"),xlab ="x" ,type="l",
     pch=15, lty=1, lwd=2.0)


lines(x_points, g2_2, type="l", 
      pch=16, 
      lty=2, 
      lwd=2.0,
      col="red") 
lines(x_points, g3_2, type="l", 
      pch=17, 
      lty=3, 
      lwd=2.0,
      col="blue") 
lines(x_points, g4_2, type="l", 
      pch=18, 
      lty=4, 
      lwd=2.0,
      col="gray") 
lines(x_points, g5_2, type="l", 
      pch=19, 
      lty=5, 
      lwd=2.0,
      col="green") 

legend("bottomright", #inset=.05,
       y=0.5,
       title=TeX("$alpha$"), 
       c(1,2,3,4,5), 
       lty=1:5, col=c("black",  "red","blue", "gray", "green"),
       cex=1.5
)


```

```{r plots}
############################################
### variation of scale parameter
par1_2_2=c(2, 2, 0)
par2_2_2=c(2, 4, 0)
par3_2_2=c(2, 6, 0)
par4_2_2=c(2, 8, 0)
par5_2_2=c(2, 10, 0)


g1_2=dweibull3p(x_points,par1_2_2)
g2_2=dweibull3p(x_points,par2_2_2)
g3_2=dweibull3p(x_points,par3_2_2)
g4_2=dweibull3p(x_points,par4_2_2)
g5_2=dweibull3p(x_points,par5_2_2)


# Ajuste o tamanho da fonte
par(cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
plot(x_points, g1_2,
     ylim = c(0,1),
     main=TeX("$alpha=2, mu=0$"),
     ylab = TeX("$f(x)$"),xlab ="x" ,type="l",
     pch=15, lty=1, lwd=2.0)


lines(x_points, g2_2, type="l", 
      pch=16, 
      lty=2, 
      lwd=2.0,
      col="red") 
lines(x_points, g3_2, type="l", 
      pch=17, 
      lty=3, 
      lwd=2.0,
      col="blue") 
lines(x_points, g4_2, type="l", 
      pch=18, 
      lty=4, 
      lwd=2.0,
      col="gray") 
lines(x_points, g5_2, type="l", 
      pch=19, 
      lty=5, 
      lwd=2.0,
      col="green") 


legend("topright", #inset=.05,
       y=0.5,
       title=TeX("$sigma$"), 
       c(2,4,6,8, 10), 
       lty=1:5, col=c("black",  "red","blue", "gray", "green"),
       cex=1.5 
)


g1_2=pweibull3p(x_points,par1_2_2)
g2_2=pweibull3p(x_points,par2_2_2)
g3_2=pweibull3p(x_points,par3_2_2)
g4_2=pweibull3p(x_points,par4_2_2)
g5_2=pweibull3p(x_points,par5_2_2)


plot(x_points, g1_2,
     ylim = c(0,1),
     main=TeX("$alpha=2, mu=0$"),
     ylab = TeX("$F(x)$"),xlab ="x" ,type="l",
     pch=15, lty=1, lwd=2.0)

lines(x_points, g2_2, type="l", 
      pch=16, 
      lty=2, 
      lwd=2.0,
      col="red") 
lines(x_points, g3_2, type="l", 
      pch=17, 
      lty=3, 
      lwd=2.0,
      col="blue") 
lines(x_points, g4_2, type="l", 
      pch=18, 
      lty=4, 
      lwd=2.0,
      col="gray") 
lines(x_points, g5_2, type="l", 
      pch=19, 
      lty=5, 
      lwd=2.0,
      col="green") 


legend("bottomright", 
       y=0.5,
       title=TeX("$sigma$"), 
       c(2,4,6,8, 10), 
       lty=1:5, col=c("black",  "red","blue", "gray", "green"),
       cex=1.5 
)



```

```{r plotsm}
############################################
### variation of location
x_points=seq(-2.5,5, 0.1)

par1_2_3=c(2, 4, -2)
par2_2_3=c(2, 4, -1)
par3_2_3=c(2, 4, 0)
par4_2_3=c(2, 4, 1)
par5_2_3=c(2, 4, 2)


g1_2=dweibull3p(x_points,par1_2_3)
g2_2=dweibull3p(x_points,par2_2_3)
g3_2=dweibull3p(x_points,par3_2_3)
g4_2=dweibull3p(x_points,par4_2_3)
g5_2=dweibull3p(x_points,par5_2_3)


plot(x_points, g1_2,
     ylim = c(0,1),
     main=TeX("$alpha=2, sigma=4$"),
     ylab = TeX("$f(x)$"),xlab ="x" ,type="l",
     pch=15, lty=1, lwd=2.0)


lines(x_points, g2_2, type="l", 
      pch=16, 
      lty=2, 
      lwd=2.0,
      col="red") 
lines(x_points, g3_2, type="l", 
      pch=17, 
      lty=3, 
      lwd=2.0,
      col="blue") 
lines(x_points, g4_2, type="l", 
      pch=18, 
      lty=4, 
      lwd=2.0,
      col="gray") 
lines(x_points, g5_2, type="l", 
      pch=19, 
      lty=5, 
      lwd=2.0,
      col="green") 



legend("topright", 
       y=0.5,
       title=TeX("$mu$"), 
       c(-2, -1, 0,1,2), 
       lty=1:5, col=c("black",  "red","blue", "gray", "green"),
)



g1_2=pweibull3p(x_points,par1_2_3)
g2_2=pweibull3p(x_points,par2_2_3)
g3_2=pweibull3p(x_points,par3_2_3)
g4_2=pweibull3p(x_points,par4_2_3)
g5_2=pweibull3p(x_points,par5_2_3)


plot(x_points, g1_2,
     ylim = c(0,1),
     main=TeX("$alpha=2, sigma=4$"),
     ylab = TeX("$F(x)$"),xlab ="x" ,type="l",
     pch=15, lty=1, lwd=2.0)

lines(x_points, g2_2, type="l", 
      pch=16, 
      lty=2, 
      lwd=2.0,
      col="red") 
lines(x_points, g3_2, type="l", 
      pch=17, 
      lty=3, 
      lwd=2.0,
      col="blue") 
lines(x_points, g4_2, type="l", 
      pch=18, 
      lty=4, 
      lwd=2.0,
      col="gray") 
lines(x_points, g5_2, type="l", 
      pch=19, 
      lty=5, 
      lwd=2.0,
      col="green") 



legend("bottomright", #inset=.05,
       y=0.5,
       title=TeX("$mu$"), 
       c(-2, -1, 0,1,2), 
       lty=1:5, col=c("black",  "red","blue", "gray", "green"),
       cex=1.5 
)

```


## Modeling 1: Foreign investment in Brazil

Textos...

```{r data1}
data_invest = c(8.958070, 25.150000, 5.015640, 11.006760, 5.279225, 5.013624, 6.435200, 5.835900, 7.000000, 30.801862, 7.266010, 5.019073, 8.000000, 22.000000, 5.514300, 5.302930, 7.968871, 6.000000, 7.058850, 15.000000, 5.155455, 5.243266, 5.219835, 9.950000, 12.041000, 7.300000, 5.743445, 6.272980, 5.070780, 6.529300, 6.000000, 8.697271, 46.067600, 17.171580, 6.098290, 10.166405, 5.445229, 7.281530, 7.757070, 6.413050, 5.331970, 6.225743, 14.399190, 5.850000, 5.653820, 5.484998, 5.257000, 5.841250, 8.265000, 27.373000, 5.412000, 5.099400, 5.145000, 5.344151, 7.064000, 7.608040, 6.346600, 5.052410, 5.623980, 14.615000, 5.358124, 5.210000, 68.570220, 5.097540, 5.400696, 5.959290, 5.148630, 7.860000, 5.104935, 22.000000, 8.985627, 5.176840, 5.177960, 5.040000, 7.419586, 15.634430, 5.146730, 5.131308, 7.153940, 6.105075, 5.100000, 5.202800, 6.371200, 5.477140, 5.309140, 5.776270, 6.301900, 5.100000, 12.041000, 5.123362, 5.133710, 5.225000, 5.030000, 6.508635, 5.012000, 5.064560, 5.500000, 12.041000, 5.115196, 13.318230, 5.147040, 7.895000, 5.050000, 5.400000, 6.441160, 5.090940, 5.369000, 8.607168, 5.455010, 22.230579, 5.376720, 7.275880, 13.920000, 5.225000, 31.122660, 12.4994, 5.342750, 5.177051, 7.556150, 5.390042, 6.433160, 7.514053, 10.695323, 8.985627, 5.247600, 6.287972, 16.052030, 5.659609, 5.100000, 6.098817, 6.101590, 5.281640)
```

```{r descr1}
###--------------------------------------------
### Descritive statistics
###--------------------------------------------

#install.packages('e1071')
library(e1071)

tab=summary(data_invest)

(tab_res=c(tab, Sd=sd(data_invest),
           CS=skewness(data_invest),
           CK=kurtosis(data_invest)))
```


## Required functions
```{r estimators}
###---------------------------------------------------------
### Required functions
###---------------------------------------------------------


##----------------------------##
## DATA MODIFIED SCORE VECTOR ##
##----------------------------##
score.dataMod<-function(z,param){
  n=length(z)
  
  mu<-min(z)   ## Location parameter
  a<-param[1] ## Shape parameter
  s<-param[2] ## Scale parameter
  u<-runif(n) ## Generating uniform data
  z<-sort(z,decreasing = F)
  x<-z[-which.min(z)]
  Salpha<- (n-1)/a+(1/a)*sum(log((x-mu)**a))-(1/s)*sum(log(x-mu)*(x-mu)**a)
  Ssigma<- -(n-1)/s+(1/s**2)*sum((x-mu)**a)
  score<- c(Salpha,Ssigma)
  return(score)
}
##score.dataMod(y,rep(0.1,2))

##----------------------------------##
## DATA MODIFIED INFORMATION MATRIX ##
##----------------------------------##
InfoMat<-function(z,param){
  n=length(z)
  
  mu<-min(z)  ## Location parameter
  a<-param[1] ## Shape parameter
  s<-param[2] ## Scale parameter
  u<-runif(n) ## Generating uniform data
  #n<-length(z)
  z<-sort(z,decreasing = F)
  x<-z[-which.min(z)]
  ##d1gama2<-0.422784 ## First order derivative of the gamma(2) function 
  ##d2gama2<-0.823681 ## Second order derivative of the gamma(2) function 
  
  ## Computing the second order cumulants
  ##kaa<-((n-1)/a**2)*((1+d2gama2)+log(s)*(2*d1gama2+log(s)))
  ##kss<-(n-1)/s**2
  ##ksa<- -(n-1)*(d1gama2+log(s))/(a*s)
  kaa<- (n-1)/a**2+(1/s)*sum((x-mu)**a*(log(x-mu))**2)
  kss<- 2*sum((x-mu)**a)/s**3-(n-1)/s**2
  ksa<- -(1/s**2)*sum(log(x-mu)*(x-mu)**a)
  InfoMatrix<-matrix(NA,nrow = length(param), ncol = length(param))
  InfoMatrix[1,1]<-kaa
  InfoMatrix[2,2]<-kss
  InfoMatrix[1,2]<-InfoMatrix[2,1]<-ksa
  ##
  return(InfoMatrix)
}
##InfoMat(y,rep(0.1,2))
##
##--------------------------##
## FISHER SCORING ALGORITHM ##
##--------------------------##
FS.dataModified<-function(z,chute,maxiter=20,eps=1e-08){
  mu<-min(z) 
  z<-sort(z,decreasing = F)
  x<-z[-which.min(z)]
  tolvec<-double() 
  count<-1
  tolvec[1]<-toler<-1
  resAnter<-cbind(chute[1],chute[2])
  
  
  while(TRUE){
    if((resAnter[1]>0)&&(resAnter[2]>0)){
      V<-score.dataMod(z,resAnter)%*%solve(InfoMat(z,resAnter))
      
      ##----------------##
      ## Iterative Step ##
      ##----------------##
      resAtual<-resAnter+V
    }
    else
      return(NA)
    
    #print(resAtual)
    
    ##-------------------------## 
    ## Computing the Tolerance ##
    ##-------------------------##
    toler<- sum(abs(resAtual-resAnter)/abs(resAnter))
    count<- count+1
    ##
    ##--------------------------##
    ## Updating some Quantities ##
    ##--------------------------##
    resAnter<-resAtual
    tolvec[count]<-toler
    ####cat("Wait: the FS algorithm is running", "\r")
    ##
    ##
    ##s.new<-sum((x-mu)**resAtual[1])/(n-1)
    ##
    if((eps>toler)|(count>maxiter))break
  }
  param.hat<-c(mu,resAnter)
  Hessian<-InfoMat(z,resAtual)
  return(list(Hessian=Hessian, tolvec=tolvec, param.hat=param.hat))
}
##
##
##
##
##
##-------------------------##
## DOBLY MODIFIED APPROACH ##
##-------------------------##
penaltyFunc<-function(z,param){
  n=length(z)
  
  mu<-min(z)  ## Location parameter
  a<-param[1] ## Shape parameter
  s<-param[2] ## Scale parameter
  u<-runif(n) ## Generating uniform data
  #n<-length(z)
  Info.da<-matrix(NA,nrow = length(param), ncol = length(param))
  Info.ds<-matrix(NA,nrow = length(param), ncol = length(param))
  
  ##--------------------------------##
  ## Calling the information matrix ##
  ##--------------------------------##
  InfoMatrix<-InfoMat(z,param)
  ##
  z<-sort(z,decreasing = F)
  x<-z[-which.min(z)]
  ##------------------------------------##
  ## Computing the third-order cumulants##
  ##------------------------------------##
  kaaa<- (1/s)*sum((x-mu)**a*(log(x-mu))**3)-(2/a**3)*(n-1)
  ksss<- (2/s**3)*(n-1)-(6/s**4)*sum((x-mu)**a)
  kaas<- kasa<-(-1/s**2)*sum((x-mu)**a*(log(x-mu))**2)
  kssa<- ksas<-(2/s**3)*sum((x-mu)**a*(log(x-mu)))
  
  ##--------------------------------------##
  ## Prime of I(phi) with respect of alpha##
  ##--------------------------------------##
  Info.da[1,1]<-kaaa
  Info.da[2,2]<-kssa
  Info.da[1,2]<-Info.da[2,1]<-kaas
  
  ##--------------------------------------##
  ## Prime of I(phi) with respect of sigma##
  ##--------------------------------------##
  Info.ds[1,1]<-kaas
  Info.ds[2,2]<-ksss
  Info.ds[1,2]<-Info.ds[2,1]<-kssa
  
  ##----------------------------##
  ## Computing the penalty term ##
  ##----------------------------##
  Aa<-sum(diag(solve(InfoMatrix)%*%Info.da))
  As<-sum(diag(solve(InfoMatrix)%*%Info.ds))
  penalty<-c(Aa,As)
  return(penalty)
}
##penaltyFunc(y,rep(0.1,2))

##--------------------------------##
## DOUBLY MODIFIED SCORE FUNCTION ##
##--------------------------------##
score.DoublyMod<-function(z,param){
  doublyModScore<-score.dataMod(z,param)+0.5*penaltyFunc(z,param)
  return(doublyModScore)
}
##score.DoublyMod(y,param)


##----------------------------------------------------##
## FISHER SCORING ALGORITHM FOR DOUBLY MODIFIED SCORE ##
##----------------------------------------------------##
FS.DoublyModified<-function(z,chute,maxiter=20,eps=1e-08){
  mu<-min(z) 
  z<-sort(z,decreasing = F)
  x<-z[-which.min(z)]
  tolvec<-double() 
  count<-1
  tolvec[1]<-toler<-1
  resAnter<-cbind(chute[1],chute[2])
  
  
  while(TRUE){
    
    if((resAnter[1]>0)&&(resAnter[2]>0)){
      V<-score.DoublyMod(z,resAnter)%*%solve(InfoMat(z,resAnter))
      
      ##----------------##
      ## Iterative Step ##
      ##----------------##
      resAtual<-resAnter+V
    }
    else
      return(NA)
    ##-------------------------## 
    ## Computing the Tolerance ##
    ##-------------------------##
    toler<- sum(abs(resAtual-resAnter)/abs(resAnter))
    count<- count+1
    ##
    ##--------------------------##
    ## Updating some Quantities ##
    ##--------------------------##
    resAnter<-resAtual
    tolvec[count]<-toler
    ####cat("Wait: the FS algorithm is running", "\r")
    ##
    if((eps>toler)|(count>maxiter))break
  }
  doubModEstimates<-c(mu,resAnter)
  Hessian<-InfoMat(z,resAtual)
  return(list(Hessian=Hessian, tolvec=tolvec,doubModEstimates=doubModEstimates))
}


##----------------------------------------------------##
## scale estimator (MMLE) ##
##----------------------------------------------------##
sigma_est=function(x, shape){
  loc=min(x)
  n=length(x)
  out=sum((x[-which.min(x)]-loc)^shape)/(n-1)
  return(out)
}

##----------------------------------------------------##
## modified log-likelihood ##
##----------------------------------------------------##
llf=function(x, shape, scale){
  loc=min(x)
  n=length(x)
  
  out=(n-1)*log(shape)-(n-1)*log(scale)-(1/scale)*sum((x[-which.min(x)]-loc)^shape)+(shape-1)*sum(log((x[-which.min(x)]-loc)))
  return(out)
}


##----------------------------------------------------##
## minus modified log-likelihood function ##
##----------------------------------------------------##
llf_alpha=function(x, shape){
  -llf(x, shape, scale=sigma_est(x, shape))
}

##----------------------------------------------------##
## parameter estimation via Modified MLE (cf. [Kundu and Zaquab, 2009]) ##
##----------------------------------------------------##
weibull_fit_optim=function(x, a0=1){
  
  mu_est=min(x)
  
    estimated_params2 <- optim(c(1),
                             llf_alpha, x = x,
                             method = "L-BFGS-B",
                             lower = c(10^(-5)), upper = c(Inf)#Inf
  )
  
  a_est=estimated_params2$par
  s_est=sigma_est(x, a_est)
  
  llf_out=-estimated_params2$value
  
  return(list(shape_est=a_est,scale_est=s_est, loc_est=mu_est, llf_max=llf_out))
  
}

##----------------------------------------------------##
## corrected log-likelihood function (cf. [Cheng and Iles]) ##
##----------------------------------------------------##
   
llf_til=function(x, param, h){
  ##param=c(alpha,sigma)
  loc=min(x)
  n=length(x)
  y=x[-which.min(x)]
  
  out2=sum(log(dweibull3p(x=y, c(param,loc))))
  funcao_para_integral <- function(x) {
    
    return(dweibull3p(x, c(param,loc)))
  }
  
  out1=log(integrate(funcao_para_integral, lower = loc, upper = loc+h)$value)  
  
  return(out1+out2)
}

menosllf_til=function(x, param, h){
  -llf_til(x, param,h)
}

weibull_fit_optim2=function(x, h, a0=c(1,1)){
  
  mu_est=min(x)
  
  estimated_params2 <- optim(a0,
                             menosllf_til, x = x, h=h,
                             method = "L-BFGS-B",
                             lower = c(0,0), upper = c(Inf,Inf)#Inf
  )
  
  a_est=estimated_params2$par[1]
  s_est=estimated_params2$par[2]
  
  llf_out=-estimated_params2$value
  
  
  
  
  
  
  return(list(shape_est=a_est,scale_est=s_est, loc_est=mu_est, llf_max=llf_out))
  
}


```

## Function for Weibull fit

```{r estimators2}
###---------------------------------------------------------
### Function for parameter estimation in 3-parameter Weibull model
###---------------------------------------------------------
# x: a vector of positive data
# a0: initial guess for (shape, scale)
# method: DM2 (Duble modified MLE), KD (Modified MLE), CI (Corrected MLE)
# h: correction parameter for CI method

weibull_fit=function(x,a0=c(1,1), method="DM2", h=0.2){
  #DM2: DMMLE (duble modified MLE)
  #KD: MMLE (from Kundu and Zaqab)
  #CI: CMLE (from Cheng and Iles)
  
  if(method=="KZ"){
    out=weibull_fit_optim(x, a0[1])
  }
  if(method=="CI"){
    out=weibull_fit_optim2(x, h, a0)
  }
  if(method=="DM2"){
    ## Doubly Modified Estimates
    Obj.Doublymod<-FS.DoublyModified(x,a0)
    mu_est=Obj.Doublymod$doubModEstimates[1]
    a_est=Obj.Doublymod$doubModEstimates[2]
    s_est=Obj.Doublymod$doubModEstimates[3]
    
    
    llf_out=sum(log(dweibull3p(x[-which.min(x)], param = c(a_est, s_est, mu_est))))
    out=list(shape_est=a_est,scale_est=s_est, loc_est=mu_est, llf_max=llf_out)
  }
  
  return(out)
}


```

```{r examples}
#DMMLE
weibull_fit(data_invest, method="DM2")

#MMLE
weibull_fit(data_invest, method="KZ")

#CMLE
weibull_fit(data_invest, method="CI")
```

```{r }

```

```{r }

```

```{r }

```

```{r }

```

```{r }

```

```{r }

```


