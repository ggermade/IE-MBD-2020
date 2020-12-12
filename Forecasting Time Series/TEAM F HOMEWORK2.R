# load libraries 
library(fBasics)
library(forecast) 

# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)
getwd()

data <- read.csv("coca_cola_earnings.csv", sep = ';', dec = ",")

### Model 1 and 1.b ###

y<-data[,2] 

ts.plot(y) 

nlags=60    # we use 60 lags for Model 1 and 40 lags for Model 2

par(mfrow=c(2,1))
acf(y,nlags)
pacf(y,nlags)  

s=4       # seasonal parameter 

nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences? 
# Formal test is telling us to take 1 regular difference and 1 seasonal diff.
# Because of sinusoidal pattern, we will not take the regular difference. 

#  plot(z) to check if our transformation woked to make data stationary
#z <- diff(y)
#ts.plot(z)

# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit<-arima(y,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

ts.plot(fit$residuals)

# Formal test says we need 1 regular and 1 seasonal difference. 
ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

ts.plot(fit$residuals)

# Now that our data is stationary, we check the 
#ACF and PACF of residuals

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  

#looking at PACF, let's try P order of 1 and/or p order of 1
# Model 1: (0,1,0)(1,1,0)
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s)) 
fit

# is the model significant?
-0.4842+1.96*0.0899
-0.4842-1.96*0.0899
# yes, significant: CI (-0.6604, -0.3079)

ts.plot(fit$residuals)

# Plot ACF and PACF of residuals 
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?
# 0.01 for the first series
# 1.5 for the second series...what do we want for coca-cola series?

Box.test(fit$residuals,lag=60)
# data is white noise

shapiro.test(fit$residuals)  
# data is NOT normally distributed 

# normality test graphically
hist(fit$residuals,prob=T,ylim=c(0,0.002),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")



# Model 1.b: (1,1,0)(0,1,0)
fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

# is the model significant?
-0.3206+1.96*0.0951
-0.3206-1.96*0.0951
# yes, significant: CI (-0.5069, -0.1342)

ts.plot(fit$residuals)

# Plot ACF and PACF of residuals 
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  
### NOT a GOOD MODEL - no white noise ####

hist(fit$residuals,prob=T,ylim=c(0,30),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

y.pred<-predict(fit,n.ahead=24)
y.pred$pred   # point predictions
y.pred$se    # standard errors

# plotting real data with point predictions

new <- c(y,y.pred$pred) # real data + predicted values

plot.ts(new,main="Predictions",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

### Out of Sample Forecasting for Model 1

n<-length(y)
n

n.estimation<-24 # need to leave 24 real values to compare models 
n.forecasting<-n-n.estimation # 83 observations
horizons<-2 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizons)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizons,ncol=1)
MAPE<-matrix(0,nrow=horizons,ncol=1)

for (Periods_ahead in 1:horizons) {
  for (i in 1:n.forecasting) {
    aux.y<-y[i:(n.estimation-Periods_ahead+i)]; # change 1 to i for rolling 
    fit<-arima(aux.y,order=c(0,1,0),seasonal=list(order=c(1,1,0),period=s), method = "ML"); ### change for each model 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}

MSFE
MAPE



#----------------------------


### Model 2 ###

y<-data[,2] 

ts.plot(y) 

nlags=40    # play with this parameter..

par(mfrow=c(2,1))
acf(y,nlags)
pacf(y,nlags)  

s=4       # seasonal parameter 

nsdiffs(y,m=s,test=c("ocsb"))  # seasonal differences?
ndiffs(y, alpha=0.05, test=c("adf")) # regular differences? 
# Formal test is telling us to take 1 regular difference and 1 seasonal diff.

# (?) plot(z) to check if our transformation woked to make data stationary
#z <- diff(y)
#ts.plot(z)

# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit<-arima(y,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

ts.plot(fit$residuals)

fit<-arima(y,order=c(0,2,0),seasonal=list(order=c(0,0,0),period=s)) 
fit

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

# estimate the SAR and analyze the estimated parameters. Compare with the Seasonal Difference
fit<-arima(y,order=c(0,0,0),seasonal=list(order=c(0,2,0),period=s)) 
fit

ts.plot(fit$residuals)

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

# Now that our data is stationary, we check the 
#ACF and PACF of residuals

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  

# Model 2.a: (0,0,0)(1,2,0)
fit<-arima(y,order=c(0,0,0),seasonal=list(order=c(1,2,0),period=s)) 
fit

# is SAR 1 coefficient significant?
-0.4566+1.96*0.1015
-0.4566-1.96*0.1015
# yes!  CI (-0.65554, -0.25766)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  
# no white noise

# Model 2.b: (0,0,0)(1,2,1)
fit<-arima(y,order=c(0,0,0),seasonal=list(order=c(1,2,1),period=s)) 
fit

# is SMA 1 coefficient significant?
-0.9460+1.96*0.1031
-0.9460-1.96*0.1031
# yes!  CI (-1.148, -0.744)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  
# no white noise 


# Model 2.c
fit<-arima(y,order=c(0,0,0),seasonal=list(order=c(0,2,1),period=s)) 
fit
# is MA 1 coefficient significant? (0,0,0)(0,2,1)
-0.9451+1.96*0.0891
-0.9451-1.96*0.0891
# yes! significant! CI (-1.1197, -0.7704)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)
# no white noise

# Model 2.d: (5,0,0)(0,2,1)
fit<-arima(y,order=c(5,0,0),seasonal=list(order=c(0,2,1),period=s)) 
fit

# is the model significant?
#ar5
0.3455+1.96*0.1045
0.3455-1.96*0.1045
# Yes! Significant ! CI (0.1407, 0.5503)

par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)

ts.plot(fit$residuals)

# Plot ACF and PACF of residuals 
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  
#2.b: not good
#2.c: not good
#2.d: good!

ndiffs(fit$residuals, alpha=0.05, test=c("adf")) # regular differences?
nsdiffs(fit$residuals, m=s,test=c("ocsb")) # seasonal differences?

Box.test(fit$residuals,lag=40)
# data is white noise

shapiro.test(fit$residuals)  
# data is NOT normally distributed .... (?)

# normality test graphically
hist(fit$residuals,prob=T,ylim=c(10,10),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

# 0.01 for the first series
# 1.5 for the second series...what do we want for coca-cola series?

# Model 2: (1,1,0)(0,1,0)
fit<-arima(y,order=c(1,1,0),seasonal=list(order=c(0,1,0),period=s)) 
fit

# is the model significant?
-0.3206+1.96*0.0951
-0.3206-1.96*0.0951
# yes, significant: CI (-0.5069, -0.1342)

ts.plot(fit$residuals)

# Plot ACF and PACF of residuals 
par(mfrow=c(2,1))
acf(fit$residuals,nlags)
pacf(fit$residuals,nlags)  
### NOT GOOD ####



#---------------------------
hist(fit$residuals,prob=T,ylim=c(0,1.5),xlim=c(mean(fit$residuals)-3*sd(fit$residuals),mean(fit$residuals)+3*sd(fit$residuals)),col="red")
lines(density(fit$residuals),lwd=2)
mu<-mean(fit$residuals)
sigma<-sd(fit$residuals)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

y.pred<-predict(fit,n.ahead=24)
y.pred$pred   # point predictions
y.pred$se    # standard errors

# plotting real data with point predictions

new <- c(y,y.pred$pred) # real data + predicted values

plot.ts(new,main="Predictions",
        ylab="Dollars",col=3,lwd=2) # time series plot
lines(y,col=4,lwd=2) # for the second series
legend("topleft",legend=c("Predictions","Historical"),col=c(3,4),
       bty="n",lwd=2)

### Out of Sample Forecasting for Model 1

n<-length(y)
n

n.estimation<-24 # need to leave 24 real values to compare models 
n.forecasting<-n-n.estimation # 83 observations
horizons<-2 # number of periods ahead

predicc<-matrix(0,nrow=n.forecasting,ncol=horizons)
real<-matrix(0,nrow=n.forecasting,ncol=1)
real<-y[(n.estimation+1):length(y)] 
MSFE<-matrix(0,nrow=horizons,ncol=1)
MAPE<-matrix(0,nrow=horizons,ncol=1)

for (Periods_ahead in 1:horizons) {
  for (i in 1:n.forecasting) {
    aux.y<-y[1:(n.estimation-Periods_ahead+i)]; # change 1 to i for rolling 
    fit<-arima(aux.y,order=c(5,0,0),seasonal=list(order=c(0,2,1),period=s), method = "ML"); ### change for each model 
    y.pred<-predict(fit,n.ahead=Periods_ahead);
    predicc[i,Periods_ahead]<- y.pred$pred[Periods_ahead];
  }
  error<-real-predicc[,Periods_ahead];
  MSFE[Periods_ahead]<-mean(error^2);
  MAPE[Periods_ahead]<-mean(abs(error/real)) *100;
}

MSFE
MAPE


