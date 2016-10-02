library(FinTS)
# read data
setwd('C:\\Users\\carrai1\\Desktop\\Master\\MA611_Time_Series\\Assigments\\3\\')
bos.df=scan(file='.\\Boston.dat')

#checks
class(bos.df)
head(bos.df)
#To change the Boston.dat data into a ts object
bos.ts=ts(bos.df,start=c(1966,1),frequency=12)
class(bos.ts)
head(bos.ts)
plot(bos.ts)

######## 1.a Use the moving average decomposition models we discussed in class along with R (decompose). 
#Determine the best model to fit the data (justify your conclusion both in terms of the characteristics of the data and an evaluation of the fit).



# Moving avg descompositon model. I'm going to check both multiplicative and additive but it really seems to be 
# multiplicative

bos.da=decompose(bos.ts,type='additive')
bos.dm=decompose(bos.ts,type='multiplicative')


predda=bos.da$trend+bos.da$seasonal
layout(1:1)
plot(bos.ts,lwd=2)
lines(predda,col='red',lwd=2)

preddm=bos.dm$trend+bos.dm$seasonal
layout(1:1)
plot(bos.ts,lwd=2)
lines(preddm,col='red',lwd=2)


# Let's check decomposition
 
# lets get rsme to compare models. To see which one fits better.
#ad
bosdaresid=window(bos.da$random, start=c(1966,7), end=c(1975,4))
rmseda=sqrt(sum(bosdaresid^2)/length(bosdaresid))
print("RMSE decomposition additive: ", as.character(rmseda))
#mul
bosdmresid=window(bos.dm$random, start=c(1966,7), end=c(1975,4))
rmsedm=sqrt(sum(bosdmresid^2)/length(bosdmresid))
rmsedm

#########1.b Fit a trend line to the smoothed deseasonalized series to facilitate forecasting using your choice of model in a.
#If we want to forecast beyond the last date at which we have data we need to create a forecast of the trend component. 
#One approach is to forecast the trend component using a linear regression model
#We want to regress the trend of the log transformed data against time so 
#we need to extract the time values of the time series as a separate variable.
#tr=bos.dm$trend
tr=window(bos.dm$trend, start=c(1966,7), end=c(1975,4))
tim=time(tr)
ti = unclass(tim)
# now we perform linear regression
trreg=lm(tr~ti)
plot(tr,lwd=2)
lines(ti,fitted(trreg),col='red',lwd=2)


########1.c. Generate forecasts for Nov. 1975 and Dec. 1975 using the best model determined in a. 

# now we can predict 
# se.fit=TRUE tells the predict function to compute standard errors for the predictions.  
predtime=c(1975.833,1975.917)
predtime
preddata=predict(trreg,data.frame(ti=predtime),se.fit=TRUE)
preddata$fit





# Let's do stl model 
#another decomposition model is 'stl' it uses another 'moving average' estimate, i.e. using loess regression 
bos.stla=stl(bos.ts,s.window='periodic')
str(bos.stla)
#The object jnjd is a list of 8 items. The one of interest to us is the first item
#This item is a 'matrix' of three columns, 'seasonal','trend','remainder' (or error) component. 
#The object jnjd$time.series gives us the three components. 
class(bos.stla$time.series[,'seasonal'])
plot(bos.stla)
pred1=bos.stla$time.series[,'seasonal'] + bos.stla$time.series[,'trend']
plot(bos.ts,lwd=2)
lines(pred1,col='red',lwd=2)
# to do multiplicative model
logbos.ts=log(bos.ts)
plot(logbos.ts,ylab='log(Boston_crime)')
bos.stlm=stl(logbos.ts,s.window='periodic')
plot(bos.stlm)
pred2=bos.stlm$time.series[,'seasonal'] + bos.stlm$time.series[,'trend']
layout(1:1)
plot(logbos.ts,lwd=2)
lines(pred2,col='red',lwd=2)


#rmsed
plot(bos.stla$time.series[,'remainder'],ylab='Remainder')#plot reminder
rmsed=sqrt(sum(bos.stla$time.series[,'remainder']^2)/length(bos.stla$time.series[,'remainder']))
rmsed
#rmsed
plot(bos.stlm$time.series[,'remainder'],ylab='Remainder')#plot reminder
rmsed=sqrt(sum(bos.stlm$time.series[,'remainder']^2)/length(bos.stlm$time.series[,'remainder']))
rmsed








#########2.b Fit a trend line to the smoothed deseasonalized series to facilitate forecasting using your choice of model in a.
#If we want to forecast beyond the last date at which we have data we need to create a forecast of the trend component. 
#One approach is to forecast the trend component using a linear regression model
#We want to regress the trend of the log transformed data against time so 
#we need to extract the time values of the time series as a separate variable.
tr=bos.stlm$time.series[,'trend']
tim=time(tr)
ti = unclass(tim)
# now we perform linear regression
trreg=lm(tr~ti)
plot(tr,lwd=2)
lines(ti,fitted(trreg),col='red',lwd=2)


########2.c. Generate forecasts for Nov. 1975 and Dec. 1975 using the best model determined in a. 

# now we can predict 
# se.fit=TRUE tells the predict function to compute standard errors for the predictions.  
predtime=c(1975.833,1975.917)
predtime
preddata=predict(trreg,data.frame(ti=predtime),se.fit=TRUE)
preddata
exp(preddata$fit)


############ 3a. As a special case of a decomposition model, use a regression model to represent the series as a linear function of time.
# same thing but we use seasonal as dummies variable to do regression
btime=unclass(time(bos.ts))
bseas=cycle(bos.ts)
bdata=coredata(bos.ts)
bosreg=lm(bdata~0+btime+factor(bseas))
#summary(bosreg)
#plot(bosreg)
# we understand that the residuals will likely be correlated which technically violates the assumptions
#required for regression (more on that later).
boshat=fitted(bosreg)
plot(bos.ts,ylim=c(-5,550))
lines(btime,boshat,col='red')


############ 3b. Use appropriate output and residual plots to comment of the fit of the model.
ebos=resid(bosreg)
plot(bosreg)
#hist errors
hist(ebos)
acf(ebos)
AutocorTest(ebos,lag=1)
AutocorTest(ebos,lag=log(length(ebos)))


############ 3c. Try and fit a quadratic model of time. Comment on whether the fit is improved. Are there still weaknesses?
btime=unclass(time(bos.ts))
bosreg2=lm(bdata~0+btime+I(btime^2)+factor(bseas))
#summary(bosreg2)
#plot(bosreg2)
#plot result
ebos2=resid(bosreg2)
boshat2=fitted(bosreg2)
plot(bos.ts,ylim=c(-5,550))
lines(btime,boshat2,col='red')
#hist errors
hist(ebos2)
# autocorrelation. There is some seasonality we didn't pick
acf(ebos2)
AutocorTest(ebos2,lag=1)
AutocorTest(ebos2,lag=log(length(ebos2)))





############ 3d. Use the best model from a. and c. (be sure to justify which you believe is best!)
#to predict armed robberies for Nov. 1975 and Dec. 1976.

predtime=c(1975.833,1975.917)
predseas=c(11,12)
predseas
preddata=predict(bosreg2,data.frame(btime=predtime,bseas=predseas),se.fit=TRUE)
preddata$fit




########### 4a. Determine the best Holt-Winters model to fit the series. Comment on any alternatives
#you considered and the rationale you used to determine the best model

# holt-winters multiplicative
hwmbos=HoltWinters(bos.ts,seasonal='multiplicative')
hwmbos$fitted
#plot
plot(bos.ts,lwd=2)
lines(hwmbos$fitted[,1],col='red',lwd=2)
#residuals

#visualize residuals
plot(reshwmbos,ylab='Residual')


# let's do now the additive model
hwabos=HoltWinters(bos.ts,seasonal='additive')
hwabos$fitted
#plot
plot(bos.ts,lwd=2)
lines(hwabos$fitted[,1],col='red',lwd=2)


#tsjnj=as.ts(bos.ts)
reshwmbos=bos.ts-hwmbos$fitted[,'xhat']
rmse1=sqrt(sum(reshwmbos^2)/length(reshwmbos))
rmse1
#residuals
reshwabos=bos.ts-hwabos$fitted[,'xhat']
rmse2=sqrt(sum(reshwabos^2)/length(reshwabos))
rmse2






########### 4b. Use the answer a. to predict armed robberies for Nov. 1975 and Dec. 1975
#compute and visualize predictions


# Additive model does much better!!!!
#plot
p=predict(hwabos,n.ahead=2,prediction.interval=TRUE)
p
plot(hwabos,predicted.values=p)


########## 5.You now have several forecasts (decomposition (regression based, moving average/loess),
# Holt-Winters, and visual) for the number of armed robberies for Nov 1975 and Dec. 1975. 
# Comment on which estimates you believe to be the best (support your response with the appropriate diagnostics).



