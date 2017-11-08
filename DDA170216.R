#################################################################RETAIL GIANT FORECAST ####################################################################################
#Installing tthe required libraries

#install.packages(forecast)
#install.packages(lubridate)
#install.packages(dplyr)
#install.packages(ggplot2)
#install.packages(tseries)
#install.packages(cowplot)

library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tseries)
library(cowplot)

#######################################################################DATA GATHERING###########################################################################
#Reading the Global superstore dataset:

superstore<-read.csv('Global Superstore.csv',header = TRUE)
View(superstore)

#######################################################################DATA CLEANING############################################################################

sum(is.na(superstore))
sapply(superstore,function(x) sum(is.na(x)))

#Apart from the Postal Code there doesn't seem to be any NA Values in the dataset and hence no imputation required.

segment<-paste(superstore$Segment,superstore$Market,sep = '_')
superstore_seg<-cbind(superstore,segment)

#######################################################################EXPLORATORY DATA ANALYSIS###############################################################

#Finding the min & Max Order Date in the dataset
superstore_seg$Order.Date<-as.Date(superstore_seg$Order.Date,format = "%d-%m-%Y")

min(superstore_seg$Order.Date)
#2011-01-01

max(superstore_seg$Order.Date)
#2014-12-31

#extracting the monthly date column out of the Superstore Segment
superstore_seg$month<-format(as.Date(superstore_seg$Order.Date), "%Y-%m")

#Finding the most profitable segment in the given dataset
Most_profitable<-arrange(with(superstore_seg, aggregate(Profit, by=list(segment), FUN=sum)),x)
Most_profitable

#Plot the graph to find the most profitable segment:
bar_theme<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                  legend.position="none")
ggplot(data = Most_profitable,aes(x=Group.1,x))+geom_bar(stat = "identity") +bar_theme

#Below are the 2 most profitable segments

#Consumer_EU   188687.707
#Consumer_APAC 222817.560

#Find out the segemtns which are the most consistently profitable

#Build the dataframe with only order date,Profit,sales,quantity and segmentnames #Consumer_EU #Consumer_APAC 

superstore_prof<-cbind()

superstore_seg_val<-data.frame(superstore_seg$segment,superstore_seg$Sales,superstore_seg$Quantity,superstore_seg$Profit,superstore_seg$month,superstore_seg$Order.Date)
str(superstore_seg_val$superstore_seg.month)
View(superstore_seg_val)

colnames(superstore_seg_val)<-c("segment","Sales","Quantity","Profit","Month","ODate")

s <- setNames(split(superstore_seg_val, superstore_seg_val$segment), paste0(unique(sort(superstore_seg_val$segment)) ,"_df"))
list2env(s, globalenv())


profit_month<-list()
COV_month<-list()
for(i in 1:length(s))
{
  prof_month<-with(s[[i]],aggregate(Profit,by=list(Month),FUN = sum))
  profit_month[[i]]<-prof_month
  cv_month<-sd(prof_month$x)/mean(prof_month$x)*100
  COV_month[[i]]<-cv_month
}

d1<-data.frame(COV_month)

#Transform D1
d1<-t(d1)
rownames(d1)<-c(1:nrow(d1))

segment_name<-data.frame(sort(unique((superstore_seg$segment))))
cv_segment<-arrange(data.frame(segment_name,d1),d1)
colnames(cv_segment)<-c("Segment","CV Value")

ggplot(data = cv_segment,aes(x=Segment,`CV Value`))+geom_bar(stat = "identity") +bar_theme

#From the above segment it is clear that Consumer EU and Consumer APAC are the most consistent and profitable segments 
#Consumer_EU    62.43052
#Consumer_APAC  63.21323

#Building the Monthly datasets consisting of the aggregate of the Quantity,Sales and Profit

###################################################BUILDING THE AGGREGATE DATA FOR THE APAC CONSUMER DATASET######################################################

Consumer_APAC_df_agg<-subset(Consumer_APAC_df[-1],Consumer_APAC_df$ODate<'2014-12-31')

Consumer_APAC_df_agg<-Consumer_APAC_df_agg[-5]%>%
                            group_by(Month) %>% 
                            summarise_all(funs(sum))

Consumer_APAC_df_agg$Month<-gsub("-","",Consumer_APAC_df_agg$Month)
Consumer_APAC_df_agg$Month<-as.numeric(Consumer_APAC_df_agg$Month)

#######################################################BUILDING THE AGGREGATE DATA FOR THE EU CONSUMER DATASET##################################################

Consumer_EU_df_agg<-subset(Consumer_EU_df[-1],Consumer_EU_df$ODate<'2014-12-31')

Consumer_EU_df_agg<-Consumer_EU_df_agg[-5]%>%
                    group_by(Month) %>% 
                    summarise_all(funs(sum))

Consumer_EU_df_agg$Month<-gsub("-","",Consumer_EU_df_agg$Month)
Consumer_EU_df_agg$Month<-as.numeric(Consumer_EU_df_agg$Month)


#Building the time series for each of these above datasets:

########################################################################Consumer APAC##########################################################################
##Sales ##

ts_cons_APAC_sales_agg   <- ts(Consumer_APAC_df_agg$Sales,frequency = 12)
plot(ts_cons_APAC_sales_agg)

ts_cons_APAC_sales   <- ts(Consumer_APAC_df_agg[1:42,]$Sales,frequency = 12)
plot(ts_cons_APAC_sales)

##Quantity

ts_cons_APAC_quan_agg   <- ts(Consumer_APAC_df_agg$Quantity,frequency = 12)
plot(ts_cons_APAC_quan_agg)

ts_cons_APAC_quan   <- ts(Consumer_APAC_df_agg[1:42,]$Quantity,frequency = 12)
plot(ts_cons_APAC_quan)

#Smoothing the curve for the above set of timeseries :

#ts_cons_APAC_sales

decompose(ts_cons_APAC_sales)
decompose(ts_cons_APAC_sales_agg)
decompose(ts_cons_APAC_quan)
decompose(ts_cons_APAC_quan_agg)

#Smoothing the series - Moving Average Smoothing

w <-2
smoothedseries <- stats::filter(ts_cons_APAC_sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)
smoothedseries

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_cons_APAC_sales)

diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(ts_cons_APAC_sales)
lines(smoothedseries, col="blue", lwd=2)
timevals_in<-Consumer_APAC_df_agg[1:42,]$Month
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')
View(smootheddf)

#Seasonality will be modeled using a sinusoid function
#sin(0.5*Month)*Month + cos(0.5*Month) + Month
lmfit <- lm(Sales ~ poly(Month,2)+cos(2*pi*Month)+sin(2*pi*Month)+sin(0.6*Month) +cos(0.6*Month), data=smootheddf)
summary(lmfit)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_cons_APAC_sales-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Consumer_APAC_df_agg[43:48,]

timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out
fcast
#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)
MAPE_class_dec
#The MAPE Value is around 28.56 for the model
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts_cons_APAC_sales_agg, col = "black")
lines(class_dec_pred, col = "red")
#The graph shows that the predicted values follow the trend of the original data plot series

#Predict the next 6 values for Month 49-54
#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(armafit, h=12)
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[1:6],outdata$Sales)
MAPE_auto_arima
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[7:12],outdata$Sales)
MAPE_auto_arima
data.frame(fcast_auto_arima$pred,outdata$Sales)

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts_cons_APAC_sales)
autoarima
s1<-tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
fitted(autoarima)

#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_cons_APAC_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
fcast_auto_arima <- forecast(autoarima, h=12)
fcast_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts_cons_APAC_quan_agg, col = "black")
lines(auto_arima_pred, col = "red")

#The prediction for the Auto Arima model gives constant value as 44898.7

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[1:6],outdata$Sales)
MAPE_auto_arima
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[7:12],outdata$Sales)
MAPE_auto_arima
#The MAPE for the Uto Arima model is around 28.26
data.frame(fcast_auto_arima$pred,outdata$Sales)

#Using HOLTWinters Exponential smoothing technique to smoothen the model
#The Quantity time series for the APAC Consumer segment has only Trend and no seasonality,hence setting only the GAMMA Function to False

smoothedseries_APAC_sales <- HoltWinters(ts_cons_APAC_sales)
smoothedseries_APAC_sales

#Plot the smoothed time series

plot(ts_cons_APAC_sales)
plot(smoothedseries_APAC_sales, col="blue")
timevals_in<-Consumer_APAC_df_agg[43:48,]$Month

fcast_APAC_sales<-forecast::forecast(smoothedseries_APAC_sales,h=12)
fcast_APAC_sales

#Now, let's compare our prediction with the actual values, using MAPE

outdata <- Consumer_APAC_df_agg[43:48,]

MAPE_class_dec <- accuracy(fcast_APAC_sales$mean[1:6],outdata$Sales)
MAPE_class_dec
MAPE_class_dec <- accuracy(fcast_APAC_sales$mean[7:12],outdata$Sales)
MAPE_class_dec
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(ts_cons_APAC_sales_agg)
plot(smoothedseries_APAC_sales, col = "red")
plot(fcast_APAC_sales,col = "Blue")
str(fcast_APAC_sales)

#ts_cons_APAC_quan

#Smoothing the series - Moving Average Smoothing

w <-2
smoothedseries_APAC_quan <- stats::filter(ts_cons_APAC_quan, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)
smoothedseries_APAC_quan

#Smoothing left end of the time series

diff <- smoothedseries_APAC_quan[w+2] - smoothedseries_APAC_quan[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_APAC_quan[i] <- smoothedseries_APAC_quan[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_cons_APAC_quan)

diff <- smoothedseries_APAC_quan[n-w] - smoothedseries_APAC_quan[n-w-1]
for (i in seq(n-w, n)) {
  smoothedseries_APAC_quan[i] <- smoothedseries_APAC_quan[i-1] + diff
}

#Plot the smoothed time series
plot(ts_cons_APAC_quan)
lines(smoothedseries_APAC_quan, col="blue", lwd=2)
timevals_in<-Consumer_APAC_df_agg[1:42,]$Month
smootheddf_APAC_quan <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_APAC_quan)))
colnames(smootheddf_APAC_quan) <- c('Month', 'Quantity')
View(smootheddf_APAC_quan)

#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ cos(0.6*Month) , data=smootheddf_APAC_quan)

global_pred <- predict(lmfit, Month=timevals_in)
global_pred
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_cons_APAC_quan-global_pred
local_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata         <- Consumer_APAC_df_agg[43:48,]
timevals_out    <- outdata$Month
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast           <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec  <- accuracy(fcast,outdata$Quantity)
MAPE_class_dec
#The MAPE using the Classical decomposition is around 41.49

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred  <- c(ts(global_pred),ts(global_pred_out))
plot(ts_cons_APAC_quan_agg, col = "black")
lines(ts(global_pred), col = "red")
global_pred_out

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(armafit,h=12)
fcast_auto_arima
MAPE_auto_arima  <- accuracy(fcast_auto_arima$mean[7:12],outdata$Quantity)
MAPE_auto_arima

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts_cons_APAC_quan)
autoarima
s1<-tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
fitted(autoarima)

#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_cons_APAC_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

fcast_auto_arima <- forecast(autoarima, h=12)
fcast_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts_cons_APAC_quan_agg, col = "black")
lines(auto_arima_pred, col = "red")

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[1:6],outdata$Quantity)
MAPE_auto_arima
#The MAPE for the Uto Arima model is around 23.24

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[7:12],outdata$Quantity)
MAPE_auto_arima

#Using HOLTWinters Exponential smoothing technique to smoothen the model
#The Quantity time series for the APAC Consumer segment has only Trend and no seasonality,hence setting only the GAMMA Function to False

smoothedseries_APAC_quan <- HoltWinters(ts_cons_APAC_quan,gamma = F)
smoothedseries_APAC_quan$alpha

#Plot the smoothed time series

plot(ts_cons_APAC_quan)
plot(smoothedseries_APAC_quan, col="blue")
timevals_in<-Consumer_APAC_df_agg[43:48,]$Month

fcast_APAC_quan<-forecast::forecast(smoothedseries_APAC_quan,h=12)

#Now, let's compare our prediction with the actual values, using MAPE

outdata <- Consumer_APAC_df_agg[43:48,]

MAPE_class_dec <- accuracy(fcast_APAC_quan$mean[1:6],outdata$Quantity)
MAPE_class_dec

MAPE_class_dec <- accuracy(fcast_APAC_quan$mean[7:12],outdata$Quantity)
MAPE_class_dec

#The MAPE using the HOLT Winter series is around 23.45
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(ts_cons_APAC_quan_agg)
plot(smoothedseries_APAC_quan, col = "red")
plot(fcast_APAC_quan,col = "Blue")

###########################################################Time Series Analysis for Consumer EU Dataset############################################

#Consumer EU
#Sales 

ts_cons_EU_sales_agg   <- ts(Consumer_EU_df_agg$Sales,frequency = 12)
plot(ts_cons_EU_sales_agg)

ts_cons_EU_sales   <- ts(Consumer_EU_df_agg[1:42,]$Sales,frequency = 12)
plot(ts_cons_EU_sales)

#Consumer APAC
#Quantity

ts_cons_EU_quan_agg   <- ts(Consumer_EU_df_agg$Quantity,frequency = 12)
plot(ts_cons_EU_quan_agg)

ts_cons_EU_quan   <- ts(Consumer_EU_df_agg[1:42,]$Quantity,frequency = 12)
plot(ts_cons_EU_quan)


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(ts_cons_EU_sales, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)
smoothedseries

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_cons_EU_sales)

diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(ts_cons_EU_sales)
lines(smoothedseries, col="blue", lwd=2)
timevals_in<-Consumer_EU_df_agg[1:42,]$Month
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')
View(smootheddf)

#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ poly(Month,1) *sin(2*pi*Month), data=smootheddf)
lmfit
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_cons_EU_sales-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
#The ACF Plot has the spike at the first occurence,hence it is a lag1

armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Consumer_EU_df_agg[43:48,]

timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)
MAPE_class_dec
#The MAPE Value is around 24.08 for the model
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts_cons_EU_sales_agg, col = "black")
lines(class_dec_pred, col = "red")
#The graph shows that the predicted values follow the trend of the original data plot series

#Predict the next 6 values for Month 49-54
#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(armafit, h=6)
fcast_auto_arima
data.frame(fcast_auto_arima$pred,outdata$Sales)

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts_cons_EU_sales)
autoarima
s1<-tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
fitted(autoarima)

#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_cons_EU_sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(autoarima, h=12)
fcast_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts_cons_EU_quan_agg, col = "black")
lines(auto_arima_pred, col = "red")

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[1:6],outdata$Sales)
MAPE_auto_arima
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[7:12],outdata$Sales)
MAPE_auto_arima
#The MAPE for the Uto Arima model is around 24.94

#Using HOLTWinters Exponential smoothing technique to smoothen the model

smoothedseries_EU_sales <- HoltWinters(ts_cons_EU_sales)
smoothedseries_EU_sales

#Plot the smoothed time series

plot(ts_cons_EU_sales)
plot(smoothedseries_EU_sales, col="blue")
timevals_in<-Consumer_EU_df_agg[43:48,]$Month

fcast_EU_sales<-forecast::forecast(smoothedseries_EU_sales,h=12)
fcast_EU_sales
#Now, let's compare our prediction with the actual values, using MAPE

outdata <- Consumer_EU_df_agg[43:48,]


MAPE_class_dec <- accuracy(fcast_EU_sales$mean[1:6],outdata$Sales)
MAPE_class_dec

MAPE_class_dec <- accuracy(fcast_EU_sales$mean[7:12],outdata$Sales)
MAPE_class_dec

#The MAPE for the forecasted valueis around 26.39
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(ts_cons_EU_sales_agg)
plot(smoothedseries_EU_sales, col = "red")
plot(fcast_EU_sales,col = "Blue")
str(fcast_EU_sales)

#ts_cons_EU_quan

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries_EU_quan <- stats::filter(ts_cons_EU_quan, 
                                          filter=rep(1/(2*w+1),(2*w+1)), 
                                          method='convolution', sides=2)
smoothedseries_EU_quan

#Smoothing left end of the time series

diff <- smoothedseries_EU_quan[w+2] - smoothedseries_EU_quan[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EU_quan[i] <- smoothedseries_EU_quan[i+1] - diff
}

#Smoothing right end of the time series

n <- length(ts_cons_EU_quan)

diff <- smoothedseries_EU_quan[n-w] - smoothedseries_EU_quan[n-w-1]
for (i in seq(n-w, n)) {
  smoothedseries_EU_quan[i] <- smoothedseries_EU_quan[i-1] + diff
}


#Plot the smoothed time series
plot(ts_cons_EU_quan)
lines(smoothedseries_EU_quan, col="blue", lwd=2)
timevals_in<-Consumer_EU_df_agg[1:42,]$Month
smootheddf_EU_quan <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_EU_quan)))
colnames(smootheddf_EU_quan) <- c('Month', 'Quantity')
View(smootheddf_EU_quan)

#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3) +  Month, data=smootheddf_EU_quan) 

global_pred <- predict(lmfit, Month=timevals_in)
global_pred
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- ts_cons_EU_quan-global_pred
local_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- Consumer_EU_df_agg[43:48,]

timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts_cons_EU_quan_agg, col = "black")
lines(class_dec_pred, col = "red")

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(armafit, h=12)
fcast_auto_arima
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean,outdata$Quantity)
MAPE_auto_arima
data.frame(fcast_auto_arima$pred,outdata$Quantity)

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts_cons_EU_quan)
autoarima
s1<-tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")
fitted(autoarima)

#Again, let's check if the residual series is white noise

resi_auto_arima <- ts_cons_EU_quan - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(autoarima, h=12)
fcast_auto_arima


#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts_cons_EU_quan_agg, col = "black")
lines(auto_arima_pred, col = "red")

#The prediction for the Auto Arima model gives constant value as 44898.7

MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[1:6],outdata$Quantity)
MAPE_auto_arima
#The MAPE for the Uto Arima model is around 17.42
MAPE_auto_arima <- accuracy(fcast_auto_arima$mean[7:12],outdata$Quantity)
MAPE_auto_arima

data.frame(fcast_auto_arima$pred,outdata$Sales)

#Using HOLTWinters Exponential smoothing technique to smoothen the model

smoothedseries_EU_quan <- HoltWinters(ts_cons_EU_quan)
smoothedseries_EU_quan$alpha

#Plot the smoothed time series

plot(ts_cons_EU_quan)
plot(smoothedseries_EU_quan, col="blue")
timevals_in<-Consumer_EU_df_agg[1:42,]$Month

fcast_EU_quan<-forecast::forecast(smoothedseries_EU_quan,h=12)
fcast_EU_quan
#Now, let's compare our prediction with the actual values, using MAPE

outdata <- Consumer_EU_df_agg[43:48,]

MAPE_class_dec <- accuracy(fcast_EU_quan$mean[1:6],outdata$Quantity)
MAPE_class_dec

#The MAPE for the Uto Arima model is around 18.78

MAPE_class_dec <- accuracy(fcast_EU_quan$mean[7:12],outdata$Quantity)
MAPE_class_dec
#The MAPE for the Uto Arima model is around 26.51

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

plot(ts_cons_EU_quan_agg)
plot(smoothedseries_EU_quan, col = "red")
plot(fcast_EU_quan,col = "Blue")


#Below are the final results of the Values of MAPE for each of the techniques used:

#                       Classical Decomposition	Auto Arima	Holt Winter
#Consumer_APAC_Sales	   28.56	                11.87	      15.28
#Consumer_APAC_Quan	     41.49	                23.34	      23.39
#Consumer_EU_Sales	     27.72	                24.94	      12.23
#Consumer_APAC_Quan	     31.46	                17.42	      18.78
