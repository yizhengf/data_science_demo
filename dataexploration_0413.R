####################
#install related packages
library("ggplot2")
library('dplyr')
library('plotly')
#import data, to avoid blank cells in dataset, use argument na.string to set as NA
flight<-read.csv("flights.csv", header = T, na.strings = c("","NA"))
airport<-read.csv("airports.csv", header = T, na.strings = c("","NA"))
airline<-read.csv("airlines.csv", header = T, na.strings = c("","NA"))
#overview data type and dataset structure
names(flight)
head(flight)
str(flight)
summary(flight)

#overview missing value table
miss_airport<-sapply(airport, function(x) sum(is.na(x))) / nrow(airport) * 100
na_percentages<-sapply(flight, function(x) sum(is.na(x))) / nrow(flight) * 100
airline_na<-sapply(airline, function(x) sum(is.na(x))) / nrow(airline) * 100

sort(round(na_percentages,3))
sort(round(miss_airport,3))
sort(round(airline_na,3))
#remove unrelated variable manually and avoid large NAs variables in the dataset
flights <- subset(flight,
                  select = -c(CANCELLATION_REASON,
                              AIR_SYSTEM_DELAY,
                              SECURITY_DELAY,
                              AIRLINE_DELAY,
                              LATE_AIRCRAFT_DELAY,
                              WEATHER_DELAY,FLIGHT_NUMBER,TAIL_NUMBER
                  ))
#before remove these variables from datasets
#we want to see whether there is any correlation between departure delay and given reasons
#conclusion: although some of the correlation seems useful to explain the relationship, the large amount of missing values led unreliable results, therefore  we remove these variables from our analysis.
cor.test(flight$DEPARTURE_DELAY,flight$WEATHER_DELAY)
cor.test(flight$DEPARTURE_DELAY,flight$AIR_SYSTEM_DELAY)
cor.test(flight$DEPARTURE_DELAY,flight$LATE_AIRCRAFT_DELAY)
cor.test(flight$DEPARTURE_DELAY,flight$AIRLINE_DELAY)
cor.test(flight$DEPARTURE_DELAY,flight$SECURITY_DELAY)

#overview after first cleaning
miss_airport<-sapply(airport, function(x) sum(is.na(x))) / nrow(airport) * 100
na_percentages<-sapply(flights, function(x) sum(is.na(x))) / nrow(flight) * 100
airline_na<-sapply(airline, function(x) sum(is.na(x))) / nrow(airline) * 100

sort(round(na_percentages,3))
#keep long/lat for further analysis
sort(round(miss_airport,3))
sort(round(airline_na,3))

#use median imputation to deal with missing values
flights$DEPARTURE_TIME[is.na(flights$DEPARTURE_TIME)] <-median(flights$DEPARTURE_TIME,na.rm = T)
flights$DEPARTURE_DELAY[is.na(flights$DEPARTURE_DELAY)] <-median(flights$DEPARTURE_DELAY,na.rm = T)
flights$TAXI_OUT[is.na(flights$TAXI_OUT)] <-median(flights$TAXI_OUT,na.rm = T)
flights$WHEELS_OFF[is.na(flights$WHEELS_OFF)] <-median(flights$WHEELS_OFF,na.rm = T)
flights$WHEELS_ON[is.na(flights$WHEELS_ON)] <-median(flights$WHEELS_ON,na.rm = T)
flights$TAXI_IN[is.na(flights$TAXI_IN)] <-median(flights$TAXI_IN,na.rm = T)
flights$WHEELS_ON[is.na(flights$WHEELS_ON)] <-median(flights$WHEELS_ON,na.rm = T)
flights$ELAPSED_TIME[is.na(flights$ELAPSED_TIME)] <-median(flights$ELAPSED_TIME,na.rm = T)
flights$ARRIVAL_TIME[is.na(flights$ARRIVAL_TIME)] <-median(flights$ARRIVAL_TIME,na.rm = T)
flights$AIR_TIME[is.na(flights$AIR_TIME)] <-median(flights$AIR_TIME,na.rm = T)
flights$ARRIVAL_DELAY[is.na(flights$ARRIVAL_DELAY)] <-median(flights$ARRIVAL_DELAY,na.rm = T)


#this part code is to extract part of code that can be used in tableau as presentation
#the variables are selected carefully so that the tableau plot can be created and visualized as expected also in tableau
#do field calculation to create a date variable

flight$DEPARTURE_TIME[is.na(flight$DEPARTURE_TIME)] <-median(flight$DEPARTURE_TIME,na.rm = T)
flight$DEPARTURE_DELAY[is.na(flight$DEPARTURE_DELAY)] <-median(flight$DEPARTURE_DELAY,na.rm = T)
flight$TAXI_OUT[is.na(flight$TAXI_OUT)] <-median(flight$TAXI_OUT,na.rm = T)
flight$WHEELS_OFF[is.na(flight$WHEELS_OFF)] <-median(flight$WHEELS_OFF,na.rm = T)
flight$WHEELS_ON[is.na(flight$WHEELS_ON)] <-median(flight$WHEELS_ON,na.rm = T)
flight$TAXI_IN[is.na(flight$TAXI_IN)] <-median(flight$TAXI_IN,na.rm = T)
flight$WHEELS_ON[is.na(flight$WHEELS_ON)] <-median(flight$WHEELS_ON,na.rm = T)
flight$ELAPSED_TIME[is.na(flight$ELAPSED_TIME)] <-median(flight$ELAPSED_TIME,na.rm = T)
flight$ARRIVAL_TIME[is.na(flight$ARRIVAL_TIME)] <-median(flight$ARRIVAL_TIME,na.rm = T)
flight$AIR_TIME[is.na(flight$AIR_TIME)] <-median(flight$AIR_TIME,na.rm = T)
flight$ARRIVAL_DELAY[is.na(flight$ARRIVAL_DELAY)] <-median(flight$ARRIVAL_DELAY,na.rm = T)





flight_delay<-subset(flight, select =c(YEAR,MONTH,DAY,DAY_OF_WEEK,AIRLINE,
                                       ORIGIN_AIRPORT,DESTINATION_AIRPORT,
                                       SCHEDULED_DEPARTURE,SCHEDULED_ARRIVAL,
                                       ARRIVAL_DELAY,DEPARTURE_DELAY,FLIGHT_NUMBER,
                                       DEPARTURE_TIME,ARRIVAL_TIME))
write.csv(flight_delay,"flight_delay.csv")
str(flight_delay)
summary(flight_delay)
dim(flight_delay)

flight_delay_cleaned<-read.csv("flight_delay_cleaned.csv")
dim(flight_delay_cleaned)
na_percentages1<-sapply(flight_delay_cleaned, function(x) sum(is.na(x))) / nrow(flight_delay_cleaned) * 100
na_percentages1
flight_delay

#create a linear model with selected variables that may correlated to departure delay
mymod<-lm(DEPARTURE_DELAY~ARRIVAL_TIME+DEPARTURE_TIME+ARRIVAL_DELAY+SCHEDULED_ARRIVAL+SCHEDULED_DEPARTURE, data = flight_delay_cleaned)
summary(mymod)
library(ROSE)
eval_data<-subset(flight_delay_cleaned, select = c(DEPARTURE_DELAY,ARRIVAL_TIME,DEPARTURE_TIME,ARRIVAL_DELAY,SCHEDULED_ARRIVAL,SCHEDULED_DEPARTURE))
library(corrplot)
corrplot(flight_delay_cleaned)
plot(resid(mymod) , main = "residual vs fitted values scatter plot")

hist(mymod$residuals, # draw the histogram of residuals
     breaks = 1000, # number of breaks
     main = 'Histogram and Density of Residuals', # main title
     xlab='Residuals', # x-axis label
     probability=TRUE, xlim = c(0,100)) 

library(ggplot2)
flights$CANCELLED_f <-as.factor(flights$CANCELLED)
ggplot(data=flights, aes(x=CANCELLED_f))+geom_bar(aes(fill=CANCELLED_f), alpha=0.5)


#lets set a target variable first, in this project, our target variable is arrival delay or departure delay, as the result, we try to build a model to predict our delays
#however, arrival delay is not normal distributed
# hist(flights$DEPARTURE_DELAY, breaks = 20)
# hist(flights$ARRIVAL_DELAY,breaks = 20)
#log transformation
# hist(log(flights$ARRIVAL_DELAY), breaks = 20)
hist(flights$DEPARTURE_DELAY, breaks =20)
#hist(log(flights$DEPARTURE_DELAY),breaks = 20) # negative ddelay refers not delay causing NaNs
#flights$delay<-log(flights$DEPARTURE_DELAY)
# flights$delay[is.nan(flights$delay)] <-0
# flights$delay[is.na(flights$delay)]<-median(flights$delay,na.rm = T)
# flights$delay[is.infinite(flights$delay)]<-0
# summary(flights$delay)
#ifelse(flights$delay>0,"delay","normal")
#but log transformation will produce NaN 
#mod.fit<-lm(flights$delay~.,data = flights)
#this will produce error
#after exploring linear model, the problem is about the raw data export from tableau cannot be used ,
#so try to convert date variable in r using package
library(lubridate)
flights$DATE<-paste(trimws(flights$DAY),"/",trimws(flights$MONTH),"/",trimws(flights$YEAR))
flights$DATE<-gsub("/","-",flights$DATE)
#then char type is converted to date data type
flights$DATE<-parse_date_time(flights$DATE, orders="dmy")
str(flights$DATE)
# flights$DATE<-as.Date.character(flights$DATE,"%d%b%Y")
# flights$date<-strptime(flights$DATE,"%m/%d/%Y")
# head(flights$date)
#flight_ts<-subset(flights, select =c(DATE,ARRIVAL_DELAY,DESTINATION_AIRPORT))


#since the complexity of data, I try to group dataset by different airlines/airports so we can analysis partially
#1. exclude the cancelled flight / calculate cancellation rate
#cancelled ==1
cancelled_flights<-subset(flights, flights$CANCELLED == 1)


unique(flights$AIRLINE)
sort(airline$IATA_CODE) == sort(unique(flights$AIRLINE))
#therefore, we can combine/join two tables by key 

#although we have ignore the cancellation reasons variables as the large number of missing values, we still want to 
#have some insights that how the existing data in this factors influences our analysis

reason_delay <- c("AIR_SYSTEM_DELAY",
                  "SECURITY_DELAY","AIRLINE_DELAY",
                 "LATE_AIRCRAFT_DELAY","WEATHER_DELAY")

summary(flight[ ,reason_delay])


## create boxplot of delay metrics ---> although we drop them from dataset, still want to have a look at some insights within
delay_plot<-boxplot(flight[ ,reason_delay],  main="Delay Boxplots")
delay_plot
summary(flight$DEPARTURE_DELAY)
na_percentages<-sapply(flight$DEPARTURE_DELAY, function(x) sum(is.na(x))) / nrow(flight$DEPARTURE_DELAY) * 100
#summary average total departure time by each airlines
airline.avg.departure.delay <- aggregate(flights$DEPARTURE_DELAY, by=list(flights$AIRLINE), mean, na.rm=T)
names(airline.avg.departure.delay) <- c("AirlineCode", "Mean.Arrival.Delay")
airline.avg.departure.delay

#ordered from least to most
airline.avg.departure.delay <- airline.avg.departure.delay[order(airline.avg.departure.delay$Mean.Arrival.Delay), ]
airline.avg.departure.delay <- airline.avg.departure.delay[ ,c(3,1,2)]
airline.avg.departure.delay

#summary average delay by month
month_avg_delay <- aggregate(flights$DEPARTURE_DELAY, by=list(flights$MONTH), mean, na.rm=T)
names(month_avg_delay)<-c("Month","Mean_Delay")
month_avg_delay
#order ascending
month_avg_delay <- month_avg_delay[order(month_avg_delay$Mean_Delay), ]
month_avg_delay

#summary average delay by week
week_avg_delay <- aggregate(flights$DEPARTURE_DELAY, by=list(flights$DAY_OF_WEEK), mean, na.rm=T)
names(week_avg_delay)<-c("Day_OF_WEEK","Mean_Delay")
week_avg_delay
plot.ts(week_avg_delay$Mean_Delay)

#order ascending
week_avg_delay <- week_avg_delay[order(week_avg_delay$Mean_Delay), ]
week_avg_delay




#arrange data group by date
delay_ts<-aggregate(flights$DEPARTURE_DELAY, by = list(flights$DATE), mean, na.rm=T)
dim(delay_ts)
str(delay_ts)
names(delay_ts)<-c("Date","average_delay")
delay_ts$Date <- as.Date(delay_ts$Date)
myts<-ts(delay_ts[,c("average_delay")])

head(myts)
delay_ts$Date<-factor(delay_ts$Date)
levels(delay_ts$Date)
#start time series model analysis arima model 

library(TTR)
library(TSA)
library(forecast)
#plot time series plot
avg_delay<-delay_ts$average_delay
date <-delay_ts$Date

ggplot() +
  geom_line(data = delay_ts, aes(x = Date, y = average_delay)) + ylab('average delay time')

plot.ts(date,avg_delay,type="l" , main ="Time series of average delay time (min)" , xlab ="Time", ylab="average delay time")

#plot ts plot along days
day<-1:365
#fit in linear model 
#linear model y = 10.3296+-0.0069*day
mod.fit<-lm(delay_ts$average_delay~day, data = delay_ts)
summary(mod.fit)

plot(mod.fit)
plot(day,avg_delay)
abline(mod.fit,col="red")
#analysis model
#since the residual does not plot around zero
#so the model does not fit well, since the residual plot has a pattern, the points for qq plot does not lie on the straight line,
#therefore their distribution does not look normal. 
plot(resid(mod.fit), main = "Residual Plot")
abline(0,0,col="red")
hist(resid(mod.fit), breaks = 20)
#better model
mod.fit2<-lm(avg_delay~day+I(day^2), data = delay_ts)
mod.fit3<-lm(avg_delay~day+I(day^2)+I(day^3), data = delay_ts)
summary(mod.fit2)
summary(mod.fit3)
hist(resid(mod.fit2), breaks = 20)
#there is significant difference between two model 
anova(mod.fit2,mod.fit3)
#auto correlation function that explains how delay correlated to time as well as in the time series model
par(mfrow=c(2,2))
acf(avg_delay, type = "correlation", lag.max = 365, main ="sample autocorrelation function")
pacf(avg_delay, lag.max = 365, main ="partial autocorrelation function")
acf(avg_delay, type = "correlation", lag.max = 20, main ="sample autocorrelation function")
pacf(avg_delay, lag.max = 20, main ="partial autocorrelation function")
acf(resid(mod.fit), lag.max = 365, main ="sample acf for residuals from linear regression")

#arima
avg_delay_ts<-ts(avg_delay)
plot(avg_delay)
avg_delay_diff1<-diff(avg_delay_ts, lag = 1)
plot(avg_delay_diff1)
avg_delay_diff2<-diff(avg_delay_ts, lag = 2)
plot(avg_delay_diff2)

#bad model
m1.css<-arima(avg_delay_ts, order = c(1,1,1), method = "CSS")
m1_res<-m1.css$residuals
plot.ts(m1_res)
acf(m1_res)
qqnorm(m1_res) #it's very likely to seem normal, fitting on a straight line

adf.test(avg_delay_ts)
m1.ml<-arima(avg_delay_ts, order = c(1,1,1), method = "ML")
par(mfrow=c(1,1))
m1_res2<-m1.ml$residuals
plot.ts(m1_res2)
acf(m1_res2)
qqnorm(m1_res2)


pred_delay<-predict(m1.ml, a.head=100)
pred_delay


mod.forecast<-HoltWinters(avg_delay_ts,gamma = FALSE)

#myforecast<-forecast.HoltWinters(mod.forecast,h=8)
mod.forecast$fitted
plot(mod.forecast)
filter<-Arima(avg_delay_ts, order = c(1,1,1))
plot(forecast(filter, h=60))
d1<-diff(avg_delay_ts, lag=1)
par(mfrow=c(1,1))
fit1<-auto.arima(d1, seasonal = F)
tsdisplay(residuals(fit1), lag.max=45, main='(4,0,2) Model Residuals')

m2.ml<-arima(avg_delay_ts, order = c(4,0,2), method = "ML")
m2.ml
tsdiag(m2.ml)
pred_delay2<-predict(m2.ml, a.head=365)
filter2<-Arima(avg_delay_ts, order = c(4,0,2))
plot(forecast(filter2, h=100))

mod.forecast<- HoltWinters(myts, beta = FALSE, gamma = FALSE)
mod.forecast
plot(mod.forecast)
