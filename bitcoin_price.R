library(dplyr)
library(zoo)
library(forecast)
library(tseries)
library(ggplot2)
#############1. Import:

bitcoin = read.csv(file='~/Desktop/projects/R/kurs Bitcoina/bitcoin.csv', dec=',', header=TRUE, stringsAsFactors=FALSE)

#Timestamp to date type:
bitcoin$Date <- as.POSIXct(bitcoin$Timestamp, origin="1970-01-01")
bitcoin$Date <- cut(bitcoin$Date, breaks = "day")
bitcoin$Date <- as.Date(bitcoin$Date, origin = "1900-01-01")

bitcoin$Open <- as.numeric(as.character(bitcoin$Open))
bitcoin$High <- as.numeric(as.character(bitcoin$High))
bitcoin$Low <- as.numeric(as.character(bitcoin$Low))
bitcoin$Close <- as.numeric(as.character(bitcoin$Close))

#Aggregation:
aggData <- select(bitcoin, -c(Timestamp, Volume_.BTC., Volume_.Currency.)) %>% na.omit() %>%
  group_by(Date) %>% summarize(open <- first(Open), high <- max(High), low <- min(Low), close <- last(Close)) 
names(aggData) <- c('Date', 'Open', 'High', 'Low', 'Close')
aggData <- filter(aggData, aggData$Date>='2013-09-01')

date <- aggData$Date
aggData <- aggData %>% select(-Date)
aggData$Avg = apply(aggData,1,mean,na.rm=TRUE) 

#Data as time series class
bitcoin.zoo <- zoo(aggData, date)

#Average price plot:
ggplot(bitcoin.zoo$Avg, aes(x=date, y=bitcoin.zoo$Avg)) + geom_line() + labs(x="Date",y="Average price") 

#############2. Time series properties:
#stactionarity:
adf.test(bitcoin.zoo$Avg)
kpss.test(bitcoin.zoo$Avg)

#logarithmic transformation:
bitcoin.bc <- BoxCox(bitcoin.zoo$Avg, lambda=0)
ggplot(bitcoin.bc, aes(x=index(bitcoin.bc), y=bitcoin.bc)) + geom_line() + labs(x="Date",y="Logarithmic transformation") 

#differencial (lag=1):
bitcoin.diff <- diff(bitcoin.bc)
ggplot(bitcoin.diff, aes(x=index(bitcoin.diff), y=bitcoin.diff)) + geom_line() + labs(x="Date",y="Differencial values") 

adf.test(bitcoin.diff)
kpss.test(bitcoin.diff)

#autocorrelation:
Acf(bitcoin.diff,main='ACF')
Pacf(bitcoin.diff, main='PACF')

#############3. Splitting the set:
bitcoin.train <- window(bitcoin.zoo$Avg, end='2017-10-20')
bitcoin.test <- window(bitcoin.zoo$Avg, start='2017-10-21')

model <- auto.arima(bitcoin.train, ic='bic', lambda=0, trace=TRUE, approximation=FALSE)

#one-step forecast:
model_test <- Arima(bitcoin.test, model=model)
onestep <- fitted(model_test)

#plotting one-step forecast:
seria <- zoo(onestep,index(bitcoin.test))
ggplot() + geom_line(data=seria, aes(index(seria), seria, color='forecast')) + 
  geom_line(data=bitcoin.test, aes(x=index(bitcoin.test), y=bitcoin.test, color='real values')) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position = c(0.92, 0.1)) +
  scale_color_manual(values=c('green4', 'black'))

#dynamic forecast:
dynamic.f <- forecast(bitcoin.train, model=model, h=80)

#plotting dynamic forecast:
plot(dynamic.f)
lines(bitcoin.test, col = 'green4')

#############4. Fitting analysis:
criteria <- c('MAE', 'RMSE', 'MAPE', 'MASE')

#errors for one-step forecasts:
accuracy(model_test)[,criteria]

#errors for dynamic forecasts:
accuracy(dynamic.f, bitcoin.test)[,criteria]

#resudials:
arima.residuals <- residuals(model)

#randomness:
ggAcf(arima.residuals)
Box.test(arima.residuals, type='Ljung-Box', lag=1)

hist(arima.residuals)

#coefficients:
coef <- model$coef
coef.std <- sqrt(diag(model$var.coef))

ratio <- coef/(1.96*coef.std)
index <- which(abs(ratio)>=1)
print(index)
