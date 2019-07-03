#HOMEWORK 04. APPLE SALES

##Author: Arturo Sánchez Palacio
##Date: 22/XI/18

#In this assignment we must predict the Apple sells for the coming years.

#In order to do this we are going to employ the following libraries:



library(dplyr) #To efficiently manipulate dataframes.
library(xts) #To work with extensible Time Series
require(ggplot2) #To build better graphs
library(ggfortify) #Plot Monthplot
library(forecast) #To use Holt (among others) models
library(urca) #To run unit roots tests to check where series are stationary or not.

#In order to do that firstly we load the data on our project:

data <- read.csv("/Users/arturosanchezpalacio/Documents/CUNEF/Predicción/Informes/Apple/apple.csv", sep = ",", dec = ".")

#We switch the NA values in the dataframe for 0's. Since each variable defines how much is earned from this product
# it makes sense to think that when these products didn't exist their earn was zero.

data[is.na(data)] <- 0

#Variables are the number of units solds each trimester (It's read as a decimal because Spanish system uses '.' as thousand
# separator instead of as decimal separator).

data <- mutate(data, iPhone = iPhone*1000)
data <- mutate(data, iPod = iPod*1000)
data <- mutate(data, iPad = iPad*1000)
data <- mutate(data, Mac = Mac*1000)



#Now in this study we are going to consider two options in order to predict the forecoming sales:

#OPTION A. We calcualate the total earning in each trimester by addding the earn from each product and then 
#build a time series of the total earn. We use this series to predict.

#OPTION B. We conduct a forecast of each product omitting the iPod since it makes no sense to predict the sale of something 
# that it is not for sale anymore.

#As this is a didactic code we are only going to forecast until 2018 (included) so afterwards it is possible to compare
#the prediction with the actual results. (Remember this code was firstly written in November 2018).

#We generate a sequence of dates from the day of the beginning of the first trimester considered until
#the last day of the last trimester considered:

rawDate <- seq(as.Date("1998/10/01"), as.Date("2016/03/31"), by = "quarter")


# First we are going to examine OPTION A.

# The first thing we do is calculating the total earnings per year:

data <- mutate(data, Total_Earnings = iPhone + iPad + iPod + Mac)

#Once this is done it is time to create the time series which will be stored in a zoo object:

Sales_A <- data$Total_Earnings


x_Sales <- xts(Sales_A, order.by = rawDate)
x_Sales <- to.quarterly(x_Sales)

Sales_Azoo <- as.zoo(x_Sales$x_Sales.Open)
names(Sales_Azoo) <- "Total earning"

##Plot Serie
autoplot(Sales_Azoo) + ggtitle("Ventas Trimestrales Apple") + xlab("Trimestres") + ylab("Ventas (en unidades)")

#Interpretation of this graph is explained in the attached paper.

#Seasonal Plot
ggfreqplot(as.ts(Sales_Azoo),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + ggtitle("Ventas Trimestrales")

#We consider serveral prediction methods. Some of them will be rapidly discarded because of the nature of our data. 
# Other will be tested and compaired.

omitted <- 4

#Data Size
observations <- length(Sales_Azoo)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),]
oSales <- window(Sales_Azoo, start = index(Sales_Azoo[1]), end = index(Sales_Azoo[observations - omitted]))




#A first forecasting option could be Simple Exponential Smoothing (SES). This idea is casted aside since it doesn't behave
# very well with data with trend or seasonal pattern.

#Since it is obvious that graphic presents a trend it is time to think of a Holt model:

fc_Holt <- holt(Sales_Azoo, h = 10)
fc_HoltDamped <- holt(Sales_Azoo, damped = TRUE, h = 10)
autoplot(Sales_Azoo) +
  autolayer(fc_Holt, series = "Holt's method", PI = FALSE) +
  autolayer(fc_HoltDamped, series = "Damped Holt's method", PI = FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Trimester") +
  ylab("Year Earnings (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

autoplot(fc_HoltDamped) +
  xlab("Trimester") + ylab("Year Earnings (millions)")

#We are going to compare this two methods (MAE and MSE):

e1 <- tsCV(Sales_Azoo, holt, h = 1)
e2 <- tsCV(Sales_Azoo, holt, damped = TRUE, h = 1)

#MSE:

mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

#MAE:

mean(abs(e1), na.rm = TRUE)
mean(abs(e2), na.rm = TRUE)

#Both errors show that it is better the second model (including damped)

#After this we are going to try the Holt-Winter's seasonal method:


fc_HWa <- hw(oSales,seasonal = "additive")
fc_HWm <- hw(oSales,seasonal = "multiplicative")


autoplot(oSales) +
  autolayer(fc_HWa, series = "HW additive forecasts", PI = FALSE) +
  autolayer(fc_HWm, series = "HW multiplicative forecasts",
            PI = FALSE) +
  xlab("Trimester") +
  ylab("Trimester Earnings (millions)") +
  ggtitle("Apple sales") +
  guides(colour = guide_legend(title = "Forecast"))


#Just with the graph we can discard the multiplicative option.

#Again we introduce damping:

fc_HWaD <- hw(oSales, damped = TRUE, seasonal = "additive")
fc_HWmD <- hw(oSales, damped = TRUE, seasonal = "multiplicative")

autoplot(Sales_Azoo) +
  autolayer(fc_HWaD, series = "HW addi damped", PI = FALSE) +
  autolayer(fc_HWmD, series = "HW multi damped", PI = FALSE) +
  guides(colour = guide_legend(title = "Sales forecasts"))



#After seeing some concrete examples we are going to try to stimate the best possible method:


best <- ets(oSales)
summary(best)

autoplot(best)

cbind('Residuals' = residuals(best),
      'Forecast errors' = residuals(best, type = 'response')) %>%
  autoplot(facet = TRUE) + xlab("Trimesters") + ylab("")

best %>% forecast(h = 10) %>%
  autoplot() +
  ylab("Trimester Earnings (millions)")

#Until this point we have used ETS models.

#We are going to face now Arima Models:

#Unique Root test is used to check if it's necessary to differentiate:

Sales_Azoo %>% ur.kpss() %>% summary()


Sales_Azoo %>% diff() %>% ur.kpss() %>% summary() #This would be close enought

#ndiffs tells the user how many times is necessary to differentiate

ndiffs(Sales_Azoo)


Best_Arima <- auto.arima(oSales, stepwise = FALSE, approximation = FALSE)
summary(Best_Arima)


oSales %>% auto.arima() %>% forecast(h = 10) %>% autoplot()


(a1 <- Best_Arima %>% forecast(h = 4) %>%
  accuracy())

(a2 <- best %>% forecast(h = 4) %>% accuracy())

#In this case ARIMA seems like a better option so let's explore a bit more this model:


checkresiduals(Best_Arima)


autoplot(forecast(Best_Arima)) + xlab("Trimesters") + ylab("Apple sales (in units)")


#Now it is time to consider OPTION B (ie. exploring each product):


#### iPad sales ####

Sales_iPad <- data$iPad
x_Sales <- xts(Sales_iPad, order.by = rawDate)
x_Sales <- to.quarterly(x_Sales)

Sales_iPadZoo <- as.zoo(x_Sales$x_Sales.Open)
names(Sales_iPadZoo) <- "Number of iPad sales"


##Plot Serie
autoplot(Sales_iPadZoo) + ggtitle("Ventas Trimestrales Apple (iPad)") + xlab("Trimestres") + ylab("Ventas")



#Seasonal Plot
ggfreqplot(as.ts(Sales_iPadZoo),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + ggtitle("Ventas Trimestrales (iPad)")




omitted <- 4

#Data Size
observations <- length(Sales_iPadZoo)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),]
oSalesiPad <- window(Sales_iPadZoo, start = index(Sales_iPadZoo[47]), end = index(Sales_iPadZoo[observations - omitted]))

#Best ETS Model

best_iPad <- ets(oSalesiPad)
summary(best_iPad)

autoplot(best_iPad)

cbind('Residuals' = residuals(best_iPad),
      'Forecast errors' = residuals(best_iPad, type = 'response')) %>%
  autoplot(facet = TRUE) + xlab("Trimesters") + ylab("")

best_iPad %>% forecast(h = 10) %>%
  autoplot() +
  ylab("Trimester Earnings (millions)")

#Best ARIMA Model


#Unique Root test is used to check if it's necessary to differentiate:

Sales_iPadZoo %>% ur.kpss() %>% summary()


Sales_iPadZoo %>% diff() %>% ur.kpss() %>% summary() #This would be close enought

#ndiffs tells the user how many times is necessary to differentiate

ndiffs(Sales_iPadZoo)

Best_Arima_iPad <- auto.arima(oSalesiPad, stepwise = FALSE, approximation = FALSE)

checkresiduals(Best_Arima_iPad)


autoplot(forecast(Best_Arima_iPad)) + xlab("Trimesters") + ylab("Apple sales (millions)")


oSalesiPad %>% auto.arima() %>% forecast(h = 10) %>% autoplot()


### iPhone sales ###

Sales_iPhone <- data$iPhone
x_Sales <- xts(Sales_iPhone, order.by = rawDate)
x_Sales <- to.quarterly(x_Sales)

Sales_iPhoneZoo <- as.zoo(x_Sales$x_Sales.Open)
names(Sales_iPhoneZoo) <- "Number of iPhone sales"


##Plot Serie
autoplot(Sales_iPhoneZoo) + ggtitle("Ventas Trimestrales Apple (iPhone)") + xlab("Trimestres") + ylab("Ventas")



#Seasonal Plot
ggfreqplot(as.ts(Sales_iPhoneZoo),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + ggtitle("Ventas Trimestrales (iPhone)")




omitted <- 4

#Data Size
observations <- length(Sales_iPhoneZoo)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),]
oSalesiPhone <- window(Sales_iPhoneZoo, start = index(Sales_iPhoneZoo[35]), end = index(Sales_iPhoneZoo[observations - omitted]))

#Best ETS Model

best_iPhone <- ets(oSalesiPhone)
summary(best_iPhone)

autoplot(best_iPhone)

cbind('Residuals' = residuals(best_iPhone),
      'Forecast errors' = residuals(best_iPhone, type = 'response')) %>%
  autoplot(facet = TRUE) + xlab("Trimesters") + ylab("")

best_iPhone %>% forecast(h = 10) %>%
  autoplot() +
  ylab("Trimester Earnings (millions)")


#Best ARIMA Model


#Unique Root test is used to check if it's necessary to differentiate:

Sales_iPhoneZoo %>% ur.kpss() %>% summary()


Sales_iPhoneZoo %>% diff() %>% ur.kpss() %>% summary() #This would be close enought

#ndiffs tells the user how many times is necessary to differentiate

ndiffs(Sales_iPhoneZoo)

Best_Arima_iPhone <- auto.arima(oSalesiPhone, stepwise = FALSE, approximation = FALSE)

checkresiduals(Best_Arima_iPhone)


autoplot(forecast(Best_Arima_iPhone)) + xlab("Trimesters") + ylab("Apple sales (millions)")


oSalesiPhone %>% auto.arima() %>% forecast(h = 10) %>% autoplot()



### Mac sales ###

Sales_Mac <- data$Mac
x_Sales <- xts(Sales_Mac, order.by = rawDate)
x_Sales <- to.quarterly(x_Sales)

Sales_MacZoo <- as.zoo(x_Sales$x_Sales.Open)
names(Sales_MacZoo) <- "Number of Mac sales"


##Plot Serie
autoplot(Sales_MacZoo) + ggtitle("Ventas Trimestrales Apple (Mac)") + xlab("Trimestres") + ylab("Ventas")



#Seasonal Plot
ggfreqplot(as.ts(Sales_MacZoo),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + ggtitle("Ventas Trimestrales (Mac)")




omitted <- 4

#Data Size
observations <- length(Sales_MacZoo)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),]
oSalesMac <- window(Sales_MacZoo, start = index(Sales_MacZoo[1]), end = index(Sales_MacZoo[observations - omitted]))


#Best ETS Model

best_Mac <- ets(oSalesMac)
summary(best_Mac)

autoplot(best_Mac)

cbind('Residuals' = residuals(best_Mac),
      'Forecast errors' = residuals(best_Mac, type = 'response')) %>%
  autoplot(facet = TRUE) + xlab("Trimesters") + ylab("")

best_Mac %>% forecast(h = 10) %>%
  autoplot() +
  ylab("Trimester Earnings (millions)")
#Best ARIMA Model


#Unique Root test is used to check if it's necessary to differentiate:

Sales_MacZoo %>% ur.kpss() %>% summary()


Sales_MacZoo %>% diff() %>% ur.kpss() %>% summary() #This would be close enought

#ndiffs tells the user how many times is necessary to differentiate

ndiffs(Sales_MacZoo)

Best_Arima_Mac <- auto.arima(oSalesMac, stepwise = FALSE, approximation = FALSE)

checkresiduals(Best_Arima_Mac)


autoplot(forecast(Best_Arima_Mac)) + xlab("Trimesters") + ylab("Apple sales (millions)")


oSalesMac %>% auto.arima() %>% forecast(h = 10) %>% autoplot()


#Finally we are going to make a global prediction based on the prediction of every product

Mac <- Best_Arima_Mac %>% forecast(h = 10)
iPad <- Best_Arima_iPad %>% forecast(h = 10)
iPhone <- Best_Arima_iPhone %>% forecast(h = 10)

total <- Mac$mean + iPad$mean + iPhone$mean
previous_estimation <- Best_Arima %>% forecast(h = 10)
total_aggregated <- previous_estimation$mean

