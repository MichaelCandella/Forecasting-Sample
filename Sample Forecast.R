library(astsa)
library(xts)
library(ggplot2)
library(forecast)

Year<-Car_Sales$Year
Sales<- Car_Sales$Sales

Time_Series<-ts(Car_Sales_Final,start=1977,end=2017)
Year<-as.character(Year)

#Plot Initial Time-Series
ggplot(Car_Sales,aes(x=Car_Sales_Final$Year,y=Sales,group=1))+geom_line(color="Red",size=1.5)+
  ggtitle("Car Sales in the U.S from 1977-2017")+
  xlab("Periods (Annual 1977-2017)")+
  ylab("Sales (Thousands of Cars)")+
  theme(axis.title=element_text(size=13,face="bold"),plot.title=element_text(size=20,hjust = .5, face="bold"),
        axis.text=element_text(size=14,face="bold"),axis.text.x = element_text(size=7,face="bold"))


#Difference the data to show if the model will be integrated
diff_sales<-diff(sales)

#ACF and PACF for Normal and Differenced Data
acf2(Sales)
acf2(diff_sales)

#Find Fit for Model
auto.arima(Sales)

#Comparing and Constrasting Residual Diagnostics for Model Seleciton
sarima(Sales,1,1,2)
sarima(Sales,1,0,2)

#Forcast using Fitted ARIMA(1,1,1)
forecast<-sarima.for(Sales,n.ahead=5,1,0,2)
sarima.for(Sales,n.ahead=5,1,0,2)

#Plot Forecast
print(forecast)
plot(sales)
