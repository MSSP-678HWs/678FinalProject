library(readxl)
library(tseries)
library(ggplot2)
library(tidyverse)

#Trying to find autoregression lag of brent crude oil prices

#Start by reading in the crude oil prices
brent_crude_proces<- read_excel("brent_crude_prices.xlsx")
x<- brent_crude_proces$`Europe Brent Spot Price FOB (Dollars per Barrel)`
pacf(x, lag = 10, pl=TRUE)

#We can tell from the partial autocorrelation that he spike at lag 1 is much much higher than at subsequent lags.
#So we can say crude oil has a AR(1) structure. 


#What about co2 emissions?
co2_emissions_data<-read.csv("co2_emission.csv")
emissions<- co2_emissions_data$value

pacf(emissions, lag=20, pl=TRUE)

#This doesn't look so great. 
#Graphing CO2 Emissions
ggplot(data=co2_emissions_data, aes(date, y=value/1000000))+
  geom_line(group=1)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Aha. Now we can see that prior to 3/1/2021, all the CO2 emissions data was the same for each month.
#So let's filter for dates after that and try the lag again. 

co2_emissions_data$date<- as.Date(co2_emissions_data$date)

co2_emissions_data_after_march_2021<- co2_emissions_data |> filter(
                                                          date>"2021-03-01"
)

#Graph again:

ggplot(data=co2_emissions_data_after_march_2021, aes(date, y=value/1000000))+
  geom_line(group=1)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Let's try the autocorrelation now:
 pacf(co2_emissions_data_after_march_2021$value, lag=20, pl=TRUE)
 
 


#Graphin brent crude oil
ggplot(data=brent_crude_proces, aes(x=Date, `Europe Brent Spot Price FOB (Dollars per Barrel)`))+
  geom_line()



#Reading FTSE in 

ftse<- read.csv("FTSE 100 Historical Data.csv")
ftse$Price<- as.numeric(str_replace(ftse$Price, ",",""))

pacf(ftse$Price, lag=10, pl=TRUE)


