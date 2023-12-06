library(readxl)
library(tseries)
library(ggplot2)

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

#Graphin brent crude oil
ggplot(data=brent_crude_proces, aes(x=Date, `Europe Brent Spot Price FOB (Dollars per Barrel)`))+
  geom_line()

#Graphing CO2 Emissions
ggplot(data=co2_emissions_data, aes(date, y=value/1000000))+
  geom_line(group=1)+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
