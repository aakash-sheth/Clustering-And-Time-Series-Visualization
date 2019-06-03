fund=read.csv("fundamentals.csv")
data=subset(fund,select=c("Ticker.Symbol","Gross.Margin","Operating.Margin","Pre.Tax.ROE","Profit.Margin"))

distances = dist(data$Gross.Margin, method = "euclidean")
clusterFund= hclust(distances, method = "ward.D2")
plot(clusterFund,sub = " Gross Margin")
rect.hclust(clusterFund, k=4, border = "red")

clusterGroups= cutree(clusterFund, k = 4)

table(clusterGroups)

data$cluster_Gross.Margin=as.factor(clusterGroups)


###
distances1 = dist(data$Operating.Margin, method = "euclidean")
clusterFund1= hclust(distances1, method = "ward.D2")
plot(clusterFund1,sub = "Operating Margin")
rect.hclust(clusterFund1, k =4, border = "red")

clusterGroups1= cutree(clusterFund1, k = 4)

table(clusterGroups1)

data$cluster_Operating.Margin=as.factor(clusterGroups1)


####
distances2 = dist(data$Pre.Tax.ROE, method = "euclidean")
clusterFund2= hclust(distances2, method = "ward.D2")
plot(clusterFund2,sub = "Pre Tax-ROE")
rect.hclust(clusterFund2, k =4, border = "red")

clusterGroups2= cutree(clusterFund2, k = 4)

table(clusterGroups2)

data$cluster_Pre.Tax.ROE=as.factor(clusterGroups2)



####
distances3 = dist(data$Profit.Margin, method = "euclidean")
clusterFund3= hclust(distances3, method = "ward.D2")
plot(clusterFund3, sub = "Profit Margin")
rect.hclust(clusterFund3, k =4, border = "red")

clusterGroups3= cutree(clusterFund3, k = 4)

table(clusterGroups3)

data$cluster_Profit.Margin=as.factor(clusterGroups3)

write.csv(data, file="proj.csv")
#


str(data)

# finding range of cluster groups
c1g1_max=max(data$Gross.Margin[data$cluster_Gross.Margin==1])
c1g1_min=min(data$Gross.Margin[data$cluster_Gross.Margin==1])
c1g2_max=max(data$Gross.Margin[data$cluster_Gross.Margin==2])
c1g2_min=min(data$Gross.Margin[data$cluster_Gross.Margin==2])
c1g3_max=max(data$Gross.Margin[data$cluster_Gross.Margin==3])
c1g3_min=min(data$Gross.Margin[data$cluster_Gross.Margin==3])
c1g4_max=max(data$Gross.Margin[data$cluster_Gross.Margin==4])
c1g4_min=min(data$Gross.Margin[data$cluster_Gross.Margin==4])

c2g1_max=max(data$Operating.Margin[data$cluster_Operating.Margin==1])
c2g1_min=min(data$Operating.Margin[data$cluster_Operating.Margin==1])
c2g2_max=max(data$Operating.Margin[data$cluster_Operating.Margin==2])
c2g2_min=min(data$Operating.Margin[data$cluster_Operating.Margin==2])
c2g3_max=max(data$Operating.Margin[data$cluster_Operating.Margin==3])
c2g3_min=min(data$Operating.Margin[data$cluster_Operating.Margin==3])
c2g4_max=max(data$Operating.Margin[data$cluster_Operating.Margin==4])
c2g4_min=min(data$Operating.Margin[data$cluster_Operating.Margin==4])

c3g1_max=max(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==1])
c3g1_min=min(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==1])
c3g2_max=max(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==2])
c3g2_min=min(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==2])
c3g3_max=max(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==3])
c3g3_min=min(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==3])
c3g4_max=max(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==4])
c3g4_min=min(data$Pre.Tax.ROE[data$cluster_Pre.Tax.ROE==4])

c4g1_max=max(data$Profit.Margin[data$cluster_Profit.Margin==1])
c4g1_min=min(data$Profit.Margin[data$cluster_Profit.Margin==1])
c4g2_max=max(data$Profit.Margin[data$cluster_Profit.Margin==2])
c4g2_min=min(data$Profit.Margin[data$cluster_Profit.Margin==2])
c4g3_max=max(data$Profit.Margin[data$cluster_Profit.Margin==3])
c4g3_min=min(data$Profit.Margin[data$cluster_Profit.Margin==3])
c4g4_max=max(data$Profit.Margin[data$cluster_Profit.Margin==4])
c4g4_min=min(data$Profit.Margin[data$cluster_Profit.Margin==4])


#Rank 1 Gross margin
c1g4_min
c1g4_max
#Rank 2 Gross margin
c1g3_min
c1g3_max
#Rank 3 Gross margin
c1g1_min
c1g1_max
#Rank 4 Gross margin
c1g2_min
c1g2_max

#Rank 1 Operating.Margin
c2g1_min
c2g1_max
#Rank 2 Operating.Margin
c2g2_min
c2g2_max
#Rank 3 Operating.Margin
c2g4_min
c2g4_max
#Rank 4 Operating.Margin
c2g3_min
c2g3_max


#Rank 1 cluster_Pre.Tax.ROE
c3g1_min
c3g1_max
#Rank 2 cluster_Pre.Tax.ROE
c3g2_min
c3g2_max
#Rank 3 cluster_Pre.Tax.ROE
c3g3_min
c3g3_max
#Rank 4 cluster_Pre.Tax.ROE
c3g4_min
c3g4_max

#Rank 1 cluster_Profit.Margin
c4g1_min
c4g1_max
#Rank 2 cluster_Profit.Margin
c4g2_min
c4g2_max
#Rank 3 cluster_Profit.Margin
c4g3_min
c4g3_max
#Rank 4 cluster_Profit.Margin
c4g4_min
c4g4_max

str(data)

for(i in 1:1781){
  if(data$cluster_Gross.Margin[i]=="1"){
    data$Rank_Gross.Margin[i]=1
  } else if(data$cluster_Gross.Margin[i]=="2") {
    data$Rank_Gross.Margin[i]=2
  } else if(data$cluster_Gross.Margin[i]=="4") {
    data$Rank_Gross.Margin[i]=3
  }  else {
    data$Rank_Gross.Margin[i]=4
  }  
}


for(i in 1:1781){
  if(data$cluster_Operating.Margin[i]=="1"){
    data$Rank_Operating.Margin[i]=1
  } else if(data$cluster_Operating.Margin[i]=="2") {
    data$Rank_Operating.Margin[i]=2
  } else if(data$cluster_Operating.Margin[i]=="3") {
    data$Rank_Operating.Margin[i]=3
  }  else {
    data$Rank_Operating.Margin[i]=4
  }  
}


for(i in 1:1781){
  if(data$cluster_Pre.Tax.ROE[i]=="1"){
    data$Rank_Pre.Tax.ROE[i]=1
  } else if(data$cluster_Pre.Tax.ROE[i]=="2") {
    data$Rank_Pre.Tax.ROE[i]=2
  } else if(data$cluster_Pre.Tax.ROE[i]=="3") {
    data$Rank_Pre.Tax.ROE[i]=3
  }  else {
    data$Rank_Pre.Tax.ROE[i]=4
  }  
}

for(i in 1:1781){
  if(data$cluster_Profit.Margin[i]=="1"){
    data$Rank_Profit.Margin[i]=1
  } else if(data$cluster_Profit.Margin[i]=="2") {
    data$Rank_Profit.Margin[i]=2
  } else if(data$cluster_Profit.Margin[i]=="3") {
    data$Rank_Profit.Margin[i]=3
  }  else {
    data$Rank_Profit.Margin[i]=4
  }  
}

for(i in 1:1781){
  data$Growth_index[i]=data$Rank_Gross.Margin[i]+data$Rank_Operating.Margin[i]+data$Rank_Pre.Tax.ROE[i]+data$Rank_Profit.Margin[i] }
table(data$Growth_index)


hist(data$Growth_index)
data
#clustering on securities data set using GICS sector variable
securities=read.csv("securities.csv")
securities$Num_GICS.Sector=as.numeric(securities$GICS.Sector)

sec_distances = dist(securities$Num_GICS.Sector, method = "euclidean")
clustersector= hclust(sec_distances, method = "ward.D2")
plot(clustersector,sub = "GISC.Sector")
rect.hclust(clustersector, k=11, border = "red")

clusterGroups= cutree(clustersector, k = 11)

table(clusterGroups)

securities$fact_GICS.Sector=as.factor(clusterGroups)
table(securities$fact_GICS.Sector)
write.csv(data,file = "Final_project.csv")


#####Time series

#reading dataset
getwd()
prices = read.csv("prices-split-adjusted.csv")
str(prices)

prices = subset(prices, prices$symbol == "T" | prices$symbol == "CTL" | prices$symbol == "FTR" |
                  prices$symbol == "LVLT" | prices$symbol == "VZ")

pricesT = subset(prices, prices$symbol == "T")
str(pricesT)
pricesCTL = subset(prices, prices$symbol == "CTL")
str(pricesCTL)
pricesFTR = subset(prices, prices$symbol == "FTR")
str(pricesFTR)
pricesLVLT = subset(prices, prices$symbol == "LVLT")
str(pricesLVLT)
pricesVZ = subset(prices, prices$symbol == "VZ")
str(pricesVZ)

#basic plots for Closing prices
plot(pricesT$date, pricesT$close,type = "b",
     main = "Securities Closing Price",
     xlab = "Date", ylab = "Price", col = "Black", ylim = c(0,60),
     sub = "Black for T, Green for CTL, Yellow for FTR, Red for LVLT, Blue for VZ")
lines(pricesCTL$date, pricesCTL$close, col="green")
lines(pricesFTR$date, pricesFTR$close, col = "yellow")
lines(pricesLVLT$date, pricesLVLT$close, col = "red")
lines(pricesVZ$date, pricesVZ$close, col = "blue")

#basic plots for daily low
plot(pricesT$date, pricesT$low,type = "b",
     main = "Securities Daily Low",
     xlab = "Date", ylab = "Price", col = "Black", ylim = c(0,80),
     sub = "Black for T, Green for CTL, Yellow for FTR, Red for LVLT, Blue for VZ")
lines(pricesCTL$date, pricesCTL$low, col="green")
lines(pricesFTR$date, pricesFTR$low, col = "yellow")
lines(pricesLVLT$date, pricesLVLT$low, col = "red")
lines(pricesVZ$date, pricesVZ$low, col = "blue")

#basic plots for daily high
plot(pricesT$date, pricesT$high,type = "b",
     main = "Securities Daily High",
     xlab = "Date", ylab = "Price", col = "Black", ylim = c(0,80),
     sub = "Black for T, Green for CTL, Yellow for FTR, Red for LVLT, Blue for VZ")
lines(pricesCTL$date, pricesCTL$high, col="green")
lines(pricesFTR$date, pricesFTR$high, col = "yellow")
lines(pricesLVLT$date, pricesLVLT$high, col = "red")
lines(pricesVZ$date, pricesVZ$high, col = "blue")

#basic plots for opening prices
plot(pricesT$date, pricesT$open,type = "1",
     main = "Securities Daily Opening Price",
     xlab = "Date", ylab = "Price in $", col = "Black", ylim = c(0,60),
     sub = "Black for T, Green for CTL, Yellow for FTR, Red for LVLT, Blue for VZ")
lines(pricesCTL$date, pricesCTL$open, col="green")
lines(pricesFTR$date, pricesFTR$open, col = "yellow")
lines(pricesLVLT$date, pricesLVLT$open, col = "red")
lines(pricesVZ$date, pricesVZ$open, col = "blue")

#basic plots for daily volumes traded

plot(pricesCTL$date, pricesCTL$volume,type = "b",
     main = "CenturyLink Inc Daily Trading Volume",
     xlab = "Date", ylab = "Volume")

plot(pricesFTR$date, pricesFTR$volume,type = "b",
     main = "Frontier Communications Daily Trading Volume",
     xlab = "Date", ylab = "Volume")

plot(pricesVZ$date, pricesVZ$volume,type = "b",
     main = "Verizon Communications
     Daily Trading Volume",
     xlab = "Date", ylab = "Volume")

plot(pricesT$date, pricesT$volume,type = "b",
     main = "AT&T Inc
     Daily Trading Volume",
     xlab = "Date", ylab = "Volume")

plot(pricesLVLT$date, pricesLVLT$volume,type = "b",
     main = "Level 3 Communications Daily Trading Volume",
     xlab = "Date", ylab = "Volume")

library(plyr)
library(rlang)
require(forecast)

#forecasting for FTR for next 90 days as time series

timeseriesFTR = ts(pricesFTR, start=c(2010, 1), end=c(2016, 1), frequency=365)
plot(timeseriesFTR[,3:7], main = "Timeseries for security FTR")

# FTR Closing Price foreast
arima_fit1 = auto.arima(timeseriesFTR[,4])
arima_forecast1 = forecast(arima_fit1, h = 90)
plot(arima_forecast1, main = " Forecast for price for FTR")

#FTR opening price forecast
arima_fit2 = auto.arima(timeseriesFTR[,7])
arima_forecast2 = forecast(arima_fit2, h = 90)
plot(arima_forecast2, main = " Forecast for FTR daily volume traded")


#forecasting for T for next 90 days as time series
timeseriesT = ts(pricesT, start=c(2010, 1), end=c(2016, 1), frequency=365)
plot(timeseriesT[,3:7], main = "Timeseries for security T")

# FTR Closing Price foreast
arima_fit3 = auto.arima(timeseriesT[,4])
arima_forecast3 = forecast(arima_fit3, h = 90)
plot(arima_forecast3, main = " Forecast for price for T")

#FTR opening price forecast
arima_fit4 = auto.arima(timeseriesT[,7])
arima_forecast4 = forecast(arima_fit4, h = 90)
plot(arima_forecast4, main = " Forecast for T daily volume traded")


#forecasting for LVLT for next 90 days as time series
timeseriesLVLT = ts(pricesLVLT, start=c(2010, 1), end=c(2016, 1), frequency=365)
plot(timeseriesLVLT[,3:7], main ("LVLT timeseries (Open"))
airTS = ts(timeseriesLVLT)
plot(airTS[,3:7], main = "Timeseries for security LVLT")

# FTR Closing Price foreast
arima_fit5 = auto.arima(timeseriesLVLT[,4])
arima_forecast5 = forecast(arima_fit5, h = 90)
plot(arima_forecast5, main = " Forecast for price for LVLT")

#FTR opening price forecast
arima_fit6 = auto.arima(timeseriesLVLT[,7])
arima_forecast6 = forecast(arima_fit6, h = 90)
plot(arima_forecast6, main = " Forecast for daily volumes traded for LVLT")


#forecasting for CTL for next 5 days as time series
timeseriesCTL = ts(pricesCTL, start=c(2010, 1), end=c(2016, 1), frequency=365)
plot(timeseriesCTL[,3:7], main = "Timeseries for CTL")

# FTR Closing Price foreast
arima_fit7 = auto.arima(timeseriesCTL[,4])
arima_forecast7 = forecast(arima_fit7, h = 90)
plot(arima_forecast7, main = " Forecast for price for CTL")

#FTR opening price forecast
arima_fit8 = auto.arima(timeseriesCTL[,7])
arima_forecast8 = forecast(arima_fit8, h = 90)
plot(arima_forecast8, main = " Forecast for volume traded for CTL")


#forecasting for CTL for next 5 days as time series
timeseriesVZ = ts(pricesVZ, start=c(2010, 1), end=c(2016, 1), frequency=365)
plot(timeseriesVZ[,3:7], main = "Timeseries for VZ")

# FTR Closing Price foreast
arima_fit9 = auto.arima(timeseriesVZ[,4])
arima_forecast9 = forecast(arima_fit9, h = 90)
plot(arima_forecast9, main = " Forecast for price for VZ")

#FTR opening price forecast
arima_fit10 = auto.arima(timeseriesVZ[,7])
arima_forecast10 = forecast(arima_fit10, h = 90)
plot(arima_forecast10, main = " Forecast for volume traded for VZ")
#end
