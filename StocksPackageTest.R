install.packages("stocks")
library(stocks)

### DROP IN PRICE ANALYSIS ###


attempt <- getSymbols("TSLA", src = "yahoo", from="2017-01-01",to=Sys.Date())
plot(TSLA[,"TSLA.Close"],main="Lol elon")
plot(TSLA[,"TSLA.Open"],main="Lol elon")
stuff = TSLA[,"TSLA.Close"]

## ONE DAY CHANGES
stuffCopy = stuff[-c(nrow(stuff)),]
stuffCopy = rbind(stuff[1,],stuffCopy)
index(stuffCopy) = index(stuff)

### Changes in stock of this day over last day
stuffChangesOneDay = (stuff-stuffCopy)/stuff

## TWO DAY CHANGES
stuffCopy = stuffCopy[-c(nrow(stuffCopy)),]
stuffCopy = rbind(stuff[1,],stuffCopy)
index(stuffCopy) = index(stuff)

### Changes in stock of this day over last day
stuffChangesTwoDays = (stuff-stuffCopy)/stuff

## THREE DAY CHANGES
stuffCopy = stuffCopy[-c(nrow(stuffCopy)),]
stuffCopy = rbind(stuff[1,],stuffCopy)
index(stuffCopy) = index(stuff)

### Changes in stock of this day over last day
stuffChangesThreeDays = (stuff-stuffCopy)/stuff

for (i in c(1:4)) {
  #Removes last row, adds a dummy to the front
  stuffCopy = stuffCopy[-c(nrow(stuffCopy)),]
  stuffCopy = rbind(stuff[1,],stuffCopy)
  index(stuffCopy) = index(stuff)
}
stuffChangesOneWeek = (stuff-stuffCopy)/stuff

#What times did Tesla stock rise more than 4% after 3 days?
#which(stuffChangesThreeDays > 0.04)
#stuff[which(stuffChangesThreeDays > 0.04)]

# Deletes everything that in the last two weeks of the total range
stuffChangesThreeDays = stuffChangesThreeDays[which(!index(stuffChangesThreeDays)%in%index(stuff)[(nrow(stuff)-15):nrow(stuff)]),]

# Deletes everything that is in the first three days
#stuffChangesThreeDays = stuffChangesThreeDays[which(!index(stuffChangesThreeDays)%in%index(stuff)[c(1:3)]),]
timeEnd = 14
threshold= -0.04

# Starts with the two weeks analysis
xTimeMin = stuff[which(stuffChangesThreeDays < threshold)]

# Iterates through next two weeks after every spike
for (i in c(1:timeEnd)) {
  incrementedList = stuff[which(stuffChangesThreeDays < threshold)+i]
  index(incrementedList) = index(xTimeMin)
  names(incrementedList) <- paste(names(incrementedList),i)
  xTimeMin = cbind(incrementedList,xTimeMin)
}
xTimeMin

analysisDF = data.frame(matrix(nrow=nrow(xTimeMin),ncol=7))
names(analysisDF) = c("Date","PriceB4Spike","PriceOfSpike","MinPriceInRange","MinPriceTime","MaxPriceInRange","MaxPriceTime")

for (i in 1:nrow(xTimeMin)) {
  analysisDF[i,"Date"] = as.character(index(xTimeMin)[i])
  analysisDF[i,"PriceOfSpike"] = xTimeMin[i,ncol(xTimeMin)]
  if (which(stuffChangesThreeDays < threshold)[i] > 3) {
    analysisDF[i,"PriceB4Spike"] =  stuff[which(stuffChangesThreeDays < threshold)[i]-3,]
  } else {
    analysisDF[i,"PriceB4Spike"] =  0
  }
  analysisDF[i,"MinPriceInRange"] = xTimeMin[i,which(xTimeMin[i,] == min(xTimeMin[i,]))][,1]
  analysisDF[i,"MinPriceTime"] = names(xTimeMin)[which(xTimeMin[i,] == min(xTimeMin[i,]))][1]
  analysisDF[i,"MaxPriceInRange"] = xTimeMin[i,which(xTimeMin[i,] == max(xTimeMin[i,]))][,1]
  analysisDF[i,"MaxPriceTime"] = names(xTimeMin)[which(xTimeMin[i,] == max(xTimeMin[i,]))][1]
}
analysisDF

length(which(analysisDF$MaxPriceInRange/analysisDF$PriceOfSpike > 1.03))
length(analysisDF$MaxPriceInRange)


getDis <- getSymbols("TSLA", src = "yahoo", from="2017-03-15",to="2017-03-30")
plot(TSLA[,"TSLA.Close"],main="lol elon")






########### TESTING HERE ############

dailyprices <- load_prices(c("FB","T"),from="2015-01-01",to=Sys.Date())
prices <- data.frame(load_prices(c("FB","T")))
class(prices)
prices$FB
plot(x=prices$FB)
rownames(prices)
fig <- gains_graph(tickers=c("T","FB"),)
fig2 <- growth_graph(tickers=c("T"))

fig3 <- growth_graph(prices=dailyprices)
dev.off()

plot(as.POSIXct(rownames(prices)),prices$FB)

prices <- load_prices(c("NFLX","AMZN"))
plot(as.POSIXct(rownames(prices)),prices[,1],by="year")
gains <- prices_gains(prices)


attempt <- getSymbols("AAPL", src = "yahoo", from="2017-01-01",to=Sys.Date())
head(attempt)
plot(AAPL[,"AAPL.Close"],main="AAPL")
candleChart(AAPL, up.col = "blue", dn.col = "yellow", theme = "white")

