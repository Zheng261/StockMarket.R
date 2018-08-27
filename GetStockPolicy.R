install.packages("stocks")
library(stocks)

###PRICE ANALYSIS ###
stocksList = c("FB")

### So far- returns a data frame analyzing what happened the last x times that the stock experienced some amount of gain or loss
### over some number of days.
### Threshold must be a positive value - to look at losses of the same value rather than gains, set the drop parameter to TRUE.
getStockData <- function(stocksList,daysUntilPeak = 3, threshold = 0.05, drop = FALSE, open=FALSE, normalize=TRUE, daysTracked = 14, from="1950-01-01",to=Sys.Date()) {
  threshold = abs(threshold)
  thisStock= stocksList[1]
  if (open) {
    marketTime = ".Open"
  } else {
    marketTime = ".Close"
  }
  # Grabs the data for this stock
  attempt <- getSymbols(thisStock, src = "yahoo", from=from,to=to)
  plot(get(thisStock)[,paste0(thisStock,marketTime)],main=paste0(thisStock,marketTime))
  stockHistStats = get(thisStock)[,paste0(thisStock,marketTime)]
  
  ### Initiaize list of stocks to have each day shifted forward to calculate change over time
  stockPricesShifted =  stockHistStats

  for (day in 1:daysUntilPeak) {
    ### Deletes last row 
    stockPricesShifted =  stockPricesShifted[-c(nrow(stockPricesShifted)),]
    
    ### Appends copy of appropriate row to front (so these days will have zero change when % gain is calculated)
    toAdd = stockHistStats[daysUntilPeak-(day-1),]
    index(toAdd) = index(stockPricesShifted[day,])
    stockPricesShifted = rbind(toAdd,stockPricesShifted)
  }
  
  ### Assign names such that every entry in stockPricesShifted represents the price of a stock (daysUntilPeak) before the given row date 
  ### Ex. 5/24/17 $14 with daysUntilPeak = 2 means that the stock was $14 on 5/22/17 at open or close.
  index(stockPricesShifted) = index(stockHistStats)
  
  ### Gets change, for each day of stocks, over daysUntilPeak days. 
  priceChanges = (stockHistStats-stockPricesShifted)/stockHistStats
  
  # Deletes all price changes in the last x weeks of the total range. It's impossible to get what happens after them if we're tracking that far,
  # since the data we would need is in the future. 
  priceChanges = priceChanges[which(!index(priceChanges)%in%index(stockHistStats)[(nrow(stockHistStats)-daysTracked):nrow(stockHistStats)]),]
  
  if (drop) {
    indexesPeaked = which(priceChanges < -threshold)
  } else {
    indexesPeaked = which(priceChanges > threshold)
  }
 
  #Removes everything that is within 2 of another entry, if we don't want to overweigh especially heavy peaks. We may want to just know what happens
  #immediately after the first peak.
  if (normalize) {
    toRemove = c()
    if (length(indexesPeaked) > 2) {
      for (iter in 3:(length(indexesPeaked))) {
        if (abs(indexesPeaked[iter] - indexesPeaked[iter-2]) < 3 | abs(indexesPeaked[iter] - indexesPeaked[iter-1] < 3)) {
          toRemove = c(toRemove,-which(indexesPeaked%in%indexesPeaked[iter]))
        }
      }
    }
    if (length(toRemove) > 0) {
      indexesPeaked = indexesPeaked[toRemove]
    }
  }
  
  ### Gets everything that is either above or below the threshold.
  stockPricesExt = stockHistStats[indexesPeaked]
  
  # Iterates through some next number of weeks after every spike, get prices each day
  for (i in c(1:daysTracked)) {
    # Get all the prices i days after the peaks
    incrementedList = stockHistStats[indexesPeaked+i]
    # Match dates
    index(incrementedList) = index(stockPricesExt)
    # Add new column name according to the i date
    names(incrementedList) <- paste(names(incrementedList),i)
    stockPricesExt = cbind(incrementedList,stockPricesExt)
  }
  
  # Sets an analysis data frame as a  global variable
  analysisDF = data.frame(matrix(nrow=nrow(stockPricesExt),ncol=10))
  names(analysisDF) = c("Date",paste0("Price",daysUntilPeak,"DaysAgo"),"PriceOfSpike","%ChangeSpike","MinPriceInRange",
                        "MinPriceTime","%ChangeMin","MaxPriceInRange","MaxPriceTime","%ChangeMax")
  for (i in 1:nrow(stockPricesExt)) {
    analysisDF[i,"Date"] = as.character(index( stockPricesExt)[i])
    analysisDF[i,"PriceOfSpike"] =  stockPricesExt[i,ncol(stockPricesExt)]
    analysisDF[i,paste0("Price",daysUntilPeak,"DaysAgo")] =  stockHistStats[indexesPeaked[i]-daysUntilPeak,]
    analysisDF[i,"%ChangeSpike"] =  (analysisDF[i,"PriceOfSpike"]-analysisDF[i,paste0("Price",daysUntilPeak,"DaysAgo")])/analysisDF[i,"PriceOfSpike"]
    analysisDF[i,"MinPriceInRange"] = stockPricesExt[i,which(stockPricesExt[i,] == min(stockPricesExt[i,]))][,1]
    analysisDF[i,"MinPriceTime"] = names(stockPricesExt)[which(stockPricesExt[i,] == min(stockPricesExt[i,]))][1]
    analysisDF[i,"%ChangeMin"] =  (analysisDF[i,"MinPriceInRange"]-analysisDF[i,"PriceOfSpike"])/analysisDF[i,"PriceOfSpike"]
    analysisDF[i,"MaxPriceInRange"] = stockPricesExt[i,which(stockPricesExt[i,] == max(stockPricesExt[i,]))][,1]
    analysisDF[i,"MaxPriceTime"] = names(stockPricesExt)[which(stockPricesExt[i,] == max(stockPricesExt[i,]))][1]
    analysisDF[i,"%ChangeMax"] =  (analysisDF[i,"MaxPriceInRange"]-analysisDF[i,"PriceOfSpike"])/analysisDF[i,"PriceOfSpike"]
  }
  ## We set a more statistically informative data frame as a global
  analysisDF <<- analysisDF
  
  return(stockPricesExt)
}



#length(which(analysisDF$"%ChangeMax" > 0.05))/nrow(analysisDF)
#stockPricesExt[which(analysisDF$"%ChangeMax" > 0.05),]
#median(analysisDF[which(analysisDF$"%ChangeMax" > 0.05),]$"%ChangeMin")
#median(analysisDF[which(analysisDF$"%ChangeMax" > 0.05),]$"%ChangeMax")


#### Some function that we pass in to optimize for the policy - we may want to weigh the expected profit (MOST IMPORTANT), median profit, the percentage of times where we profit, 
#### ,the mean number of days until we sell, and the mean loss (if we do decide to sell)
opt.fun <- function(expectedProfit,medianProfit,percentProfited,meanLoss,avgDaysTilSold) {
  ## mean loss is negative
  return (expectedProfit + 0.25*medianProfit + 0.01*percentProfited + 0.15*meanLoss - 0.004*avgDaysTilSold )
}

## Finds the best policy to maximize gain and minimize probability of loss
findBestPolicy <- function(stockPricesExt,opt.fun,percentGainLower = 0.03,percentGainUpper=0.10,percentLossLower=0.03,percentLossUpper=0.10,gainInt=0.005,lossInt=0.005) {
   #All the gain and loss sell policy combinations to brute force over
   gainXticks = seq(percentGainLower,percentGainUpper,gainInt)
   lossXticks = -seq(percentLossLower,percentLossUpper,lossInt)
   policyFrame = data.frame(matrix(nrow=length(gainXticks)*length(lossXticks),ncol=11))
   colnames(policyFrame) <- c("SellAfterGain","SellAfterLoss","PolicyValue","ExpectedProfit","MedianProfit",
                              "ProfitSD","PercentProfited","MeanLossGivenLoss","MeanGainGivenGain","AvgDaysTilSold","%Unsold")
   policyFrame$"SellAfterGain" = rep(gainXticks,each=length(lossXticks))
   policyFrame$"SellAfterLoss" = rep(lossXticks,times=length(lossXticks))
   

   for (gainSell in 1:length(gainXticks)) {
     print(paste("Iteration:",gainSell,"out of",length(gainXticks)))
     for (lossSell in 1:length(gainXticks)) {
      ### Separates the spike prices and the prices of the elapsed days
      actualPrices = stockPricesExt[,ncol(stockPricesExt)]
      daysElapsed = stockPricesExt[,(ncol(stockPricesExt)-1):1]
      
      ## Finds the percent change (from the original value) at each day
      percentChangeFrame = sweep(daysElapsed,1,actualPrices,'/')-1
      
      ## Keeps a data frame that keeps track of the beginning date of each stock, original value, sold value, profit %, and days until sold
      profitFrame = data.frame(matrix(nrow=nrow(stockPricesExt),ncol=5))
      colnames(profitFrame) <- c("Date","OrigValue","SoldValue","Profit%UnderPolicy","DaysTilSold")
      profitFrame$Date = index(stockPricesExt)
      profitFrame$OrigValue = actualPrices
    
      ## For each starting point, figure out what the expected profit is under this policy. We try to maximize it.
      for (spike in 1:nrow(stockPricesExt)) {
          gainPlaces = which(percentChangeFrame[spike,] > gainXticks[gainSell])
          lossPlaces = which(percentChangeFrame[spike,] < lossXticks[lossSell])
      
          # We sell the first time we either rise a certain % under our policy, or drop a certain % 
          if (length(c(gainPlaces,lossPlaces)) > 0) {
             timeToSell = min(c(lossPlaces,gainPlaces))
          } else {
             # Else we sell for the market price by the end of the two weeks, since afterwards the behavior of the stock cannot be accounted for.
             timeToSell = ncol(percentChangeFrame)
          }
          #### Stores the number of days until we sold, the value at which we sold at, and the profit %
          profitFrame[spike,"DaysTilSold"] = timeToSell
          profitFrame[spike,"SoldValue"] = daysElapsed[spike,timeToSell]
          profitFrame[spike,"Profit%UnderPolicy"] = percentChangeFrame[spike,timeToSell]
      }
      
      ### Begins calculating statistics for the entire profit frame ### 
      # Stores the percent of stocks that we profited with
      if (length(which(profitFrame$`Profit%UnderPolicy` > 0)) > 0) {
        percentProfited <- length(which(profitFrame$`Profit%UnderPolicy` > 0))/nrow(profitFrame)
        meanGain <- mean(profitFrame$"Profit%UnderPolicy"[which(profitFrame$"Profit%UnderPolicy" > 0)])
      } else {
        percentProfited = 0
        meanGain = 0
      }
      expectedProfit <-  mean(profitFrame$"Profit%UnderPolicy")
      medianProfit <-  median(profitFrame$"Profit%UnderPolicy")
      profitSD <- sd(profitFrame$"Profit%UnderPolicy")
      avgDaysTilSold <- mean(profitFrame$"DaysTilSold")
      numHeldTilEnd <- length(which(profitFrame$"DaysTilSold" == ncol(percentChangeFrame)))
      if (length(which(profitFrame$"Profit%UnderPolicy" < 0))>0) {
        meanLoss <- mean(profitFrame$"Profit%UnderPolicy"[which(profitFrame$"Profit%UnderPolicy" < 0)])
      } else {
        meanLoss = 0
      }
      policyValue = opt.fun(expectedProfit,medianProfit,percentProfited,meanLoss,avgDaysTilSold)
      
      ### Grabs right row of array - each gain sell option has x adjacent rows, where x is the number of loss sell options ###
      rightIndex = lossSell+(gainSell-1)*length(gainXticks)
      policyFrame[rightIndex,"PolicyValue"] = policyValue
      policyFrame[rightIndex,"ExpectedProfit"] = expectedProfit
      policyFrame[rightIndex,"MedianProfit"] = medianProfit
      policyFrame[rightIndex,"ProfitSD"] = profitSD
      policyFrame[rightIndex,"PercentProfited"] = percentProfited
      policyFrame[rightIndex,"MeanLossGivenLoss"] = meanLoss
      policyFrame[rightIndex,"MeanGainGivenGain"] = meanGain
      policyFrame[rightIndex,"AvgDaysTilSold"]= avgDaysTilSold
      policyFrame[rightIndex,"%Unsold"]= numHeldTilEnd/nrow(stockPricesExt)
     }
   }
   return(policyFrame)
}



stocksList = c("FB")
stockPricesExt <- getStockVolatility("FB",daysUntilPeak = 1, threshold = 0.03, drop = FALSE, open=FALSE, normalize=TRUE, daysTracked = 14)
#stockAnalyzed <- analyzeStock("FB",daysUntilPeak = 1, threshold = 0.03, drop = FALSE, open=FALSE, normalize=TRUE, daysTracked = 14)
policyFrame <- findBestPolicy(stockPricesExt,opt.fun)

policyFrame[which(policyFrame$ExpectedProfit == max(policyFrame$ExpectedProfit)),]
policyFrame[which(policyFrame$PolicyValue == max(policyFrame$PolicyValue)),]

policyFrame[which(policyFrame$ExpectedProfit == min(policyFrame$ExpectedProfit)),]
policyFrame[which(policyFrame$PolicyValue == min(policyFrame$PolicyValue)),]
