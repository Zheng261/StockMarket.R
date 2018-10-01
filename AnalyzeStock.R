# Strat: BUY STOCK RIGHT BEFORE HITTING AFTER HOURS
# BUY LONG STRADDLE TWO DAYS BEFORE EARNINGS, GET SOMETHING REASONABLY FAR AND ATM TO MAXIMIZE VEGA. LOOK FOR HIGH GAMMA TO PRICE RATIO. 
# DELTA MUST BE THE SAME BETWEEN PRICE AND PUT, PREFERABLY HIGH GAMMA
# SELL RIGHT BEFORE EARNINGS
# ALL OF THIS IS BAD ADVICE TBH

stocksList = c("COST")
stockPricesExt <- getStockData(stocksList,daysUntilPeak = 5, threshold = 0.10, drop = FALSE, open=TRUE, normalize=TRUE, daysTracked = 14,from="2008-01-01",to=Sys.Date())
analysisDF
stockPricesExt

lolz <- stockPricesExt[which(stockPricesExt[,7] - stockPricesExt[,8]/stockPricesExt[,8]>0),]
mean((stockPricesExt[,7] - stockPricesExt[,8])/stockPricesExt[,8])
mean((stockPricesExt[,1] - stockPricesExt[,8])/stockPricesExt[,8])
mean((lolz[,6] - lolz[,7])/lolz[,7])

mean(analysisDF$"%ChangeMin")
boxplot(analysisDF$"%ChangeMin")
nrow(analysisDF)

#mean(analysisDF$"%ChangeMin" < -0.06)
#mean(analysisDF[-which(analysisDF$"%ChangeMin" < -0.10),"%ChangeMax"])
#mean(analysisDF[which(analysisDF$"%ChangeMin" < -0.10),"%ChangeMin"])
#analysisDF[which(analysisDF$"%ChangeMin"> -0.05),]

### Function to optimize what the best sell policy is 
opt.fun <- function(expectedProfit,medianProfit,percentProfited,meanLoss,avgDaysTilSold) {
  ## mean loss is negative, so we subtract
  return (expectedProfit + 0.25*medianProfit + 0.075*percentProfited + 0.14*meanLoss - 0.003*avgDaysTilSold)
}


policyFrame <- findBestPolicy(stockPricesExt,opt.fun,percentGainLower = 0.005,percentGainUpper=0.12,percentLossLower=0.005,percentLossUpper=0.12)

policyFrame[which(policyFrame$ExpectedProfit == max(policyFrame$ExpectedProfit)),]
policyFrame[which(policyFrame$PolicyValue == max(policyFrame$PolicyValue)),]

policyFrame[which(policyFrame$ExpectedProfit == min(policyFrame$ExpectedProfit)),]
policyFrame[which(policyFrame$PolicyValue == min(policyFrame$PolicyValue)),]

gain = 0.01
loss = -0.07
policyFrame[which( abs(policyFrame$SellAfterGain - gain) <= 0.0001 & (abs(policyFrame$SellAfterLoss - loss) <= 0.0001)),]
