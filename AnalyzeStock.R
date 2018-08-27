# Strat: BUY STOCK RIGHT BEFORE HITTING AFTER HOURS
# STOCKS TEND TO GO UP AFTER HOURS NORMALLY 
# BUY LONG STRADDLE TWO DAYS BEFORE EARNINGS, GET SOMETHING REASONABLY FAR AND ATM TO MAXIMIZE VEGA. LOOK FOR HIGH GAMMA TO PRICE RATIO. 
# DELTA MUST BE THE SAME BETWEEN PRICE AND PUT, PREFERABLY HIGH GAMMA
# SELL RIGHT BEFORE EARNINGS
# ALL OF THIS IS BAD ADVICE TBH

#Hold XONE until September 9
#Sell at -7.80 
#8.40*0.93
stocksList = c("XONE")
stockPricesExt <- GETSTOCKDATA(stocksList,daysUntilPeak = 3, threshold = 0.10, drop = FALSE, open=TRUE, normalize=TRUE, daysTracked = 14,from="2008-01-01",to=Sys.Date())
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


policyFrame <- findBestPolicy(stockPricesExt,opt.fun,percentGainLower = 0.005,percentGainUpper=0.14,percentLossLower=0.005,percentLossUpper=0.14)
#FB put sell 3% gain or 5% loss
#172.94
#SELL
172.94*c(1.03,0.95)

#Exone put sell 3% gain or 12% loss 
#7.63
7.63*c(1.03,0.88)

#CMCM put sell 3% gain or 1.5% loss
#10.95
10.95*c(1.03,0.985)

#RGIS
# 20.44
# SellAfterGain SellAfterLoss PolicyValue ExpectedProfit MedianProfit   ProfitSD PercentProfited MeanLossGivenLoss MeanGainGivenGain AvgDaysTilSold %Unsold
#75  0.015        -0.095  -0.0455212    -0.03556863  -0.05237538 0.08339618             0.4       -0.09541946        0.05420761            4.5     0.2

policyFrame[which(policyFrame$ExpectedProfit == max(policyFrame$ExpectedProfit)),]
policyFrame[which(policyFrame$PolicyValue == max(policyFrame$PolicyValue)),]

policyFrame[which(policyFrame$ExpectedProfit == min(policyFrame$ExpectedProfit)),]
policyFrame[which(policyFrame$PolicyValue == min(policyFrame$PolicyValue)),]

gain = 0.01
loss = -0.07
policyFrame[which( abs(policyFrame$SellAfterGain - gain) <= 0.0001 & (abs(policyFrame$SellAfterLoss - loss) <= 0.0001)),]
