install.packages("RCurl")
library(RCurl)
library(jsonlite)
library(plyr)
library(dplyr)
library(stocks)

### Sets work directory to be options folder
setwd("/Users/ZhengYan/Desktop/Research/StockMarket/OptionsData")

### Code adapted from https://datawookie.netlify.com/blog/2015/01/downloading-options-data-in-r-an-update/
### into a much nicer, more condensed version. 
fixJSON <- function(json){
   gsub('([^,{:]+):', '"\\1":', json)
}


### Gets options quotes for symbol
getOptionQuotes <- function(symbol, goodDates) {
     URL1 = 'https://query1.finance.yahoo.com/v7/finance/options/'
     url = paste0(URL1, symbol)
     chain = tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
     chainList <- as.list(chain)
     
     #### Gets all expiration dates available for options
     expDatesSecs <- as.data.frame(chainList$optionChain$result$expirationDates[1])
     expDates <- as.POSIXct(expDatesSecs[,1],origin="1970-01-01", tz="GMT")
    
     
     #### Build starting data frame to hold all options data
     calls <- as.list(as.data.frame(chainList$optionChain$result$options)[3])[[1]][[1]]
     callsList = data.frame(matrix(nrow=0,ncol=ncol(calls)))
     colnames(callsList) <- colnames(calls)
     putsList = callsList
    
     
     ### Grabs which indexes the dates we want are
     goodDateIndexes <- which(as.character(expDates)%in%goodDates)
     
     ### Iterates through all desirable dates
     for (thisDate in expDatesSecs[goodDateIndexes,1]) {
       print(paste("Querying date :",as.POSIXct(thisDate,origin="1970-01-01", tz="GMT")))
       dateURL = paste0(url,"?&date=",thisDate)
       chain = tryCatch(jsonlite::fromJSON(dateURL), error = function(e) NULL)
       chainList <- as.list(chain)
       calls <- as.list(as.data.frame(chainList$optionChain$result$options)[3])[[1]][[1]]
       puts <- as.list(as.data.frame(chainList$optionChain$result$options)[4])[[1]][[1]]
       tryCatch({
       calls$expiration = as.POSIXct(calls$expiration,origin="1970-01-01", tz="GMT")
       puts$expiration = as.POSIXct(puts$expiration,origin="1970-01-01", tz="GMT")
       ### Sleeps so we don't get marked as some sort of attacker
       Sys.sleep(0.5)
       index = index + 1
       callsList = rbind(callsList,calls)
       putsList = rbind(putsList,puts)
       }, error = function(err) {
         print(paste("MY_ERROR:  ",err))
       })
     }
     
     callsList$type = "CALL"
     putsList$type = "PUT"
     allList = rbind(callsList,putsList)
     return(allList)
}



### Initiate options to be whatever is desired
symbolList = c("CRON","TLRY","GCO","SONO","PVTL","KR","PLAY","FDX","DPZ","GIS","RHT","MU","NKE","KMX","ACN","PEP","COST","DAL","C","JPM","WFC","V"
               ,"BAC","IBM","NFLX","HOG","NAVI","UNH","OMC","TEVA","BOLD","ACOR","IONS","MRK","MSFT","AAPL","FB","XOM","TQQQ","JBLU","MCD","BA","SIX","SAM","HSY","KKR","ABMD","XOM","CMG","TWTR","AMD","ATVI","EA","SPY","XBI")

##  KR - 9/13
##  NKE - 9/25
##  KMX - 9/26
##  PEP - 10/2
##  COST - 10/4
##  DAL - 10/9
##  C/JPM/WFC all 10/12
##  BAC IBM NFLX HOG NAVI UNH OMC all 10/15
#### PDUFAS
## TEVA 9/16
## BOLD 10/2
## ACOR 10/5
## IONS 10/6
## PTCT 10/6
## MRK !? 11/6

### Gets last 5 days of stock data to analyze options with ###
getBasicStockInfo <- function(symbol,daysBefore=5) {
  getSymbols(symbol, src = "yahoo", from=Sys.Date()-daysBefore,to=Sys.Date())
  return(get(symbol))
}

######### START HERE FOR DATA COLLECTION #############
#### NOTE: INITIATE STRANGLE ONLY AFTER VOLATILITY DIP AT END OF DAY. STOCK BOUNCING BACK TO NULL IS BAD.
#### NOTE: PLAN TO SELL STRANGLE ONLY AFTER IV DIPS TWO DAYS IN A ROW
### NOTE: PREFER TO BUY CALLS/PUTS AFTER ONE DAY OF LOW VOLATILITY DURING RISE (HIGH POTENTIAL ENERGY, LOW REALIZATION)
# TO WATCH: NFLX, TWTR, EA, IBM, ATVI, SIX, DPZ
### Creates directories to store information for each of these stock options
for(symbol in symbolList) {
  if(!file.exists(symbol)) {
    dir.create(symbol)
  }
}

### Gets options quotes and writes them to CSV 
monthdate = Sys.Date()

### REMEMBER TO CHANGE DATE BACK TOMORROW####
pauseIndex = 1
### Dates that we want
goodDates <- c("2018-09-21","2018-10-19","2018-11-16","2018-12-21","2019-01-18","2019-06-21","2020-01-17")

### Saves all options data for stocks of interest as .csv files 
for (symbol in symbolList) {
  print(symbol)
  allList <- getOptionQuotes(symbol,goodDates)
  write.csv(allList,paste0(symbol,"/",symbol,"AllOptions",monthdate,".csv"))
  ### Sleeps every 10 seconds
  if (pauseIndex %% 10 == 0) {
    Sys.sleep(10)
  }
  pauseIndex = pauseIndex + 1
}



