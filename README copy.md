### STOCK POLICY BRUTE FORCE OPTIMIZATION ###
This project grabs useful information about historical data, given a stock symbol and its performance over the past x number of days.

Functions: 
1) GetStockData - Takes a bunch of information about a stock, user-inputted, and returns multiple data frames containing information about what happened when the stock was at this state before within a past time range (min/max price over next some number of days, % gain/loss, etc.)
2) findBestPolicy - Returns a data frame analyzing the performance of every policy combination of (sell for profit after x gain, stop loss sell after y loss). Takes in an optimization function provided by the user. 
