# =====================================================================
# CSE487/587
# Author: Khushboo Tekchandani
# Email: ktekchan@buffalo.edu
# =====================================================================

# need to install the following two packages in CCR(at least)
# install.packages("forecast")
# install.packages("fpp")
# data path /gpfs/courses/cse587/spring2015/data/hw2/data

library(forecast)
library(fpp)

# To read the stocklist, and loop all files
#fileList <- list.files(path="G:/School/Spring 2015/DIC/HW2/Data/", pattern=".csv", full.names=TRUE)
fileList <- list.files(path="/gpfs/courses/cse587/spring2015/data/hw2/data", pattern=".csv", full.names=TRUE)

NASDAQ_linear <- function(filename){
  
  # if file is not empty
  if(file.info(filename)[1]==0){
    
  }
  else{
    
    fileLen<-length(readLines(filename, warn=FALSE))   
    
    if(fileLen < 755){
      
    }
    else{
      
      print(filename)
      # read one csv file into variable (DO NOT EDIT)
      textData=read.csv(file=filename, header=T)
      
      # convert txt data to time-series data, in day unit (DO NOT EDIT)
      tsData = ts(rev(textData$Adj.Close),start=c(2012, 1),frequency=365)
      
      # define train data (DO NOT EDIT)
      trainData = window(tsData, end=c(2014,14))
      
      # define test data (DO NOT EDIT)
      testData = window(tsData, start=c(2014,15))
      
      # MAE row vector (DO NOT EDIT)
      MAE = matrix(NA,1,length(testData))
      
      fitDataLM = tslm(trainData~trend+season)
      forecastDataLM = forecast(fitDataLM, h=length(testData))

      # calculate Mean Absolute Error 
      for(i in 1:length(testData))
      {
        MAE[1,i] = abs(forecastDataLM$mean[i] - testData[i])
      }
      
      # Calculate the sum of MAE
      sumMAE = sum(MAE[1,1:10])
      
      stock = basename(filename)
      df = data.frame(stock, sumMAE)
    }
  }
}

#Loop the function on all the files in fileList
output <- (lapply(fileList, NASDAQ_linear))

#Remove output that give NULL values
output[vapply(output,is.null,logical(1L))] <- NULL

#Convert the list to data frame
as.data.frame(output)
temp <- do.call(rbind, output)

#Sort by MAE sum
final <- temp[order(temp$sumMAE),]

#Get the top 10 stocks with minimum sum of MAE
finalLM <- head(final,10)
print("The top 10 stocks with the minimum sum of MAE using Linear Regression Model")
print (finalLM)

# plot the top 10 minimum sum of MAE in 3 models respectively
plot(x=1:10, finalLM$sumMAE, col = "blue", main = "Linear Regression model", xlab = "Stock Index", ylab = "MAE sum")
lines(x=1:10, finalLM$sumMAE, lw = 2, col = "red")


