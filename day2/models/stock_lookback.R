stock_lookback <- function(data, window, time){
  # look back window datapoints and return that value
  return(data$Value[which(data$Time == (time - window))])
}


# test function
testdata <- data.frame(Time = simtime,
                       Value = vector(mode = "numeric",length = length(simtime)))


testdata$Value <- 1:nrow(testdata)

# test function
stock_lookback(testdata, window = 4, time = 6) == 3


# delay 0, should give current stock value
stock_lookback(testdata, window = 0, time = 6) == 7