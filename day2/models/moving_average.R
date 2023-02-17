moving_average <- function(data, window, time){
  # special case of zero window: just take current value
  if (time == 0) return(data$Value[which(data$Time == 0)])
  if (window > time) window = time
  # calculate average of last window datapoints in data
  accum = 0
  for (t in (time - window):(time - 1)) {
    accum = accum + data$Value[which(data$Time == t)]
  }
  return(accum/window)
}

# test function
testdata <- data.frame(Time = simtime,
                                       Value = vector(mode = "numeric",length = length(simtime)))


testdata$Value <- 1:nrow(testdata)

head(testdata, 10)

moving_average(testdata, window = 5, time = 6) == 4
# PM function does not work correctly for window 0
moving_average(testdata, window = 0, time = 6) == 7

moving_average(testdata, window = 5, time = 0) == 1