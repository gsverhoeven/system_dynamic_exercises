# Set the time period and step. 
START <- 0
FINISH <- 100
STEP <- 0.25
simtime <- seq(START, FINISH, by = STEP)

###########################################
# Create the function
model <- function(time, stocks, auxs){
  
  with(as.list(c(stocks, auxs)),{
    
    aAvailability <- 1 - sStock / aCapacity
    aEffect <- aAvailability / aRef.Availability
    aGrowth.Rate <- aRef.GrowthRate * aEffect
    fNet.Flow <- sStock * aGrowth.Rate
    dS_dt <- fNet.Flow
    
    return(list(c(dS_dt), 
                NetFlow = fNet.Flow,
                GrowthRate = aGrowth.Rate, 
                Effect = aEffect,
                Availability = aAvailability))
  })
}