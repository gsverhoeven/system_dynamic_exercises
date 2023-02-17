# growth model

#########################################
# Set time period and step
START <- 0
FINISH <- 10
STEP <- 1

simtime <- seq(START, FINISH, by = STEP)

#########################################
# Set initial values and parameter values
stocks <- c(water_in_tub = 50)
auxs <- c()



#########################################
# Create SD model
model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)), {
    inflow <- ifelse(time > 4, 5, 0)
    outflow <- 5
    dC_dt <- inflow - outflow
    return (list(c(dC_dt)))
  }) 
}
