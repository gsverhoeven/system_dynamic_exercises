# customer model

#########################################
# Set time period and step
START <- 2015
FINISH <- 2050
STEP <- 1

simtime <- seq(START, FINISH, by = STEP)

#########################################
# Set initial values and parameter values
stocks <- c(sCustomers = 10000)
auxs <- c(aGrowthFraction = 0.08, 
          aDeclineFraction = 0.03)

#########################################
# Create SD model
model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)), {
    fRecruits <- sCustomers * aGrowthFraction
    fLosses <- sCustomers * aDeclineFraction
    dC_dt <- fRecruits - fLosses
    return (list(c(dC_dt),
                 Recruits = fRecruits, 
                 Losses = fLosses,
                 GF = aGrowthFraction, 
                 DF = aDeclineFraction))
  }) 
}
