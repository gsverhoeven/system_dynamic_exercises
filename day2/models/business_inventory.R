# Meadows p 60 Car inventory with delays

# https://github.com/JimDuggan/SDMR/blob/master/models/04%20Chapter/Extra%20Examples/FixedDelay.R

#########################################
# Set time period and step (days)
START <- 0
FINISH <- 100
STEP <- 1

simtime <- seq(START, FINISH, by = STEP)

#########################################
# Set initial values and parameter values
stocks <- c(sInventory_of_cars_on_the_lot = 200)


# Need to keep track of history OUTSIDE the ode() function
salesHistory <- data.frame(Time = simtime,
                            Value = vector(mode = "numeric",length = length(simtime)))


orders_to_factoryHistory <- data.frame(Time = simtime,
                                       Value = vector(mode = "numeric",length = length(simtime)))

source("models/moving_average.R")
source("models/stock_lookback.R")

#########################################
# Create SD model
model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)), {

    # outflows (sales)
    
    if (time <= 25)
      customer_demand <- 20
    else  # demand shock at t = 26
      customer_demand <- 22
    
    sales <- min(sInventory_of_cars_on_the_lot, customer_demand) 
    
    # Need to keep track of sales history
    salesHistory$Value[which(salesHistory$Time == time)] <<- sales
    
    # inflows (deliveries)
    
    # need to average sales over the time period aPerception_delay
    if (aPerception_delay > 0) {
      perceived_sales = moving_average(salesHistory, aPerception_delay, time) 
    } else {perceived_sales = sales}
    
    desired_inventory = perceived_sales * 10 
    discrepancy = desired_inventory - sInventory_of_cars_on_the_lot
    
    # reponse delay
    orders_to_factory = max(perceived_sales + (discrepancy / aResponse_delay), 0) 
    
    # Need to keep track of orders history
    orders_to_factoryHistory$Value[which(orders_to_factoryHistory$Time == time)] <<- orders_to_factory
    
    if (time <= 5)
      deliveries <- 20
    if (time > 5) 
      # look back delivery_delay time points
      deliveries <- stock_lookback(orders_to_factoryHistory, aDelivery_delay, time)
    
    dC_dt <- deliveries - sales
    
    # use for debugging
    #cat("Time=", time, "perceived_sales=", perceived_sales, "discrepancy=", discrepancy, "orders_to_factory=", orders_to_factory, "\n")
    
    return(list(c(dC_dt),
                customer_demand = customer_demand,
                perceived_sales= perceived_sales,
                desired_inventory = desired_inventory,
                discrepancy = discrepancy,
                orders_to_factory = orders_to_factory,
                deliveries = deliveries,
                sales = sales
                
))
  }) 
}


# <!-- From stackoverflow:
# Most solvers of deSolve use an automatic internal time step, that adjusts itself, 
# depending on the roughness or smoothness of the system. The use of if statements or if()functions in the model
# function is not a good idea for two reasons: (i) the time steps may not be hit exactly and (2) the model function 
# (i.e. the derivative) should ideally be continuous and differentiable and avoid step-wise behavior, even 
# if the solvers are quite robust in such cases.

# There are two explicit methods that do not adapt the time step: the euler method and the rk4 method. They are implemented in two ways:
# -->