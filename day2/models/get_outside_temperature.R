get_outside_temperature <- function(time) {
  # returns outside temperature based on 24 hour cycle
  data <- c(10, 10, 9, 8, 7, 5, 3, 1, -1, -3, -4, -5, -5, 
            -5, -4, -3, -1, 1, 3, 5, 7, 8, 9, 10, 10)
  
  # linear interpolation
  outside_temp_fun <- approxfun(x = seq(0, 24, by = 1),
                                y = data, 
                                method = "linear",
                                yleft = 0, yright = 24)
  
  # repeat same pattern every 24 hours
  time <- time %% 24
  return(outside_temp_fun(time))
}
