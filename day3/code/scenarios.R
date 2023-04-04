scenarios <- function(parameter_to_vary, start_value, n, step = 0.01, stocks, simtime, model, auxs) {
  vec <- seq(from = start_value, by = step, length.out = n)
  base <- data.frame()
  for (i in 1:length(vec)) {
    auxs[parameter_to_vary] <- vec[i]
    temp <- data.frame(ode(y = stocks, times = simtime, func = model, parms = auxs, method = "euler"))
    temp$par_value <- vec[i]
    base <- rbind(base,temp)
  }
  return(base)
}