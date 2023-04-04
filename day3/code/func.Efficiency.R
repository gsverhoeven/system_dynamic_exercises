# Set the x and y vectors for linearization by the `approxfun` command
x.Resource <- seq(0, 1000, by = 100)
y.Efficiency <- c(0, 0.25, 0.45, 0.63, 0.75, 0.85, 0.92, 0.96, 0.98, 0.99, 1.0)

func.Efficiency <- approxfun(x = x.Resource,
                             y = y.Efficiency,
                             method = "linear",
                             yleft = 0, yright = 1.0)