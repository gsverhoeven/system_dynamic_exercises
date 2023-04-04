#########################################################################
# Set the time period and step
START <- 0
FINISH <- 200
STEP <- 0.25


simtime <- seq(START, FINISH, by = STEP)


#########################################################################
# Set the stocks and auxiliaries

stocks <- c(sCapital = 5, 
            sResource = 1000)

auxs <- c(aDesired.Growth = 0.07,
          aDepreciation = 0.05,
          aCost.Per.Investment = 2.00,
          aFraction.Reinvested = 0.12,
          aRevenue.Per.Unit = 3.00)

#########################################################################
# Create the function for the model and the list to return for output

model <- function(time, stocks, auxs){
  
  with(as.list(c(stocks, auxs)), {
    
    aExtr.Efficiency <- func.Efficiency(sResource)
    fExtraction <- aExtr.Efficiency * sCapital
    aTotal.Revenue <- aRevenue.Per.Unit * fExtraction
    aCapital.Costs <- sCapital * 0.10
    aProfit <- aTotal.Revenue - aCapital.Costs
    aCapital.Funds <- aFraction.Reinvested * aProfit
    aMaximum.Investment <- aCapital.Funds / aCost.Per.Investment
    aDesired.Investment <- sCapital * aDesired.Growth
    fInvestment <- min(aMaximum.Investment,
                       aDesired.Investment)
    fDepreciation <- sCapital * aDepreciation
    dS_dt <- fInvestment - fDepreciation
    dR_dt <- -fExtraction
    
    return(list(c(dS_dt, dR_dt),
                CapitalCosts = aCapital.Costs,
                Capital = sCapital,
                DesiredInvestment = aDesired.Investment,
                MaximumInvestment = aMaximum.Investment,
                Investment = fInvestment,
                Depreciation = fDepreciation,
                Extraction = fExtraction))
  })
}
