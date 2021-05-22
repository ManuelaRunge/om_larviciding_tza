
### Repdocuded from SI Text 1 from
# 1. Stuckey EM, Smith T, Chitnis N. Seasonally Dependent Relationships between Indicators of Malaria Transmission
# and Disease Provided by Mathematical Model Simulations. PLOS Computational Biology. 2014 Sep 4;10(9):e1003812.

f_getDailyEIR <- function(param, t, selected_aEIR) {
  Time <- 1
  w <- (2 * pi) / 365 
  e <- sum(1 / factorial(0:100)) 

  phi <- -((2 * pi) / (365)) * 90 ## add phasenverschiebung

  a1 <- as.numeric(param[1])
  a2 <- as.numeric(param[2])
  b1 <- as.numeric(param[3])
  b2 <- as.numeric(param[4])

  aEIR <- selected_aEIR
  # a0=0.12
  a0 <- log(aEIR / (sum(e^(a1 * cos(w * t + phi) + b1 + sin(w * t + phi) + a2 * cos(2 * (w * t + phi)) + b2 * sin(2 * (w * t + phi))))))

  dEIR_t <- e^(a0 + a1 * cos(w * t + phi) + b1 * sin(w * t + phi) + a2 * cos(2 * (w * t + phi)) + b2 * sin(2 * (w * t + phi)))
  
  return(dEIR_t)
  
}
