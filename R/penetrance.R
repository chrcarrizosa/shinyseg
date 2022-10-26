library(flexsurv, quietly = TRUE)


get_hazards = function(x, distr, params) {
  
  # Cumulative hazards
  H = switch(
    distr,
    
    "Weibull" = -pweibull(x, lower = FALSE, log = TRUE,
                          shape = params[1], scale = params[2]),
    
    "Log-logist" = -pllogis(x, lower = FALSE, log = TRUE,
                              shape = params[1], scale = params[2])
  )
  
  # Hazards
  h = c(H[1], diff(H))
  
  return(h)
  
}

surv_penetrance = function(h) {
  
  H = lapply(h, function(i) apply(i, 2, cumsum))
  CR = lapply(H, function(i) cbind(1 - exp(-i), nonaff = 1 - exp(-rowSums(i))))
  SP = lapply(1:length(h),
              function(i) cbind((1 - c(0, CR[[i]][-nrow(CR[[i]]), "nonaff"])) * h[[i]],
                                nonaff = CR[[i]][,"nonaff"]))
  
  return(list("CR" = CR, "SP" = SP))
}
