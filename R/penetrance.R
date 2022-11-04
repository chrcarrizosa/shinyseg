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

get_f = function(x, values, inputs, sensitivity = FALSE) {

  # Baseline hazards
  h_0 = sapply(seq(values[["pheno_total"]]), function(i){
    get_hazards(x, distr = inputs[["pheno_dist"]],
                params = c(inputs[[paste0("pheno", i, "_f0a")]], inputs[[paste0("pheno", i, "_f0b")]]))
  })
  colnames(h_0) = values[["pheno_vector"]]
  h_2 = sapply(seq(values[["pheno_total"]]), function(i){
    get_hazards(x, distr = inputs[["pheno_dist"]],
                params = c(inputs[[paste0("pheno", i, "_f2a")]], inputs[[paste0("pheno", i, "_f2b")]]))
  })
  colnames(h_2) = values[["pheno_vector"]]
  h_1 = sapply(seq(values[["pheno_total"]]), function(i){
    h_2[,i] * exp(inputs[[paste0("pheno", i, "_f1v")]])
  })
  colnames(h_1) = values[["pheno_vector"]]
  
  if(values[["factor_total"]] > 0) {
    # Calculate risk factors
    risks_change = exp(sapply(paste0("factor", 1:values[["factor_total"]], "_risk"), function(x) inputs[[x]]))
    riskmat = log(t(risks_change*t(values[["rf_combs"]]) + rep(1,values[["factor_total"]])*t(!values[["rf_combs"]])))
    final = exp(riskmat %*% t(matrix(values[["phenomat"]], ncol = values[["factor_total"]])))
    # Update hazards
    h_0 = lapply(1:nrow(values[["rf_combs"]]), function(i) final[i,] * h_0)
    h_1 = lapply(1:nrow(values[["rf_combs"]]), function(i) final[i,] * h_1)
    h_2 = lapply(1:nrow(values[["rf_combs"]]), function(i) final[i,] * h_2)
  }
  else {
    h_0 = list(h_0)
    h_1 = list(h_1)
    h_2 = list(h_2)
  }
  
  # Penetrance values
  f_0_all = surv_penetrance(h_0)
  f_1_all = surv_penetrance(h_1)
  f_2_all = surv_penetrance(h_2)
  
  # Penetrance values
  f = lapply(1:length(h_0), function(i) {
    cbind(reshape2::melt(f_0_all[["SP"]][[i]], varnames = c("age", "phenotype"), value.name = "f0"),
          f1 = as.vector(f_1_all[["SP"]][[i]]),
          f2 = as.vector(f_2_all[["SP"]][[i]]))
  })
  f = dplyr::bind_rows(f, .id = 'comb')
  
  # Returns
  if(!sensitivity) {
    values[["f"]] = f
    values[["CR"]] =
      cbind(reshape2::melt(f_0_all[["CR"]][[1]], varnames = c("age", "phenotype"), value.name = "f0"),
            f1 = as.vector(f_1_all[["CR"]][[1]]),
            f2 = as.vector(f_2_all[["CR"]][[1]]))
    values[["f_idx"]] = array(1:nrow(f),
                              dim = c(length(x), values[["pheno_total"]]+1, 2^values[["factor_total"]]),
                              dimnames = list(x, colnames(f_0_all[["SP"]][[1]]), seq(2^values[["factor_total"]])))
  }
  else
    return(f)
}