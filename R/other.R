
clearUI = function(values) {
  
  # Clear factors
  if(values[["factor_total"]] > 0){
    to_remove = paste0("factor", 1:values[["factor_total"]])
    removeUI(selector = paste0("div:has(> #", to_remove, ")"), multiple = TRUE)
    values[["flb_v"]] = setdiff(values[["flb_v"]], paste0("factor", 1:values[["factor_total"]], "_risk"))
    length(values[["factor_vector"]]) = 0
    values[["factor_total"]] = 0
  }
  
  # Clear phenotypes
  if(values[["pheno_total"]] > 0){
    to_remove = paste0("pheno", 1:values[["pheno_total"]])
    removeUI(selector = paste0("div:has(> #", to_remove, ")"), multiple = TRUE)
    values[["flb_v"]] = setdiff(values[["flb_v"]], as.vector(outer(paste0("pheno", 1:values[["pheno_total"]]), c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v"), paste0)))
    length(values[["pheno_vector"]]) = 0
    values[["pheno_total"]] = 0
  }
  
}