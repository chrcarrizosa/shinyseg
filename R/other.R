
add_pheno = function(values, label, params) {
  new_id = paste0("pheno", values[["pheno_total"]] + 1)
  
  insertUI(
    selector = paste0("#pheno", values[["pheno_total"]]),
    where = "afterEnd",
    ui = 
      div(
        fluidRow(
          column(3, p(HTML(paste0("&nbsp;", label)), style = "color:#4B4B4B;background-color:#FFEC98;")),
          column(4, tags$div(class = "inline", selectInput(inputId = paste0(new_id, "_dist"), label = "hazards",
                                                           choices = c("Weibull", "Log-logist"), selected = "Weibull"))),
          column(3, tags$div(class = "inline", numericInput(inputId = paste0(new_id, "_f1v"), label = HTML("f<sub>1</sub>=exp"),
                                                            min = -3, max = +3, step = 0.1, value = params[1]))),
          column(2, tags$div(class = "inline", selectInput(inputId = paste0(new_id, "_f1fx"), label = "x",
                                                           choices = c("f0", "f2"), selected = "f2")))
        ),
        fluidRow(id = new_id,
                 style = "margin-bottom:15px;",
                 column(3, numericInput(inputId = paste0(new_id, "_f0a"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> shape") else NULL,
                                        min = 1, max = 5, step = 0.1, value = params[2])),
                 column(3, numericInput(inputId = paste0(new_id, "_f0b"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> scale") else NULL,
                                        min = 1, max = 500, step = 5, value = params[3])),
                 column(3, numericInput(inputId = paste0(new_id, "_f2a"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> shape") else NULL,
                                        min = 1, max = 5, step = 0.1, value = params[4])),
                 column(3, numericInput(inputId = paste0(new_id, "_f2b"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> scale") else NULL,
                                        min = 1, max = 500, step = 5, value = params[5]))
        )
      )
  )
  
  values[["pheno_vector"]] = c(values[["pheno_vector"]], label)
  
  # Update sensitivity analysis choices
  values[["flb_v"]] = c(values[["flb_v"]], paste0(new_id, c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v")))
  
  
  # Update risk factor phenotype choices
  if(values[["factor_total"]]> 0)
    lapply(seq(values[["factor_total"]]), function(x)
      updateSelectizeInput(
        inputId = paste0("factor", x, "_pheno"),
        choices = values[["pheno_vector"]],
        selected = values[[paste0("factor", x, "_pheno")]])
    )
  
  values[["pheno_total"]] = values[["pheno_total"]] + 1
}



rmv_pheno = function(values, all = FALSE) {
  
  if(all)
    to_remove = paste0("pheno", 1:values[["pheno_total"]])
  else
    to_remove = paste0("pheno", values[["pheno_total"]])
  
  removeUI(
    selector = paste0("div:has(> #", to_remove, ")"),
    multiple = TRUE
  )
  
  # Update sensitivity analysis choices
  values[["flb_v"]] = setdiff(values[["flb_v"]], as.vector(outer(to_remove, c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v"), paste0)))
  
  
  length(values[["pheno_vector"]]) = values[["pheno_total"]] - length(to_remove)
  
  # Update risk factor phenotype choices
  if(values[["factor_total"]]> 0)
    lapply(seq(values[["factor_total"]]), function(x)
      updateSelectizeInput(
        inputId = paste0("factor", x, "_pheno"),
        choices = values[["pheno_vector"]],
        selected = values[[paste0("factor", x, "_pheno")]])
    )
  
  values[["pheno_total"]] = values[["pheno_total"]] - length(to_remove)
}



add_factor = function(values, label, params) {
  new_id = paste0("factor", values[["factor_total"]] + 1)
  
  insertUI(
    selector = paste0("#factor", values[["factor_total"]]),
    where = "afterEnd",
    ui = 
      div(fluidRow(id = new_id,
                   column(3, style = ifelse(values[["factor_total"]] == 0, "margin-top: 30px;", "margin-top: 5px;"), p(HTML(paste0("&nbsp;", label)), style = "color:#4B4B4B;background-color:#FFCF80;")),
                   column(6, selectizeInput(inputId = paste0(new_id, "_pheno"),
                                            label = if(values[["factor_total"]] == 0) "phenotypes" else NULL,
                                            choices = values[["pheno_vector"]],
                                            multiple = TRUE)),
                   column(3, numericInput(inputId = paste0(new_id, "_risk"), label = if(values[["factor_total"]] == 0) "log(risk)" else NULL,
                                          min = -3, max = 3, step = 0.1, value = params))
      ))
  )
  
  values[["factor_vector"]] = c(values[["factor_vector"]], label)
  updateTextInput(inputId = "factor_name", value = "")
  
  
  # Update sensitivity analysis choices
  values[["flb_v"]] = c(values[["flb_v"]], paste0(new_id, "_risk"))
  
  
  values[["factor_total"]] = values[["factor_total"]] + 1
}



rmv_factor = function(values, all = FALSE) {
  
  if(all)
    to_remove = paste0("factor", 1:values[["factor_total"]])
  else
    to_remove = paste0("factor", values[["factor_total"]])
  
  removeUI(
    selector = paste0("div:has(> #", to_remove, ")"),
    multiple = TRUE
  )
  
  # Update sensitivity analysis choices
  values[["flb_v"]] = setdiff(values[["flb_v"]], paste0(to_remove, "_risk"))
  
  length(values[["factor_vector"]]) = length(values[["factor_vector"]]) - length(to_remove)
  
  values[["factor_total"]] = values[["factor_total"]] - length(to_remove)
}



add_fhelp = function(values) {
  insertUI(
    selector = paste0("#fhelp", values[["fhelp_total"]]),
    where = "afterEnd",
    ui = fluidRow(id = paste0("fhelp", values[["fhelp_total"]]+1),
                  column(6, style = "margin-top:-10px",
                         numericInput(inputId = paste0("fhelp", values[["fhelp_total"]] + 1, "_p"),
                                      label = NULL, value = 0, min = 0, max = 1, step = 0.05)),
                  column(5, style = "margin-top:-10px",
                         numericInput(inputId = paste0("fhelp", values[["fhelp_total"]] + 1, "_q"),
                                      label = NULL, value = 1, min = 1, max = 100, step = 1))
    )
  )
  values[["fhelp_total"]] = values[["fhelp_total"]] + 1
}



rmv_fhelp = function(values) {
  id = c(paste0("fhelp", values[["fhelp_total"]], "_p"), paste0("fhelp", values[["fhelp_total"]], "_q"))
  removeUI(
    selector = paste0("div:has(> #", id, ")"),
    multiple = TRUE
  )
  values[["fhelp_total"]] = values[["fhelp_total"]] - 1
}