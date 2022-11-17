
add_pheno = function(values, label, params, sexspec) {
  new_id = paste0("pheno", values[["pheno_total"]] + 1)
  
  insertUI(
    selector = paste0("#pheno", values[["pheno_total"]]),
    where = "afterEnd",
    ui = 
      div(
        fluidRow(
          #style = "margin-top:12px;",
          column(3, p(HTML(paste0("&nbsp;&nbsp;", label)), style = "color:#4b4b4b;background-color:#fff2cc"))
        ),
        tags$table(id = new_id,
                   if(!sexspec)
                     fluidRow(
                       tags$tr(
                         tags$td(p(HTML("&nbsp;<b>a</b>&nbsp;&nbsp;"))),
                         tags$td(numericInput(inputId = paste0(new_id, "_f0a"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> shape") else NULL,
                                              min = 1, max = 5, step = 0.1, value = params[2])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f0b"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> scale") else NULL,
                                              min = 1, max = 500, step = 5, value = params[3])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f2a"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> shape") else NULL,
                                              min = 1, max = 5, step = 0.1, value = params[4])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f2b"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> scale") else NULL,
                                              min = 1, max = 500, step = 5, value = params[5])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f1v"), label = if(values[["pheno_total"]] == 0) HTML("log(f<sub>1</sub> &beta;)") else NULL,
                                              min = -3, max = +3, step = 0.1, value = params[1]))
                       )
                     )
                   else {
                     fluidRow(
                       tags$tr(
                         tags$td(p(HTML("&nbsp;<b>m</b>&nbsp;&nbsp;"))),
                         tags$td(numericInput(inputId = paste0(new_id, "_f0a_m"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> shape") else NULL,
                                              min = 1, max = 5, step = 0.1, value = params[2])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f0b_m"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> scale") else NULL,
                                              min = 1, max = 500, step = 5, value = params[3])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f2a_m"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> shape") else NULL,
                                              min = 1, max = 5, step = 0.1, value = params[4])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f2b_m"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> scale") else NULL,
                                              min = 1, max = 500, step = 5, value = params[5])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f1v_m"), label = if(values[["pheno_total"]] == 0) HTML("log(f<sub>1</sub> &beta;)") else NULL,
                                              min = -3, max = +3, step = 0.1, value = params[1]))
                       ),
                       tags$tr(
                         tags$td(p(HTML("&nbsp;<b>f</b>&nbsp;&nbsp;"))),
                         tags$td(numericInput(inputId = paste0(new_id, "_f0a_f"), label = NULL,
                                              min = 1, max = 5, step = 0.1, value = params[2])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f0b_f"), label = NULL,
                                              min = 1, max = 500, step = 5, value = params[3])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f2a_f"), label = NULL,
                                              min = 1, max = 5, step = 0.1, value = params[4])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f2b_f"), label = NULL,
                                              min = 1, max = 500, step = 5, value = params[5])),
                         tags$td(numericInput(inputId = paste0(new_id, "_f1v_f"), label = NULL,
                                              min = -3, max = +3, step = 0.1, value = params[1]))
                       )
                     )
                   }
        )
      )
  )
  
  values[["pheno_vector"]] = c(values[["pheno_vector"]], label)
    
  if(!sexspec) {
    values[["sexspec_vector"]] = c(values[["sexspec_vector"]], FALSE)
    # Update sensitivity analysis choices
    values[["flb_v"]] = c(values[["flb_v"]], paste0(new_id, c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v")))
  }
  else {
    values[["sexspec_vector"]] = c(values[["sexspec_vector"]], TRUE)
    # Update sensitivity analysis choices
    values[["flb_v"]] = c(values[["flb_v"]], paste0(new_id, c("_f0a_m", "_f0b_m", "_f2a_m", "_f2b_m", "_f1v_m",
                                                              "_f0a_f", "_f0b_f", "_f2a_f", "_f2b_f", "_f1v_f")))
  }
  
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
  values[["flb_v"]] = setdiff(values[["flb_v"]], as.vector(outer(to_remove, c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v",
                                                                              "_f0a_m", "_f0b_m", "_f2a_m", "_f2b_m", "_f1v_m",
                                                                              "_f0a_f", "_f0b_f", "_f2a_f", "_f2b_f", "_f1v_f"), paste0)))
  
  
  length(values[["pheno_vector"]]) = values[["pheno_total"]] - length(to_remove)
  length(values[["sexspec_vector"]]) = values[["pheno_total"]] - length(to_remove)
  
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
                   column(3, style = ifelse(values[["factor_total"]] == 0, "margin-top: 30px;", "margin-top: 5px;"), p(HTML(paste0("&nbsp;", label)), style = "color:#4b4b4b;background-color:#fce5cd")),
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
