# Libraries
library(shiny, quietly = TRUE)
# library(shinyjs)
# library(tidyverse) # dplyr::bind_rows() is being used!
# library(rlang) # is_empty (also in tidyverse purrr?)
library(rhandsontable, quietly = TRUE)
library(pedsuite, quietly = TRUE)
library(segregatr, quietly = TRUE)
# library(flexsurv, quietly = TRUE)
library(ggplot2, quietly = TRUE)

# Age
x = seq(1:100)

# Notes
#   Numeric validation of age not working decimals
#   Add risk factors to penetrance plots?
#   Missing ages?
#   Add phenotype number to riskplots?
#   Add risk factor pop distribution?
#   Separate baseline penetrances calculation
#   When user selects same variable in sensitivity analysis...
#   Clear uploaded pedigree if example


# Input widgets -----------------------------------------------------------------

W_input_pedigree =
  fileInput(
    inputId = "input_pedigree",
    label = "Pedigree",
    buttonLabel = "Browse...",
    placeholder = "None selected"
  )

W_input_example =
  selectInput(
    inputId = "input_example",
    label = "Load example",
    choices = list("", "Basic pedigrees" = c("Trio", "Full siblings"), "Worked examples" = c("Example 1", "Example 2")),
    selected = "")

W_toquickped =
  actionButton(
    inputId = "toquickped",
    label = "QuickPed",
    value = "Open popup",
    onclick = "window.open('https://magnusdv.shinyapps.io/quickped/')",
    style = "padding-top:2px;padding-bottom:2px;margin-top:30px;margin-left:10px;color:#52a173;border:1px solid #52a173;font-size:90%;"
  )

W_afreq =
  numericInput(
    inputId = "afreq",
    label = "log10(afreq)",
    value = -3,
    min = -5,
    max = 0,
    step = 0.1
  )

W_save = 
  actionButton(
    inputId = "save",
    label = "Save",
    style = "padding-top:4px;padding-bottom:4px;margin-top:25px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_load = 
  actionButton(
    inputId = "load",
    label = "Load",
    style = "padding-top:4px;padding-bottom:4px;margin-top:25px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_reset = 
  actionButton(
    inputId = "reset",
    label = "Reset",
    style = "padding-top:4px;padding-bottom:4px;margin-top:25px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_pheno_name = 
  textInput(
    inputId = "pheno_name",
    label = NULL,
    placeholder = "New phenotype label"
  )

W_pheno_add = 
  actionButton(
    inputId = "pheno_add",
    label = "Add",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_pheno_rmv = 
  actionButton(
    inputId = "pheno_rmv",
    label = "Undo",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%;margin-left:-10px"
  )

W_factor_name = 
  textInput(
    inputId = "factor_name",
    label = NULL,
    placeholder = "New factor label"
  )

W_factor_add =
  actionButton(
    inputId = "factor_add",
    label = "Add",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_factor_rmv = 
  actionButton(
    inputId = "factor_rmv",
    label = "Undo",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%;margin-left:-10px"
  )

W_fhelp_dist = 
  selectInput(
    inputId = "fhelp_dist",
    label = "Distribution",
    choices = c("Weibull", "Log-logist")
  )

W_fhelp1_p =
  numericInput(
    inputId = 'fhelp1_p',
    label = 'Quantile',
    value = 0.25,
    min = 0,
    max = 1,
    step = 0.05
  )

W_fhelp1_q =
  numericInput(
    inputId = 'fhelp1_q',
    label = 'Age',
    value = 40,
    min = 1,
    max = 100,
    step = 1
  )


W_fhelp2_p =
  numericInput(
    inputId = 'fhelp2_p',
    label = NULL,
    value = 0.50,
    min = 0,
    max = 1,
    step = 0.05
  )

W_fhelp2_q =
  numericInput(
    inputId = 'fhelp2_q',
    label = NULL,
    value = 50,
    min = 1,
    max = 100,
    step = 1
  )

W_fhelp3_p =
  numericInput(
    inputId = 'fhelp3_p',
    label = NULL,
    value = 0.75,
    min = 0,
    max = 1,
    step = 0.05
  )

W_fhelp3_q =
  numericInput(
    inputId = 'fhelp3_q',
    label = NULL,
    value = 60,
    min = 1,
    max = 100,
    step = 1
  )

W_fhelp_add =
  actionButton(
    inputId = "fhelp_add",
    label = "Add pair",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_fhelp_rmv =
  actionButton(
    inputId = "fhelp_rmv",
    label = "Undo",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_flb_v1 = 
  selectInput(
    inputId = "flb_v1",
    label = "Variable 1",
    choices = "afreq",
    selected = "afreq"
  )

W_flb_s1 =
  sliderInput(
    inputId = "flb_s1",
    label = NULL,
    min = -5,
    max = -1,
    value = c(-5,-1)
  )

W_flb_v2 = 
  selectInput(
    inputId = "flb_v2",
    label = "Variable 2",
    choices = "afreq",
    selected = "afreq"
  )

W_flb_s2 = 
  sliderInput(
    inputId = "flb_s2",
    label = NULL,
    min = -5,
    max = -1,
    value = c(-5,-1)
  )

W_flb_run = 
  actionButton(
    inputId = "flb_run",
    label = "Run",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )



# UI ----------------------------------------------------------------------

# Define UI
ui = fluidPage(
  
  tags$head(
    tags$style(type = "text/css",
    "
    .inline label{
        display: table-cell;
        text-align: left;
        vertical-align: middle;
        font-weight: normal;
        padding-right: 5px;
        padding-bottom: 10px;
      } 
    
     .inline .form-group{
        display: table-row;
      }
    
    .inline .selectize-input{
        display: table-row;
      }
    
    .inline .selectize-control.single .selectize-input:after{
      content: none;
    }
    
    .inline .form-control.shiny-bound-input{
      display: table-row;
      height: 20px;
      width: 50px;
      padding: 0px;
      border-radius: 0px;
      border: 0px;
    }
    ")
  ),
  
  tags$style(
    HTML(".tabbable > .nav > li > a {
            border:1px solid #DEDEDE;
          }

          .tabbable > .nav > li[class=active] > a {
            background-color: #3295b8;
            color:white
          }

          .irs .irs-single, .irs .irs-bar-edge, .irs .irs-bar, .irs .irs-from, .irs .irs-to {
            background: #0BAABA;
            border-top: #0BAABA;
            border-bottom:#0BAABA;
            border: #0BAABA;
          }")
  ),
  
  
  # Application title
  titlePanel("Application title"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(width = 4,
      
      helpText(h4("Data and settings", style = "color:white;background-color:#3295b8;padding:2px;font-size:110%;")),
      
      fluidRow(
        
        # Load file
        column(5, W_input_pedigree),
        
        # Load example
        column(4, W_input_example),
        column(3, W_toquickped)
      ),
      
      fluidRow(
        # Allele frequency
        column(3, W_afreq),
        column(2, offset = 3, W_save),
        column(2, W_load),
        column(2, W_reset)
      ),
      
      # Phenotype selection
      helpText(h4("Phenotypes", style = "color:white;background-color:#3295b8;padding:2px;font-size:110%;")),
      fluidRow(id = "pheno0",
               column(6, W_pheno_name),
               column(2, offset = 2, W_pheno_add),
               column(2, W_pheno_rmv)
      ),
      
      # Risk factor selection
      helpText(h4("Risk factors", style = "color:white;background-color:#3295b8;padding:2px;font-size:110%;")),
      fluidRow(id = "factor0",
               column(6, W_factor_name),
               column(2, offset = 2, W_factor_add),
               column(2, W_factor_rmv)
      )
    ),
    
    
    # Main panel
    mainPanel(width = 8,
      
      # fluidRow(
      #   column(7, tabsetPanel(type = "tabs",
      #                         tabPanel("Pedigree data", rHandsontableOutput("pedTable", height = "350px")),
      #                         selected = "Pedigree data")),
      #   column(5, tabsetPanel(type = "tabs",
      #                         tabPanel("Segregation plot", plotOutput("pedPlot", height = "350px")),
      #                         tabPanel("Plot settings"),
      #                         selected = "Segregation plot")
      #   )
      # ),
      
      fluidRow(
        column(7, rHandsontableOutput("pedTable", height = "350px")),
        column(5, plotOutput("pedPlot", height = "350px"))
      ),
      
      fluidRow(
        column(7,
               tabsetPanel(type = "tabs",
                           # tabPanel("Hazards", plotOutput("hazardPlot"), height = "350px"),
                           tabPanel("Cumulative risks", plotOutput("CRPlot", height = "350px")),
                           # tabPanel("Survival penetrance", plotOutput("SPPlot"), height = "350px"),
                           tabPanel("Assistant",
                                    column(5,
                                           inputPanel(
                                             W_fhelp_dist,
                                             div(style = "margin-top:-10px;margin-bottom:20px;margin-right:20px;",
                                                 verbatimTextOutput("fhelp_text")),
                                                      fluidRow(id = "fhelp1", class = 'littlei',
                                                               column(6, W_fhelp1_p),
                                                               column(5, W_fhelp1_q)),
                                                      fluidRow(id = "fhelp2",
                                                               column(6, style = "margin-top:-10px", W_fhelp2_p),
                                                               column(5, style = "margin-top:-10px", W_fhelp2_q)),
                                                      fluidRow(id = "fhelp3",
                                                               column(6, style = "margin-top:-10px", W_fhelp3_p),
                                                               column(5, style = "margin-top:-10px", W_fhelp3_q)),
                                             fluidRow(column(4, offset = 2, W_fhelp_add),
                                                      column(3, W_fhelp_rmv)
                                             )
                                           )
                                    ),
                                    column(7,
                                           plotOutput(outputId = "fhelp_plot", height = "350px")),
                                    
                           ),
                           selected = "Assistant")),
        column(5, tabsetPanel(type = "tabs",
                              tabPanel("FLB",
                                       verbatimTextOutput("flb_main"),
                                       plotOutput(outputId = "flb_colorbar", height = "60px")),
                              tabPanel("FLB = f(x1,x2)",
                                       sidebarPanel(width = 12,
                                                    fluidRow(column(6, W_flb_v1),
                                                             column(6, W_flb_s1)),
                                                    fluidRow(column(6, W_flb_v2),
                                                             column(6, W_flb_s2)),
                                                    W_flb_run
                                       )
                              ),
                              tabPanel("Plot results", plotOutput(outputId = "testplot", height = "350px")),
                              selected = "FLB"))
      )
    )
  )
)



# Server ------------------------------------------------------------------

# Define server logic
server = function(input, output, session) {
  
  
  # Reactive values object
  values = reactiveValues()
  values[["fhelp_total"]] = 3
  values[["pheno_total"]] = 0
  values[["factor_total"]] = 0
  # values[["pheno_vector"]] = character()
  # values[["factor_vector"]] = character()
  values[["flb_v"]] = c("afreq")
  
  
  
  # Reset session
  observeEvent(input$reset, session$reload()) 
  
  
  
  # Store phenorisk selection (rough)
  observeEvent(priority = 2,
    c(input$factor1_pheno, input$factor2_pheno, input$factor3_pheno), {
      message("Store phenorisk selection (rough)")
      values[["factor1_pheno"]] = input$factor1_pheno
      values[["factor2_pheno"]] = input$factor2_pheno
      values[["factor3_pheno"]] = input$factor3_pheno
    })
  
  
  
  # Update phenotype selection
  observeEvent(priority = 2, ignoreNULL = FALSE, values[["pheno_vector"]], {
    req(values[["peddata"]])
    message("Update phenotype selection")
    
    temp = factor(values[["peddata"]][["phenotype"]], levels = c("", "nonaff", values[["pheno_vector"]]))
    temp[is.na(temp)] = ""
    values[["peddata"]]["phenotype"] = temp
  })
  
  
  
  # Factor combinations
  observe(priority = 2, {
    req(values[["factor_total"]] > 0)
    message("Factor combinations")
    values[["rf_combs"]] = expand.grid(replicate(values[["factor_total"]], c(FALSE,TRUE), simplify = FALSE))
    if(values[["pheno_total"]]>0)
      values[["phenomat"]] = 1*sapply(1:values[["factor_total"]], function(i) {
        values[["pheno_vector"]] %in% input[[paste0("factor", i, "_pheno")]]
      })
  })
  
  
  
  # Only one proband
  observeEvent(priority = 2, values[["peddata"]][["proband"]], {
    message("Only one proband")
    currentproband = values[["peddata"]][["proband"]]
    if(!is.null(values[["lastproband"]]) & sum(currentproband)>1)
      newproband = values[["lastproband"]] != currentproband
    else
      newproband = currentproband
    values[["lastproband"]] = newproband
    values[["peddata"]][["proband"]] = newproband
  })
  
  
  
  # Load pedigree from file
  observeEvent(input$input_pedigree, {
    message("Load pedigree from file")
    temp = data.frame(
      readRDS(input$input_pedigree$datapath),
      phenotype = factor("nonaff", levels = c("", "nonaff", values[["pheno_vector"]])),
      carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
      proband = FALSE,
      age = 50)
    
    # Attach factors
    if(!is.null(values[["factor_vector"]])) # not needed...
      temp[, values[["factor_vector"]]] = FALSE
    
    values[["peddata"]] = temp
  })
  
  
  
  # Example pedigree (+ factors)
  observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$input_example, {
    
    req(input$input_example != "")
    message("Example pedigree (+ factors)")
    
    switch(input$input_example,
           
           "Trio" = {
             temp = data.frame(
               nuclearPed(),
               phenotype = factor("nonaff", levels = c("", "nonaff", values[["pheno_vector"]])),
               carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
               proband = FALSE,
               age = c(40, 40, 10))
           },
           
           "Full siblings" = {
             temp = data.frame(
               nuclearPed(nch = 2),
               phenotype = factor("nonaff", levels = c("", "nonaff", values[["pheno_vector"]])),
               carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
               proband = FALSE,
               age = c(40, 40, 10, 10))
           },
           
           "Example 1" = {
             # temp = data.frame(
             #   nuclearPed(nch = 2),
             #   phenotype = factor(c("", "pheno1", "nonaff", "pheno1"), levels = c("", "nonaff", values[["pheno_vector"]])),
             #   carrier = factor(c("no", "het", "no", "het"), levels = c("", "no", "het", "hom")),
             #   proband = c(FALSE, FALSE, FALSE, TRUE),
             #   age = c(40, 40, 10, 10))
             temp = data.frame(
               nuclearPed(nch = 2),
               phenotype = factor("nonaff", levels = c("", "nonaff", values[["pheno_vector"]])),
               carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
               proband = FALSE,
               age = c(40, 40, 10, 10))
           },
           
           "Example 2" = {
             # temp = data.frame(
             #   nuclearPed(nch = 2),
             #   phenotype = factor(c("", "pheno1", "nonaff", "pheno1"), levels = c("", "nonaff", values[["pheno_vector"]])),
             #   carrier = factor(c("no", "het", "no", "het"), levels = c("", "no", "het", "hom")),
             #   proband = c(FALSE, FALSE, FALSE, TRUE),
             #   age = c(40, 40, 10, 10))
             temp = data.frame(
               nuclearPed(nch = 2),
               phenotype = factor("nonaff", levels = c("", "nonaff", values[["pheno_vector"]])),
               carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
               proband = FALSE,
               age = c(40, 40, 10, 10))
           }
    )
    
    # Attach factors
    if(!is.null(values[["factor_vector"]])) # not needed...
      temp[, values[["factor_vector"]]] = FALSE
    
    values[["peddata"]] = temp
    
    updateSelectInput(inputId = "input_example", selected = "")
  })
  
  
  
  # Pedigree table
  output$pedTable = renderRHandsontable({
    req(values[["peddata"]])
    rhandsontable(values[["peddata"]],
                  useTypes = TRUE,
                  manualColumnResize = TRUE,
                  rowHeaders = NULL,
                  height = 300) %>%
      hot_col(col = 'age', format = "0") %>% 
      hot_validate_numeric(col = 'age', min = 1, max = 100, allowInvalid = FALSE) %>%
      hot_table(highlightRow = TRUE) %>%
      # hot_col("phenotype", allowInvalid = TRUE) %>%
      hot_col(1:4, renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                              Handsontable.renderers.NumericRenderer.apply(this, arguments);
                              td.style.background = '#FCFCFC';
                              cellProperties.readOnly = true;}") %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  
  
  # Update data from table edits (this runs twice...)
  observe(priority = 1, {
    req(!is.null(input$pedTable))
    message("Update data from table edits (this runs twice")
    
    temp_full = hot_to_r(input$pedTable)

    temp =
      within(
        temp_full[,1:8], {
          if(!is.null(values[["factor_vector"]])){ # values[["factor_vector"]] != ''
            for(i in rev(values[["factor_vector"]])){
              if(!is.null(temp_full[[i]]))
                assign(i, temp_full[[i]])
              else
                assign(i, FALSE)
            }
            rm(i)
          }
        }
      )
    # temp = temp_full
    
    # print(runif(1))
    values[["peddata"]] = temp
    
  })
  
  
  
  # # Update data from table edits (this runs twice...)
  # observe({
  #   req(!is.null(input$pedTable))
  #   
  #   values[["peddata"]] =
  #     within(
  #       hot_to_r(input$pedTable)[,1:8], {
  #         if(!is.null(values[["factor_vector"]])){ # values[["factor_vector"]] != ''
  #           for(i in rev(values[["factor_vector"]])){
  #             if(!is.null(hot_to_r(input$pedTable)[[i]]))
  #               assign(i, hot_to_r(input$pedTable)[[i]])
  #             else
  #               assign(i, FALSE)
  #           }
  #           rm(i)
  #         }
  #       }
  #     )
  # })
  
  
  
  # # Update data from table edits (this runs twice...)
  # observe(priority = -1000, {
  #   req(!is.null(input$pedTable))
  #   
  #   print('oh')
  #   
  #   temp = hot_to_r(input$pedTable)[,1:8]
  #   
  #   # within(
  #   #   temp, {
  #   #     if(!is.null(values[["factor_vector"]])){ # values[["factor_vector"]] != ''
  #   #       for(i in rev(values[["factor_vector"]])){
  #   #         if(!is.null(hot_to_r(input$pedTable)[[i]]))
  #   #           assign(i, hot_to_r(input$pedTable)[[i]])
  #   #         else
  #   #           assign(i, FALSE)
  #   #       }
  #   #       rm(i)
  #   #     }
  #   #   }
  #   # )
  #   
  #   values[["peddata"]] = temp
  # 
  # })
  
  
  
  # Update FLB indexes
  observe({
    message("Update FLB indexes")
    values[["affected"]] = which(values[["peddata"]][["phenotype"]] != "" & values[["peddata"]][["phenotype"]] != "nonaff")
    values[["unknown"]] = which(values[["peddata"]][["phenotype"]] == "")
    values[["proband"]] = which(values[["peddata"]][["proband"]] == 1)
    values[["carriers"]] = which(values[["peddata"]][["carrier"]] == "het")
    values[["homozygous"]] = which(values[["peddata"]][["carrier"]] == "hom")
    values[["noncarriers"]] = which(values[["peddata"]][["carrier"]] == "no")
  })
  
  
  
  # Segregation plot
  output$pedPlot = renderPlot({
    req(values[["peddata"]])
    plotSegregation(as.ped(values[["peddata"]][, c("id", "fid", "mid", "sex")]),
                    affected = values[["affected"]],
                    unknown = values[["unknown"]],
                    proband = values[["proband"]],
                    if(length(values[["carriers"]] > 0)) carriers = values[["carriers"]],
                    if(length(values[["homozygous"]] > 0)) homozygous = values[["homozygous"]],
                    if(length(values[["noncarriers"]] > 0)) noncarriers = values[["noncarriers"]]
    )
  })
  
  
  
  # Update penetrances
  observe({
    
    req(values[["pheno_total"]]>0)
    
    # Validate that all neccesary UI inputs are created
    validate(need(input[[paste0("pheno", values[["pheno_total"]], "_f2b")]], ""))
    if(values[["factor_total"]] > 0)
      validate(need(input[[paste0("factor", values[["factor_total"]], "_risk")]], ""))
    
    message("Update penetrances")
    
    # Baseline hazards
    h_0 = sapply(seq(values[["pheno_total"]]), function(i){
      get_hazards(x, distr = input[[paste0("pheno", i, "_dist")]],
                  params = c(input[[paste0("pheno", i, "_f0a")]], input[[paste0("pheno", i, "_f0b")]]))
    })
    colnames(h_0) = values[["pheno_vector"]]
    h_2 = sapply(seq(values[["pheno_total"]]), function(i){
      get_hazards(x, distr = input[[paste0("pheno", i, "_dist")]],
                  params = c(input[[paste0("pheno", i, "_f2a")]], input[[paste0("pheno", i, "_f2b")]]))
    })
    colnames(h_2) = values[["pheno_vector"]]
    h_1 = sapply(seq(values[["pheno_total"]]), function(i){
      switch(input[[paste0("pheno", i, "_f1fx")]],
             "f0" = h_0[,i] * exp(input[[paste0("pheno", i, "_f1v")]]),
             "f2" = h_2[,i] * exp(input[[paste0("pheno", i, "_f1v")]])
      )
    })
    colnames(h_1) = values[["pheno_vector"]]

    if(values[["factor_total"]] > 0) {
      # Calculate risk factors
      risks_change = exp(sapply(paste0("factor", 1:values[["factor_total"]], "_risk"), function(x) input[[x]]))
      riskmat = log(t(risks_change*t(values[["rf_combs"]]) + rep(1,values[["factor_total"]])*t(!values[["rf_combs"]])))
      final = exp(riskmat %*% t(matrix(values[["phenomat"]], ncol = values[["factor_total"]])))
      # Update hazards
      values[["h_0"]] = lapply(1:nrow(values[["rf_combs"]]), function(i) final[i,] * h_0)
      values[["h_1"]] = lapply(1:nrow(values[["rf_combs"]]), function(i) final[i,] * h_1)
      values[["h_2"]] = lapply(1:nrow(values[["rf_combs"]]), function(i) final[i,] * h_2)
    }
    else {
      values[["h_0"]] = list(h_0)
      values[["h_1"]] = list(h_1)
      values[["h_2"]] = list(h_2)
    }
    
    # Penetrance values
    values[["f_0_all"]] = surv_penetrance(values[["h_0"]])
    values[["f_1_all"]] = surv_penetrance(values[["h_1"]])
    values[["f_2_all"]] = surv_penetrance(values[["h_2"]])
    
    # Penetrance values
    f = lapply(1:length(values[["h_0"]]), function(i) {
      cbind(reshape2::melt(values[["f_0_all"]][["SP"]][[i]], varnames = c("age", "phenotype"), value.name = "f0"),
            f1 = as.vector(values[["f_1_all"]][["SP"]][[i]]),
            f2 = as.vector(values[["f_2_all"]][["SP"]][[i]]))
    })
    values[["f"]] = dplyr::bind_rows(f, .id = 'comb')
    values[["f_idx"]] = array(1:nrow(values[["f"]]),
                              dim = c(length(x), values[["pheno_total"]]+1, 2^values[["factor_total"]]),
                              dimnames = list(x, colnames(values[["f_0_all"]][["SP"]][[1]]), seq(2^values[["factor_total"]])))
  })
  
  
  
  # Update liability class groups (problem with double run)
  observe(priority = -1, {
    req(values[["peddata"]])
    
    message("Update liability class groups (problem with double run)")

    if(values[["factor_total"]] > 0){
      # validate(need(values[["peddata"]][,values[["factor_vector"]]], ""))
      lclass_idx = apply(as.matrix(values[["peddata"]][,values[["factor_vector"]]]), 1,
                         function(i) which(apply(values[["rf_combs"]], 1, function(j) all(j == i))))
    }
    else
      lclass_idx = rep(1, nrow(values[["peddata"]]))
    
    # Set missing phenotype to nonaff...
    new_j = as.character(values[["peddata"]][["phenotype"]])
    new_j[new_j == ""] = "nonaff"
    
    # Set liability classes
    values[["lclass"]] = mapply(function(i,j,l) values[["f_idx"]][i,j,l],
                                i = values[["peddata"]][["age"]],
                                j = new_j,
                                l = lclass_idx)
  })
  
  
  
  # Penetrance plots
  # output$hazardPlot = renderPlot({
  #   req(values[["h_0"]], values[["h_1"]], values[["pheno_total"]]>0)
  #     dat = cbind(
  #       reshape2::melt(values[["h_0"]][[1]], varnames = c("age", "phenotype"), value.name = "f0"),
  #       f1 = as.vector(values[["h_1"]][[1]]),
  #       f2 = as.vector(values[["h_1"]][[1]])
  #     )
  #     dat$phenotype = factor(dat$phenotype, levels = values[["pheno_vector"]])
  #     ggplot(dat) +
  #       geom_line(aes(x = age, y = f0, color = "f0", group = 1)) +
  #       geom_line(aes(x = age, y = f1, color = "f1", group = 1)) +
  #       facet_wrap(~ phenotype, scales = 'free_y')
  # })
  output$CRPlot = renderPlot({
    req(values[["f_0_all"]], values[["f_1_all"]], values[["f_2_all"]], values[["pheno_total"]]>0)
    dat = cbind(
      reshape2::melt(values[["f_0_all"]][["CR"]][[1]], varnames = c("age", "phenotype"), value.name = "f0"),
      f1 = as.vector(values[["f_1_all"]][["CR"]][[1]]),
      f2 = as.vector(values[["f_2_all"]][["CR"]][[1]])
    )
    dat$phenotype = gsub('nonaff', 'total', dat$phenotype)
    dat$phenotype = factor(dat$phenotype, levels = c(values[["pheno_vector"]], 'total'))
    ggplot(dat) +
      geom_line(aes(x = age, y = f0, color = "f0", group = 1)) +
      geom_line(aes(x = age, y = f1, color = "f1", group = 1)) +
      geom_line(aes(x = age, y = f2, color = "f2", group = 1)) +
      facet_wrap(~ phenotype, scales = 'free_y') +
      theme_classic()
  })
  # output$SPPlot = renderPlot({
  #   req(values[["SP_0"]], values[["SP_1"]], values[["pheno_total"]]>0)
  #     dat = cbind(
  #       reshape2::melt(values[["SP_0"]][[1]], varnames = c("age", "phenotype"), value.name = "f0"),
  #       f1 = as.vector(values[["SP_1"]][[1]]),
  #       f2 = as.vector(values[["SP_1"]][[1]])
  #     )
  #     dat$phenotype = factor(dat$phenotype, levels = c(values[["pheno_vector"]], 'nonaff'))
  #     ggplot(dat) +
  #       geom_line(aes(x = age, y = f0, color = "f0", group = 1)) +
  #       geom_line(aes(x = age, y = f1, color = "f1", group = 1)) +
  #       facet_wrap(~ phenotype, scales = 'free_y')
  # })
  
  
  
  # FLB calculation
  observe(priority = -2, {
    req(values[["peddata"]])
    
    message("FLB calculation")
    
    # Calculate BF
    values[["flb"]] = tryCatch(
      FLB(x = as.ped(values[["peddata"]][, c("id", "fid", "mid", "sex")]),
          affected = values[["affected"]],
          unknown = values[["unknown"]],
          proband = values[["proband"]],
          if(length(values[["carriers"]] > 0)) carriers = values[["carriers"]],
          if(length(values[["homozygous"]] > 0)) homozygous = values[["homozygous"]],
          if(length(values[["noncarriers"]] > 0)) noncarriers = values[["noncarriers"]],
          freq = 10^input$afreq,
          penetrances = values[["f"]][,c("f0", "f1", "f2")],
          liability = values[["lclass"]],
          details = FALSE),
      error = function(err) NULL)
  })
  
  
  # FLB calculation
  output$flb_main = renderText({
    
    req(values[["flb"]])
    
    values[["flb"]]
    
  })
  
  

  output$testplot = renderPlot({

    req(values[["flb_vals"]])

    # ggplot() +
    #   geom_contour_filled(aes(x = values[["grid"]][,1], y = values[["grid"]][,2], z = values[["flb_vals"]]))
    contourplot(values[["grid"]], round(values[["flb_vals"]], 7), values[["flb"]])

  })
  
  observeEvent(input$flb_run, {
    
    req(values[["pheno_total"]]>0, values[["peddata"]], input$flb_v1 != input$flb_v2)
    
    # Create grid of values
    values[["grid"]] = setNames(
      expand.grid(seq(input$flb_s1[1], input$flb_s1[2], l = 10),
                  seq(input$flb_s2[1], input$flb_s2[2], l = 10)),
      c(names(values[["flb_choices"]])[which(values[["flb_v"]] == input$flb_v1)],
        names(values[["flb_choices"]])[which(values[["flb_v"]] == input$flb_v2)])
    )
    
    
    # Collect inputs
    fullgrid = sapply(c("afreq",
                        if(values[["pheno_total"]] > 0) paste0("pheno", as.vector(t(outer(seq(values[["pheno_total"]]), c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v"), paste0)))),
                        if(values[["factor_total"]] > 0) paste0("factor", seq(values[["factor_total"]]), "_risk")),
                      function(x) input[[x]], simplify = FALSE, USE.NAMES = TRUE)
    fullgrid[[input$flb_v1]] = values[["grid"]][, 1]
    fullgrid[[input$flb_v2]] = values[["grid"]][, 2]
    fullgrid = as.data.frame(fullgrid)
    
  
    # Calculate FLB
    
    values[["flb_vals"]] = sapply(seq(nrow(fullgrid)), function(gridrow) {
      
      # Baseline hazards
      h_0 = sapply(seq(values[["pheno_total"]]), function(i){
        get_hazards(x, distr = input[[paste0("pheno", i, "_dist")]],
                    params = c(fullgrid[gridrow, paste0("pheno", i, "_f0a")], fullgrid[gridrow, paste0("pheno", i, "_f0b")]))
      })
      colnames(h_0) = values[["pheno_vector"]]
      h_2 = sapply(seq(values[["pheno_total"]]), function(i){
        get_hazards(x, distr = input[[paste0("pheno", i, "_dist")]],
                    params = c(fullgrid[gridrow, paste0("pheno", i, "_f2a")], fullgrid[gridrow, paste0("pheno", i, "_f2b")]))
      })
      colnames(h_2) = values[["pheno_vector"]]
      h_1 = sapply(seq(values[["pheno_total"]]), function(i){
        switch(input[[paste0("pheno", i, "_f1fx")]],
               "f0" = h_0[,i] * exp(fullgrid[gridrow, paste0("pheno", i, "_f1v")]),
               "f2" = h_2[,i] * exp(fullgrid[gridrow, paste0("pheno", i, "_f1v")])
        )
      })
      colnames(h_1) = values[["pheno_vector"]]
      
      
      if(values[["factor_total"]] > 0) {
        # Calculate risk factors
        risks_change = exp(fullgrid[gridrow, paste0("factor", 1:values[["factor_total"]], "_risk")])
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

      
      
      # Calculate BF
      tryCatch(
        FLB(x = as.ped(values[["peddata"]][, c("id", "fid", "mid", "sex")]),
            affected = values[["affected"]],
            unknown = values[["unknown"]],
            proband = values[["proband"]],
            if(length(values[["carriers"]] > 0)) carriers = values[["carriers"]],
            if(length(values[["homozygous"]] > 0)) homozygous = values[["homozygous"]],
            if(length(values[["noncarriers"]] > 0)) noncarriers = values[["noncarriers"]],
            freq = 10^fullgrid[gridrow, "afreq"],
            penetrances = f[,c("f0", "f1", "f2")],
            liability = values[["lclass"]],
            details = FALSE),
        error = function(err) NULL)
      
    })
    
  })
  
  
  observeEvent(input$fhelp_add, {
    req(values[["fhelp_total"]]<9)
    
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
  })
  
  
  observeEvent(input$fhelp_rmv, {
    req(values[["fhelp_total"]]>3)
    id = c(paste0("fhelp", values[["fhelp_total"]], "_p"), paste0("fhelp", values[["fhelp_total"]], "_q"))
    removeUI(
      selector = paste0("div:has(> #", id, ")"),
      multiple = TRUE
    )
    values[["fhelp_total"]] = values[["fhelp_total"]] - 1
  })
  
  
  
  # Add/remove phenotypes
  
  observeEvent(input$pheno_add, {
    req(values[["pheno_total"]] < 9,
        !grepl("^\\s*$", input$pheno_name),
        !input$pheno_name %in% c(values[["pheno_vector"]], "nonaff"))
    
    new_id = paste0("pheno", values[["pheno_total"]] + 1)
    
    insertUI(
      selector = paste0("#pheno", values[["pheno_total"]]),
      where = "afterEnd",
      ui = 
        div(
          fluidRow(
            column(3, p(HTML(paste0("&nbsp;", input$pheno_name)), style = "color:#4B4B4B;background-color:#FFEC98;")),
            column(4, tags$div(class = "inline", selectInput(inputId = paste0(new_id, "_dist"), label = "hazards",
                                                             choices = c("Weibull", "Log-logist"), selected = "Weibull"))),
            column(3, tags$div(class = "inline", numericInput(inputId = paste0(new_id, "_f1v"), label = HTML("f<sub>1</sub>=exp"),
                                                              min = -3, max = +3, step = 0.1, value = 0))),
            column(2, tags$div(class = "inline", selectInput(inputId = paste0(new_id, "_f1fx"), label = "x",
                                                             choices = c("f0", "f2"), selected = "f2")))
            # column(5, tags$div(class = "inline", selectInput(inputId = paste0(new_id, "_f1fx"), label = "f1 = exp(lambda) x",
            #                                                  choices = c("f0", "f2"), selected = "f2")))
          ),
          fluidRow(id = new_id,
                   style = "margin-bottom:15px;",
                   column(3, numericInput(inputId = paste0(new_id, "_f0a"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> shape") else NULL,
                                          min = 1, max = 5, step = 0.1, value = 1)),
                   column(3, numericInput(inputId = paste0(new_id, "_f0b"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>0</sub> scale") else NULL,
                                          min = 1, max = 500, step = 5, value = 50)),
                   column(3, numericInput(inputId = paste0(new_id, "_f2a"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> shape") else NULL,
                                          min = 1, max = 5, step = 0.1, value = 1)),
                   column(3, numericInput(inputId = paste0(new_id, "_f2b"), label = if(values[["pheno_total"]] == 0) HTML("f<sub>2</sub> scale") else NULL,
                                          min = 1, max = 500, step = 5, value = 50))
                   # column(2, numericInput(inputId = paste0(new_id, "_f1v"), label = if(values[["pheno_total"]] == 0) HTML("&lambda;") else NULL,
                   #                        min = -3, max = +3, step = 0.1, value = 0))
          )
        )
    )
    
    values[["pheno_vector"]] = c(values[["pheno_vector"]], input$pheno_name)
    updateTextInput(inputId = "pheno_name", value = "")
    
    
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
  })
  
  
  observeEvent(input$pheno_rmv, {
    req(values[["pheno_total"]] > 0)
    
    to_remove = paste0("pheno", values[["pheno_total"]])
    
    removeUI(
      selector = paste0("div:has(> #", to_remove, ")"),
      multiple = TRUE
    )
    
    
    # Update sensitivity analysis choices
    values[["flb_v"]] = setdiff(values[["flb_v"]], paste0("pheno", values[["pheno_total"]], c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v")))
    
    
    length(values[["pheno_vector"]]) = values[["pheno_total"]] - 1
    
    # Update risk factor phenotype choices
    if(values[["factor_total"]]> 0)
      lapply(seq(values[["factor_total"]]), function(x)
        updateSelectizeInput(
          inputId = paste0("factor", x, "_pheno"),
          choices = values[["pheno_vector"]],
          selected = values[[paste0("factor", x, "_pheno")]])
      )
    
    values[["pheno_total"]] = values[["pheno_total"]] - 1
  })
  
  
  
  # Add/remove factors
  
  observeEvent(input$factor_add, {
    req(values[["factor_total"]] < 3,
        !grepl("^\\s*$", input$factor_name),
        !input$factor_name %in% c(values[["factor_vector"]], "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age"))
    
    new_id = paste0("factor", values[["factor_total"]] + 1)
    
    insertUI(
      selector = paste0("#factor", values[["factor_total"]]),
      where = "afterEnd",
      ui = 
        div(fluidRow(id = new_id,
                     column(3, style = ifelse(values[["factor_total"]] == 0, "margin-top: 30px;", "margin-top: 5px;"), p(HTML(paste0("&nbsp;", input$factor_name)), style = "color:#4B4B4B;background-color:#FFCF80;")),
                     column(6, selectizeInput(inputId = paste0(new_id, "_pheno"),
                                              label = if(values[["factor_total"]] == 0) "phenotypes" else NULL,
                                              choices = values[["pheno_vector"]],
                                              multiple = TRUE)),
                     column(3, numericInput(inputId = paste0(new_id, "_risk"), label = if(values[["factor_total"]] == 0) "log(risk)" else NULL,
                                            min = -3, max = 3, step = 0.1, value = 0))
        ))
    )
    
    values[["factor_vector"]] = c(values[["factor_vector"]], input$factor_name)
    updateTextInput(inputId = "factor_name", value = "")
    
    
    # Update sensitivity analysis choices
    values[["flb_v"]] = c(values[["flb_v"]], paste0(new_id, "_risk"))
    
    
    values[["factor_total"]] = values[["factor_total"]] + 1
  })
  
  
  observeEvent(input$factor_rmv, {
    req(values[["factor_total"]] > 0)
    
    to_remove = paste0("factor", values[["factor_total"]])
    
    removeUI(
      selector = paste0("div:has(> #", to_remove, ")"),
      multiple = TRUE
    )
    
    # Update sensitivity analysis choices
    values[["flb_v"]] = setdiff(values[["flb_v"]], paste0("factor", values[["factor_total"]], "_risk"))
    
    length(values[["factor_vector"]]) = values[["factor_total"]] - 1
    
    values[["factor_total"]] = values[["factor_total"]] - 1
  })
  
  
  
  # Store sensitivity analysis choices
  observe({
    values[["flb_v1"]] = input$flb_v1
    values[["flb_v2"]] = input$flb_v2
  })
  
  # Update sensitivity analysis choices
  observeEvent(values[["flb_v"]], {
    sortedlist = factor(values[["flb_v"]],
                        levels = c("afreq",
                                   paste0("pheno", as.vector(t(outer(seq(values[["pheno_total"]]), c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v"), paste0)))),
                                   paste0("factor", seq(values[["factor_total"]]), "_risk")))
    sortedlist = sort(sortedlist)
    names(sortedlist) = c("log10(afreq)",
                          if(values[["pheno_total"]] > 0) as.vector(t(outer(values[["pheno_vector"]], c("f0 shape", "f0 scale", "f2 shape", "f2 scale", "f1 coef"), paste))),
                          if(values[["factor_total"]] > 0) paste(values[["factor_vector"]], "log(risk)"))
    updateSelectInput(inputId = "flb_v1", choices = sortedlist, selected = values[["flb_v1"]])
    updateSelectInput(inputId = "flb_v2", choices = sortedlist, selected = values[["flb_v2"]])
    values[["flb_choices"]] = sortedlist
  })
  
  # Update sliders
  observeEvent(input$flb_v1, {
    slidervals = switch(gsub(".*_", "", input$flb_v1),
                        "afreq" = c(-5, -1),
                        "f0a" = c(1, 5),
                        "f2a" = c(1, 5),
                        "f0b" = c(5, 500),
                        "f2b" = c(5, 500),
                        "f1v" = c(-3, +3),
                        "risk" = c(-3, +3))
    updateSliderInput(inputId = "flb_s1", min = slidervals[1], max = slidervals[2], value = slidervals)
  })
  observeEvent(input$flb_v2, {
    slidervals = switch(gsub(".*_", "", input$flb_v2),
                        "afreq" = c(-5, -1),
                        "f0a" = c(1, 5),
                        "f2a" = c(1, 5),
                        "f0b" = c(5, 500),
                        "f2b" = c(5, 500),
                        "f1v" = c(-3, +3),
                        "risk" = c(-3, +3))
    updateSliderInput(inputId = "flb_s2", min = slidervals[1], max = slidervals[2], value = slidervals)
  }) 
  
  
  # F helper values
  observe({
    validate(
      need(input[[paste0("fhelp", values[["fhelp_total"]], "_p")]], ""),
      need(input[[paste0("fhelp", values[["fhelp_total"]], "_q")]], "")
      )
    values[["fhelp_p"]] = sapply(paste0("fhelp", seq(values[["fhelp_total"]]), "_p"), function(x) input[[x]])
    values[["fhelp_q"]] = sapply(paste0("fhelp", seq(values[["fhelp_total"]]), "_q"), function(x) input[[x]])
    
    values[["fhelp_sol"]] = switch(input$fhelp_dist,
                                   "Weibull" = optim(par = c(0, 1), function(params) {
                                     summand = suppressWarnings(pweibull(q = values[["fhelp_q"]], shape = params[1], scale = params[2]) - values[["fhelp_p"]])
                                     sum(summand^2)},
                                     method = "L-BFGS-B", lower = c(0.001, 0.001), upper = c(10000, 10000)),
                                   "Log-logist" = optim(par = c(0, 1), function(params) {
                                     summand = suppressWarnings(pllogis(q = values[["fhelp_q"]], shape = params[1], scale = params[2]) - values[["fhelp_p"]])
                                     sum(summand^2)},
                                     method = "L-BFGS-B", lower = c(0.001, 0.001), upper = c(10000, 10000)))
      
  })
  
  output$fhelp_plot = renderPlot({
    req(values[["fhelp_sol"]])
    
    fit = switch(input$fhelp_dist,
                 "Weibull" = pweibull(x, shape = values[["fhelp_sol"]]$par[1], scale = values[["fhelp_sol"]]$par[2]),
                 "Log-logist" = pllogis(x, shape = values[["fhelp_sol"]]$par[1], scale = values[["fhelp_sol"]]$par[2]))
    ggplot(mapping = aes(x = age, y = prob)) +
      geom_point(data = data.frame(age = values[["fhelp_q"]], prob = values[["fhelp_p"]])) +
      geom_line(data = data.frame(age = x, prob = fit)) +
      scale_y_continuous(limits = c(0,1)) +
      theme_classic()
  })

  output$fhelp_text = renderText({
    paste0("Shape: ", round(values[["fhelp_sol"]]$par[1], 4), "\nScale: ",  round(values[["fhelp_sol"]]$par[2], 4))
  })
  
  
  output$flb_colorbar = renderPlot({
    
    # require(values[["flb"]])

    BFplot(values[["flb"]])
  })
  
  
}



# End ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
