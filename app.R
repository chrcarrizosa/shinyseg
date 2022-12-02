# Libraries
library(shiny, quietly = TRUE)
library(shinydashboard, quietly = TRUE)
library(shinydashboardPlus, quietly = TRUE)
library(shinyjs, quietly = TRUE) # show/hide boxes
# library(tidyverse) # dplyr::bind_rows() is being used!
# library(rlang) # is_empty (also in tidyverse purrr?)
library(rhandsontable, quietly = TRUE)
library(pedsuite, quietly = TRUE)
library(segregatr, quietly = TRUE)
# library(flexsurv, quietly = TRUE)
library(ggplot2, quietly = TRUE)
# library(reactlog)

# Age
x = seq(1:100)

# Notes
#   Add risk factors to penetrance plots?
#   Missing ages?
#   Add phenotype number to riskplots?
#   Add risk factor pop distribution?
#   Separate baseline penetrances calculation
#   When user selects same variable in sensitivity analysis...
#   Clear uploaded pedigree if example
#   Pedigree labels
#   Multiple pedigrees
#   Age groups and custom penetrances
#   FLB = f(x)
#   Notifications
#   Add remove ped


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
    choices = list("", "Clear pedigrees" = c("Trio", "Full siblings"), "Worked examples" = c("Example 1", "Example 2")),
    selected = ""
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
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#4b4b4b;border:1px solid #d4d4d4;background-color:#d9ead3;font-size:80%;margin-left:-10px"
  )

W_pheno_rmv = 
  actionButton(
    inputId = "pheno_rmv",
    label = "Undo",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#4b4b4b;border:1px solid #d4d4d4;background-color:#f4cccc;font-size:80%;margin-left:-10px"
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
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#4b4b4b;border:1px solid #d4d4d4;background-color:#d9ead3;font-size:80%;margin-left:-10px"
  )

W_factor_rmv = 
  actionButton(
    inputId = "factor_rmv",
    label = "Undo",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#4b4b4b;border:1px solid #d4d4d4;background-color:#f4cccc;font-size:80%;margin-left:-10px"
  )

W_fhelp_dist = 
  selectInput(
    inputId = "fhelp_dist",
    label = "Distribution",
    choices = c("Weibull", "Log-logist")
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

# W_flb_calc =
#   actionButton(
#     inputId = "flb_calc",
#     label = "Calculate",
#     style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
#   )

W_flb_run = 
  actionButton(
    inputId = "flb_run",
    label = "Run",
    style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
  )

W_lclass_mode = 
  checkboxInput(
    inputId = "lclass_mode",
    label = "Custom liab classes",
    value = TRUE
  )

W_sexspec_mode =
  checkboxInput(
    inputId = "sexspec_mode",
    label = "Sex-specific parameters",
    value = FALSE
  )

W_xr_mode = 
  checkboxInput(
    inputId = "xr_model",
    label = "XR model",
    value = FALSE
  )

# W_ped_rmv = 
#   actionButton(
#     inputId = "ped_rmv",
#     label = "Remove",
#     style = "padding-top:4px;padding-bottom:4px;margin-top:6px;color:#007BA7;border:1px solid #007BA7;font-size:90%"
#   )

# UI ----------------------------------------------------------------------

# Define UI
ui = dashboardPage(
  
  skin = "black",
  
  # Application title
  dashboardHeader(
    title = "shinyseg"#,
    # dropdownMenu(type = "tasks", badgeStatus = "success",
    #              taskItem(value = 90, color = "green",
    #                       "Documentation"
    #              ),
    #              taskItem(value = 17, color = "aqua",
    #                       "Project X"
    #              ),
    #              taskItem(value = 75, color = "yellow",
    #                       "Server deployment"
    #              ),
    #              taskItem(value = 80, color = "red",
    #                       "Overall project"
    #              )
    # )
  ),
  
  # Sidebar
  dashboardSidebar(disable = TRUE, width = 0, minified = FALSE),
  
  # Main panel
  dashboardBody(
    
    useShinyjs(),
    
    tags$head(
      tags$style(
        HTML("

    .skin-black .main-header .navbar {
      background-color: #ebebeb;
    }
    
    .skin-black .main-header .logo {
      background-color: #fafafa;
      color: #333;
    }

    .skin-black .inline label{
      display: table-cell;
      text-align: left;
      vertical-align: middle;
      font-weight: normal;
      padding-right: 5px;
      padding-bottom: 10px;
    }

    .skin-black .inline .form-group{
      display: table-row;
    }

    .skin-black .inline .selectize-input{
      display: table-row;
    }

    .skin-black .inline .selectize-control.single .selectize-input:after{
      content: none;
    }

    .skin-black .inline .form-control.shiny-bound-input{
      display: table-row;
      height: 20px;
      width: 50px;
      padding: 0px;
      border-radius: 0px;
      border: 0px;
    }

    .skin-black .tabbable > .nav > li > a {
      border:1px solid #DEDEDE;
    }

    .skin-black .tabbable > .nav > li[class=active] > a {
      background-color: #3295b8;
      color:white
    }

    .skin-black .irs .irs-single, .irs .irs-bar-edge, .irs .irs-bar, .irs .irs-from, .irs .irs-to {
      background: #0BAABA;
      border-top: #0BAABA;
      border-bottom:#0BAABA;
      border: #0BAABA;
    }

    .nav-tabs-custom>.nav-tabs>li.header {
      line-height: 35px;
      padding: 0 10px;
      font-size: 19px;
      color: #3665b5;
      font-family: Helvetica;
    }
    
    .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
      font-size: 18px;
      color: #1f6a99;
      font-family: Helvetica;
    }
    
    .box-header.with-border {
      background-color: #fafafa;
    }
    
    .box.box-solid {
      border: 1px solid;
      border-color:#abcacc;
    }
         ")
      )
    ),
    
    fluidRow(
      column(5,
             
             box(width = 12,
                 title = "Data and settings",
                 collapsible = FALSE,
                 solidHeader = TRUE,

                 fluidRow(
                   
                   # Load file
                   column(6, W_input_pedigree),
                   
                   # Load example
                   column(6, W_input_example)
                 ),
                 
                 
                 fluidRow(
                   # Allele frequency
                   column(3, W_afreq),
                   column(3, W_lclass_mode),
                   column(3, W_xr_mode)
                 ),
                 
                 dropdownMenu = boxDropdown(
                   boxDropdownItem("Load session", id = "load", icon = icon("upload")),
                   boxDropdownItem("Save session", id = "save", icon = icon("download")),
                   boxDropdownItem("Reset session", id = "reset", icon = icon("sync")),
                   dropdownDivider(),
                   boxDropdownItem("To QuickPed", href = "https://magnusdv.shinyapps.io/quickped/", icon = icon("network-wired"))
                 )
             ),
             
             # Phenotype selection
             box(width = 12,
                 id = "box_pheno",
                 title = "Phenotypes",
                 collapsible = TRUE,
                 fluidRow(id = "pheno0",
                          column(5, W_pheno_name),
                          column(3, W_sexspec_mode),
                          column(2, offset = 0, W_pheno_add),
                          column(2, W_pheno_rmv)
                 ),
                 actionLink(inputId = "link1", label = "Help"),
                 sidebar = boxSidebar(
                   width = 100,
                   id = "pheno_sidebar",
                   background = "white",
                   column(4, selectInput(inputId = "pheno_dist", label = "hazards model",
                                         choices = c("Weibull", "Log-logist"), selected = "Weibull"))
                 )
             ),
             
             # Risk factor selection
             box(width = 12,
                 id = "box_factor",
                 title = "Risk factors",
                 collapsible = TRUE,
                 # collapsed = TRUE,
                 fluidRow(id = "factor0",
                          column(6, W_factor_name),
                          column(2, offset = 2, W_factor_add),
                          column(2, W_factor_rmv)
                 )
             ),
             
             # Liability classes
             box(width = 12,
                 id = "box_lclass",
                 title = "Liability classes",
                 collapsible = TRUE,
                 # collapsed = TRUE,
                 # fluidRow(id = "lclass0",
                 #          column(6, W_lclass_name),
                 #          column(2, offset = 2, W_lclass_add),
                 #          column(2, W_lclass_rmv)
                 # ),
                 rHandsontableOutput("lclassTable")
             )
      ),
      column(7,
             box(width = 12,
                 title = "Pedigree table",
                 collapsible = TRUE,
                 rHandsontableOutput("pedTable")
             ),
             # box(width = 3,
             #     title = "Info",
             #     collapsible = TRUE,
             #     p(HTML("hello<br><br><br><br><br>")),
             #     label = boxLabel(
             #       text = 1,
             #       status = "danger",
             #       style = "circle"
             #     )
             # ),
             box(width = 6,
                 id = "box_pedplot",
                 title = "Plot",
                 collapsible = TRUE,
                 plotOutput("pedPlot", height = "350px"),
                 tags$table(
                   tags$tr(
                     tags$td(actionLink("ped_prev", NULL, icon("arrow-left"))),
                     tags$td(uiOutput('ped_current')),
                     tags$td(actionLink("ped_next", NULL, icon("arrow-right")))
                   )
                 )
             ),
             box(width = 6,
                 title = "FLB",
                 collapsible = TRUE,
                 verbatimTextOutput("flb_main"),
                 # W_flb_calc,
                 # HTML("<br><br><br><br><br><br>"),
                 plotOutput(outputId = "flb_colorbar", height = "55px"),
                 HTML("<br><br><br>"),
                 # fluidRow(
                   fluidRow(column(6, W_flb_v1), column(6, W_flb_s1)),
                   fluidRow(column(6, W_flb_v2), column(6, W_flb_s2)),
                   W_flb_run
                 # )
             )
             
      )
    )
  )
)



# Server ------------------------------------------------------------------

# Define server logic
server = function(input, output, session) {
  
  
  # Reactive values object
  values = reactiveValues()
  values[["pheno_total"]] = 0
  values[["factor_total"]] = 0
  values[["flb_v"]] = c("afreq")
  values[["lclassdata"]] = data.frame(id = character(1),
                                      f0 = 0.1,
                                      f1 = 0.8,
                                      f2 = 0.8)
  values[["fhelpdata"]] = data.frame(quantile = c(0.25, 0.50, 0.75),
                                     age = as.integer(c(20, 40, 60)))
  values[["ped_total"]] = 0
  values[["ped_current"]] = 0
  
  
  # Reset session
  observeEvent(input$reset, session$reload()) 
  
  
  
  # Update mode (sidebar boxes)
  observeEvent(input$lclass_mode, {
    if(input$lclass_mode) {
      shinyjs::hide("box_pheno")
      shinyjs::hide("box_factor")
      shinyjs::show("box_lclass")
      
    }
    else {
      shinyjs::show("box_pheno")
      shinyjs::show("box_factor")
      shinyjs::hide("box_lclass")
    }
  })
  
  
  
  # Store phenorisk selection (rough)
  observeEvent(priority = 2,
               c(input$factor1_pheno, input$factor2_pheno, input$factor3_pheno), {
                 message("Store phenorisk selection (rough)")
                 values[["factor1_pheno"]] = input$factor1_pheno
                 values[["factor2_pheno"]] = input$factor2_pheno
                 values[["factor3_pheno"]] = input$factor3_pheno
               })
  
  
  
  # Update phenotype selection
  observeEvent(priority = 2, ignoreNULL = FALSE, c(input$lclass_mode, values[["pheno_vector"]]), {
    req(values[["peddata"]])
    message("Update phenotype selection")
    temp = if(input$lclass_mode) factor(values[["peddata"]][["phenotype"]], levels = c("", "nonaff", "aff", values[["pheno_vector"]]),
                                        labels = c("", "nonaff", rep("aff", 1+length(values[["pheno_vector"]]))))
    else factor(values[["peddata"]][["phenotype"]], levels = c("", "nonaff", values[["pheno_vector"]]))
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
    req(any(values[["peddata"]][["proband"]]))
    req(input$input_example == "") # avoids updating twice...
    
    message("Only one proband")
    lapply(1:values[["ped_total"]], function(pedid) {
      idxs = which(values[["peddata"]][["ped"]] == pedid)
      currentproband = values[["peddata"]][["proband"]][idxs]
      if(!is.null(values[["lastproband"]][idxs]) & sum(currentproband)>1)
        newproband = values[["lastproband"]][idxs] != currentproband
      else
        newproband = currentproband
      values[["lastproband"]][idxs] = newproband
      values[["peddata"]][["proband"]][idxs] = newproband
    })
  })
  
  
  
  # Load pedigree from file
  observeEvent(input$input_pedigree, {
    message("Load pedigree from file")
    values[["ped_total"]] = as.integer(values[["ped_total"]] + 1)
    temp = data.frame(
      ped = values[["ped_total"]],
      readRDS(input$input_pedigree$datapath),
      phenotype = if(input$lclass_mode) factor("", levels = c("", "nonaff", "aff"))
      else factor("", levels = c("", "nonaff", values[["pheno_vector"]])),
      carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
      proband = FALSE,
      age = as.integer(50),
      lclass = as.integer(1))
    
    # Attach factors
    if(!is.null(values[["factor_vector"]])) # not needed...
      temp[, values[["factor_vector"]]] = FALSE
    
    values[["peddata"]] = rbind(values[["peddata"]], temp)
    values[["ped_current"]] = values[["ped_total"]]
  })
  
  
  
  # XR model changes
  observeEvent(priority = 2, ignoreInit = TRUE, c(values[["peddata"]][["carrier"]], input$xr_model), {
    req(values[["peddata"]], input$xr_model)
    message("XR model changes")
    # Remove homozygous males
    to_change = which(values[["peddata"]][["carrier"]] == "hom" & values[["peddata"]][["sex"]] == 1)
    newcarrier = values[["peddata"]][["carrier"]]
    newcarrier[to_change] = "het"
    values[["peddata"]][["carrier"]] = newcarrier
  })
  
  
  
  # Example pedigree (+ factors)
  observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$input_example, {
    
    req(input$input_example != "")
    message("Example pedigree (+ factors)")
    
    switch(input$input_example,
           
           "Trio" = {
             values[["ped_total"]] = as.integer(values[["ped_total"]] + 1)
             temp = data.frame(
               ped = values[["ped_total"]],
               nuclearPed(),
               phenotype = if(input$lclass_mode) factor("", levels = c("", "nonaff", "aff"))
               else factor("", levels = c("", "nonaff", values[["pheno_vector"]])),
               carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
               proband = FALSE,
               age = as.integer(c(40, 40, 10)),
               lclass = as.integer(rep(1, 3)))
           },
           
           "Full siblings" = {
             values[["ped_total"]] = as.integer(values[["ped_total"]] + 1)
             temp = data.frame(
               ped = values[["ped_total"]],
               nuclearPed(nch = 2),
               phenotype = if(input$lclass_mode) factor("", levels = c("", "nonaff", "aff"))
               else factor("", levels = c("", "nonaff", values[["pheno_vector"]])),
               carrier = factor(NA_character_, levels = c("", "no", "het", "hom")),
               proband = FALSE,
               age = as.integer(c(40, 40, 10, 10)),
               lclass = as.integer(rep(1, 4)))
           },
           
           "Example 1" = {
             
             if(input$lclass_mode){
               updateCheckboxInput(inputId = "lclass_mode", value = FALSE)
               freezeReactiveValue(input, "lclass_mode")
             }
             if(input$sexspec_mode){
               updateCheckboxInput(inputId = "sexspec_mode", value = FALSE)
               # freezeReactiveValue(input, "sexspec_mode")
             }
             if(input$xr_model) {
               updateCheckboxInput(inputId = "xr_model", value = FALSE)
               # freezeReactiveValue(input, "xr_model")
             }
             
             # Update UI
             if(values[["factor_total"]]>0) rmv_factor(values, all = TRUE)
             if(values[["pheno_total"]]>0) rmv_pheno(values, all = TRUE)
             add_pheno(values, "pheno1", params = c(0, 1, 200, 1, 50), sexspec = FALSE)

             values[["ped_total"]] = as.integer(1)
             temp = data.frame(
               ped = values[["ped_total"]],
               nuclearPed(nch = 2),
               phenotype = factor(c("", "pheno1", "nonaff", "pheno1"), levels = c("", "nonaff", values[["pheno_vector"]])),
               carrier = factor(c("no", "het", "no", "het"), levels = c("", "no", "het", "hom")),
               proband = c(FALSE, FALSE, FALSE, TRUE),
               age = as.integer(c(40, 40, 10, 10)),
               lclass = as.integer(rep(1, 4)))
           },
           
           "Example 2" = {
             if(!input$lclass_mode){
               updateCheckboxInput(inputId = "lclass_mode", value = TRUE)
               # freezeReactiveValue(input, "lclass_mode")
             }
             if(input$sexspec_mode){
               updateCheckboxInput(inputId = "sexspec_mode", value = FALSE)
               # freezeReactiveValue(input, "sexspec_mode")
             }
             if(!input$xr_model) {
               updateCheckboxInput(inputId = "xr_model", value = TRUE)
               # freezeReactiveValue(input, "xr_model")
             }
             
             # Update UI
             # if(values[["factor_total"]]>0) rmv_factor(values, all = TRUE)
             # if(values[["pheno_total"]]>0) rmv_pheno(values, all = TRUE)
             values[["lclassdata"]] = data.frame(id = as.character(1:2),
                                                 f0 = c(0.001, 0.001),
                                                 f1 = c(0.999, 0.001),
                                                 f2 = c(NA, 0.999))

             x = nuclearPed(3, sex = c(1, 1, 2))
             x = addChildren(x, mo = 5, sex = 1:2, verbose = FALSE)
             values[["ped_total"]] = as.integer(1)
             temp = data.frame(
               ped = values[["ped_total"]],
               x,
               phenotype = factor(c("", "", "aff", "nonaff", "nonaff", "nonaff", "aff", "nonaff"), levels = c("", "nonaff", "aff")),
               carrier = factor(c("", "", "het", "no", "", "", "het", ""), levels = c("", "no", "het", "hom")),
               proband = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
               age = as.integer(50),
               lclass = as.integer(x$SEX))
           }
    )
    
    # Attach factors
    if(!is.null(values[["factor_vector"]])) # not needed...
      temp[, values[["factor_vector"]]] = FALSE
    
    if(input$input_example %in% c("Trio", "Full siblings"))
      values[["peddata"]] = rbind(values[["peddata"]], temp)
    else
      values[["peddata"]] = temp
    values[["ped_current"]] = values[["ped_total"]]

    updateSelectInput(inputId = "input_example", selected = "")
  })
  
  
  
  # Pedigree table
  output$pedTable = renderRHandsontable({
    req(values[["peddata"]])
    rhandsontable(values[["peddata"]],
                  useTypes = TRUE,
                  manualColumnResize = TRUE,
                  rowHeaders = NULL,
                  # height = if(nrow(values[["peddata"]])> 12) 300 else NULL
                  height = 175) %>%
      hot_validate_numeric(col = 'age', min = 1, max = 100, allowInvalid = FALSE) %>%
      hot_validate_numeric(col = 'lclass', min = 1, max = nrow(values[["lclassdata"]]), allowInvalid = FALSE) %>%
      hot_table(highlightRow = TRUE) %>%
      hot_col(c("ped", "id", "fid", "mid", "sex"), renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                              Handsontable.renderers.NumericRenderer.apply(this, arguments);
                              td.style.background = '#FCFCFC';
                              cellProperties.readOnly = true;}") %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  })
  
  
  
  # Update data from table edits (this runs twice...)
  observe(priority = 1, {
    req(!is.null(input$pedTable))
    message("Update data from table edits (this runs twice)")

    temp_full = hot_to_r(input$pedTable)

    temp =
      within(
        temp_full[,c("ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age", "lclass")], {
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
  
  
  
  # Liability classes table
  output$lclassTable = renderRHandsontable({
    req(values[["lclassdata"]])
    rhandsontable(values[["lclassdata"]],
                  useTypes = TRUE,
                  manualColumnResize = TRUE,
                  # rowHeaders = NULL,
                  height = if(nrow(values[["lclassdata"]])> 12) 300 else NULL) %>%
      hot_validate_numeric(col = c('f0', 'f1', 'f2'), min = 0, max = 1, allowInvalid = FALSE) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE,
                       customOpts = list(
                         remove_row = list(
                           name = 'Remove row',
                           hidden = htmlwidgets::JS("function() {
                             return this.getSelectedLast()[0] === 0;
                           }")
                         )))
  })
  
  
    
  # Update lclass data from table edits (this runs twice...)
  observe({
    req(!is.null(input$lclassTable)) # input$lclass_mode, input$input_example == ""
    
    message("Update liability classes from table edits (this runs twice)")
    
    temp = hot_to_r(input$lclassTable)
    values[["lclassdata"]] = temp
    
    # Update sensitivity analysis choices
    to_keep = grep("_f0a(_[f,m])?$|_f0b(_[f,m])?$|_f2a(_[f,m])?$|_f2b(_[f,m])?$|_f1v(_[f,m])?$|_risk$", values[["flb_v"]], value = TRUE)
    values[["flb_v"]] = c("afreq", paste0("lclass", as.vector(t(outer(seq(nrow(temp)), c("_f0", "_f1", "_f2"), paste0)))), to_keep)
  })
  
  
  
  # Update FLB indexes
  observe({
    req(values[["peddata"]], input$input_example == "") # avoids updating twice...
    message("Update FLB indexes")
    values[["affected"]] = values[["peddata"]][["phenotype"]] != "" & values[["peddata"]][["phenotype"]] != "nonaff"
    values[["unknown"]] = values[["peddata"]][["phenotype"]] == ""
    values[["proband"]] = values[["peddata"]][["proband"]] == 1
    values[["carriers"]] = values[["peddata"]][["carrier"]] == "het"
    values[["homozygous"]] = values[["peddata"]][["carrier"]] == "hom"
    values[["noncarriers"]] = values[["peddata"]][["carrier"]] == "no"
  })
  
  
  
  # Segregation plot
  output$pedPlot = renderPlot({
    req(values[["peddata"]], input$input_example == "")
    idxs = which(values[["peddata"]][["ped"]] == values[["ped_current"]])
    plotSegregation(as.ped(values[["peddata"]][idxs, c("id", "fid", "mid", "sex")]),
                    affected = which(values[["affected"]][idxs]),
                    unknown = which(values[["unknown"]][idxs]),
                    proband = which(values[["proband"]][idxs]),
                    if(length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
                    if(length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
                    if(length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs])
    )
  })
  
  
  
  # Update penetrances
  observe({
    
    req(!input$lclass_mode, values[["pheno_total"]]>0)
    
    # Validate that all necessary UI inputs are created
    if(values[["sexspec_vector"]][values[["pheno_total"]]] == TRUE)
      req(input[[paste0("pheno", values[["pheno_total"]], "_f1v_f")]])
    else
      req(input[[paste0("pheno", values[["pheno_total"]], "_f1v")]])
    if(values[["factor_total"]] > 0)
      req(input[[paste0("factor", values[["factor_total"]], "_risk")]])
    
    message("Update penetrances")
    
    # Select relevant inputs (because they are NOT removed by removeUI())
    values[["pheno_inputs"]] = lapply(seq(values[["pheno_total"]]), function(i) {
      temp = paste0("pheno", i,  c("_f0a", "_f0b", "_f2a", "_f2b", "_f1v"))
      if(values[["sexspec_vector"]][i])
        temp = outer(temp, c("_m", "_f"), paste0)
      return(temp)
    })
    inputs = sapply(c("afreq", "pheno_dist",
                      unlist(values[["pheno_inputs"]]),
                      # grep("_f0a(_[f,m])?$|_f0b(_[f,m])?$|_f2a(_[f,m])?$|_f2b(_[f,m])?$|_f1v(_[f,m])?$", names(input), value = TRUE),
                      if(values[["factor_total"]] > 0) paste0("factor", seq(values[["factor_total"]]), "_risk")),
                    function(x) input[[x]], simplify = FALSE, USE.NAMES = TRUE)
    # M (collect inputs + get penetrance values)
    inputs_m = inputs
    names(inputs_m) = gsub("_m$", "", names(inputs_m))
    f_m = get_f(x, values, inputs_m)
    
    # F (collect inputs + get penetrance values)
    inputs_f = inputs
    names(inputs_f) = gsub("_f$", "", names(inputs_f))
    f_f = get_f(x, values, inputs_f)
    
    # Join
    if(!input$xr_model){
      values[["f"]] = dplyr::bind_rows(f_m[,c("f0", "f1", "f2")], f_f[,c("f0", "f1", "f2")])
      values[["f_idx"]] = array(1:nrow(values[["f"]]),
                                dim = c(length(x), values[["pheno_total"]]+1, 2^values[["factor_total"]], 2),
                                dimnames = list(x, c(values[["pheno_vector"]], "nonaff"), seq(2^values[["factor_total"]]), c("1", "2")))
    }
    else {
      values[["f"]] = list("male" = f_m[,c("f0", "f1")], "female" = f_f[,c("f0", "f1", "f2")])
      values[["f_idx"]] = array(1:nrow(values[["f"]][[1]]),
                                dim = c(length(x), values[["pheno_total"]]+1, 2^values[["factor_total"]]),
                                dimnames = list(x, c(values[["pheno_vector"]], "nonaff"), seq(2^values[["factor_total"]])))
    }
  })
  
  
  
  # Update liability class groups (problem with double run)
  observe(priority = -1, {
    req(!input$lclass_mode, values[["pheno_total"]]>0, values[["peddata"]], input$input_example == "")
    
    # Validate that all necessary UI inputs are created
    if(values[["sexspec_vector"]][values[["pheno_total"]]] == TRUE)
      req(input[[paste0("pheno", values[["pheno_total"]], "_f1v_f")]])
    else
      req(input[[paste0("pheno", values[["pheno_total"]], "_f1v")]])
    if(values[["factor_total"]] > 0)
      req(input[[paste0("factor", values[["factor_total"]], "_risk")]])
    
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
    if(!input$xr_model)
      values[["lclass"]] = mapply(function(i,j,k,l) values[["f_idx"]][i,j,k,l],
                                  i = values[["peddata"]][["age"]],
                                  j = new_j,
                                  k = lclass_idx,
                                  l = values[["peddata"]][["sex"]])
    else
      values[["lclass"]] = mapply(function(i,j,k) values[["f_idx"]][i,j,k],
                                  i = values[["peddata"]][["age"]],
                                  j = new_j,
                                  k = lclass_idx)
  })
  
  
  
  # Update penetrances (liabclass mode)
  observe({
    req(input$lclass_mode, nrow(values[["lclassdata"]]>0))
    
    message("Update penetrances (liabclass mode)")
    
    if(!input$xr_model){
      values[["f"]] = data.matrix(values[["lclassdata"]])[1:nrow(values[["lclassdata"]]),c("f0", "f1", "f2")]
    }
    else {
      temp = list("male" = data.matrix(values[["lclassdata"]])[1:nrow(values[["lclassdata"]]),c("f0", "f1")],
                  "female" = data.matrix(values[["lclassdata"]])[1:nrow(values[["lclassdata"]]),c("f0", "f1", "f2")])
      if(length(temp[["female"]])>3) # if there are 2+ rows, in females, substitutes missing f2 values with f1 (because those lclasses SHOULD BE only for males)
        temp[["female"]][is.na(temp[["female"]][,"f2"]), "f2"] = temp[["female"]][is.na(temp[["female"]][, "f2"]), "f1"]
      values[["f"]] = temp
    }
  })
  
  # # Penetrance plots
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
  # output$CRPlot = renderPlot({
  #   req(values[["CR"]], values[["pheno_total"]]>0)
  #   dat = values[["CR"]]
  #   dat$phenotype = gsub('nonaff', 'total', dat$phenotype)
  #   dat$phenotype = factor(dat$phenotype, levels = c(values[["pheno_vector"]], 'total'))
  #   ggplot(dat) +
  #     geom_line(aes(x = age, y = f0, color = "f0", group = 1)) +
  #     geom_line(aes(x = age, y = f1, color = "f1", group = 1)) +
  #     geom_line(aes(x = age, y = f2, color = "f2", group = 1)) +
  #     facet_wrap(~ phenotype, scales = 'free_y') +
  #     theme_classic()
  # })
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
    values[["flb"]] = 
      sapply(1:values[["ped_total"]], function(pedid) {
        idxs = which(values[["peddata"]][["ped"]] == pedid)
        tryCatch(
          if(input$lclass_mode)
            FLB(x = as.ped(values[["peddata"]][idxs, c("id", "fid", "mid", "sex")]),
                affected = which(values[["affected"]][idxs]),
                unknown = which(values[["unknown"]][idxs]),
                proband = which(values[["proband"]][idxs]),
                if(length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
                if(length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
                if(length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs]),
                freq = 10^input$afreq,
                penetrances = values[["f"]],
                liability = values[["peddata"]][["lclass"]][idxs],
                Xchrom = ifelse(input$xr_model, TRUE, FALSE),
                details = FALSE)
          else
            FLB(x = as.ped(values[["peddata"]][idxs, c("id", "fid", "mid", "sex")]),
                affected = which(values[["affected"]][idxs]),
                unknown = which(values[["unknown"]][idxs]),
                proband = which(values[["proband"]][idxs]),
                if(length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
                if(length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
                if(length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs]),
                freq = 10^input$afreq,
                penetrances = values[["f"]],
                liability = values[["lclass"]][idxs],
                Xchrom = ifelse(input$xr_model, TRUE, FALSE),
                details = FALSE),
          error = function(err) NA)
      })
    
  })
  
  
  # FLB calculation (show if not null)
  output$flb_main = renderText({
    res = prod(values[["flb"]])
    if(!is.na(res) & !is.null(values[["flb"]]))
      res
    else
      " "
  })
  
  
  
  output$testplot = renderPlot({
    
    req(values[["flb_vals"]])
    
    # ggplot() +
    #   geom_contour_filled(aes(x = values[["grid"]][,1], y = values[["grid"]][,2], z = values[["flb_vals"]]))
    contourplot(values[["grid"]], round(values[["flb_vals"]], 7), values[["flb"]])
    
  })
  
  observeEvent(input$flb_run, {
    
    req(values[["pheno_total"]]>0 | nrow(values[["lclassdata"]]) >=1, # (right now, not necessary due to 3rd condition + lack of choices)
        values[["peddata"]],
        input$flb_v1 != input$flb_v2)
    
    # Create grid of values
    values[["grid"]] = setNames(
      expand.grid(seq(input$flb_s1[1], input$flb_s1[2], l = 10),
                  seq(input$flb_s2[1], input$flb_s2[2], l = 10)),
      c(names(values[["flb_choices"]])[which(values[["flb_choices"]] == input$flb_v1)],
        names(values[["flb_choices"]])[which(values[["flb_choices"]] == input$flb_v2)])
    )

    
    if(input$lclass_mode) {
      
      # Collect inputs
      fullgrid = sapply(c("afreq"),
                        function(x) input[[x]], simplify = FALSE, USE.NAMES = TRUE)
      fullgrid[[input$flb_v1]] = values[["grid"]][, 1]
      fullgrid[[input$flb_v2]] = values[["grid"]][, 2]
      fullgrid = as.data.frame(fullgrid)
      
      # Calculate FLB
      values[["flb_vals"]] = sapply(seq(nrow(fullgrid)), function(i) {
        
        # Get penetrance values
        f = values[["lclassdata"]]
        if(grepl("_f[0-2]$", input$flb_v1))
          f[gsub("\\D+(\\d+)_.*", "\\1", input$flb_v1), gsub(".*_", "", input$flb_v1)] = values[["grid"]][i,1]
        if(grepl("_f[0-2]$", input$flb_v2))
          f[gsub("\\D+(\\d+)_.*", "\\1", input$flb_v2), gsub(".*_", "", input$flb_v2)] = values[["grid"]][i,2]
        
        # XR model?
        if(!input$xr_model){
          f = data.matrix(f)[1:nrow(f),c("f0", "f1", "f2")]
        }
        else {
          f = list("male" = data.matrix(f)[1:nrow(f),c("f0", "f1")],
                   "female" = data.matrix(f)[1:nrow(f),c("f0", "f1", "f2")])
          if(length(f[["female"]])>3) # if there are 2+ rows, in females, substitutes missing f2 values with f1 (because those lclasses SHOULD BE only for males)
            f[["female"]][is.na(f[["female"]][,"f2"]), "f2"] = f[["female"]][is.na(f[["female"]][, "f2"]), "f1"]
        }
        
        # Calculate BF
        prod(
          sapply(1:values[["ped_total"]], function(pedid) {
            idxs = which(values[["peddata"]][["ped"]] == pedid)
            tryCatch(
              FLB(x = as.ped(values[["peddata"]][idxs, c("id", "fid", "mid", "sex")]),
                  affected = which(values[["affected"]][idxs]),
                  unknown = which(values[["unknown"]][idxs]),
                  proband = which(values[["proband"]][idxs]),
                  if(length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
                  if(length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
                  if(length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs]),
                  freq = 10^fullgrid[i, "afreq"],
                  penetrances = f,
                  liability = values[["peddata"]][["lclass"]][idxs],
                  Xchrom = ifelse(input$xr_model, TRUE, FALSE),
                  details = FALSE),
              error = function(err) NA)
          })
        )
      })
    }
    
    else {
      # Collect inputs
      fullgrid = sapply(c("afreq", "pheno_dist",
                          unlist(values[["pheno_inputs"]]),
                          # grep("_f0a(_[m,f])?$|_f0b(_[m,f])?$|_f2a(_[m,f])?$|_f2b(_[m,f])?$|_f1v(_[m,f])?$", names(input), value = TRUE),
                          if(values[["factor_total"]] > 0) paste0("factor", seq(values[["factor_total"]]), "_risk")),
                        function(x) input[[x]], simplify = FALSE, USE.NAMES = TRUE)
      fullgrid[[input$flb_v1]] = values[["grid"]][, 1]
      fullgrid[[input$flb_v2]] = values[["grid"]][, 2]
      fullgrid = as.data.frame(fullgrid)
      

      # Calculate FLB
      values[["flb_vals"]] = sapply(seq(nrow(fullgrid)), function(i) {
        
        # M (collect inputs + get penetrance values)
        inputs_m = fullgrid[i,]
        names(inputs_m) = gsub("_m$", "", names(inputs_m))
        f_m = get_f(x, values, inputs_m)
        
        # F (collect inputs + get penetrance values)
        inputs_f = fullgrid[i,]
        names(inputs_f) = gsub("_f$", "", names(inputs_f))
        f_f = get_f(x, values, inputs_f)
        
        # XR model?
        if(!input$xr_model){
          f = dplyr::bind_rows(f_m[,c("f0", "f1", "f2")], f_f[,c("f0", "f1", "f2")])
        }
        else {
          f = list("male" = f_m[,c("f0", "f1")], "female" = f_f[,c("f0", "f1", "f2")])
        }
        
        # Calculate BF
        prod(
          sapply(1:values[["ped_total"]], function(pedid) {
            idxs = which(values[["peddata"]][["ped"]] == pedid)
            tryCatch(
              FLB(x = as.ped(values[["peddata"]][idxs, c("id", "fid", "mid", "sex")]),
                  affected = which(values[["affected"]][idxs]),
                  unknown = which(values[["unknown"]][idxs]),
                  proband = which(values[["proband"]][idxs]),
                  if(length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
                  if(length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
                  if(length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs]),
                  freq = 10^fullgrid[i, "afreq"],
                  penetrances = f,
                  liability = values[["lclass"]][idxs],
                  Xchrom = ifelse(input$xr_model, TRUE, FALSE),
                  details = FALSE),
              error = function(err) NA)
          })
        )
      })
    }
    
    
    # Show if not NULL
    if(!is.null(values[["flb_vals"]]))
      showModal(modalDialog(plotOutput("testplot", height = "550px"), size = "m"))
    
  })
  
  
  
  # Add/remove phenotypes
  observeEvent(input$pheno_add, {
    req(values[["pheno_total"]] < 9,
        !grepl("^\\s*$", input$pheno_name),
        !input$pheno_name %in% c(values[["pheno_vector"]], "nonaff"))
    add_pheno(values, input$pheno_name, c(0, 1, 50, 1, 50), sexspec = input$sexspec_mode)
    updateTextInput(inputId = "pheno_name", value = "")
  })
  observeEvent(input$pheno_rmv, {
    req(values[["pheno_total"]] > 0)
    rmv_pheno(values)
    # inputs = sapply(c("afreq", "pheno_dist",
    #                   grep("_f0a(_[f,m])?$|_f0b(_[f,m])?$|_f2a(_[f,m])?$|_f2b(_[f,m])?$|_f1v(_[f,m])?$", names(input), value = TRUE),
    #                   if(values[["factor_total"]] > 0) paste0("factor", seq(values[["factor_total"]]), "_risk")),
    #                 function(x) input[[x]], simplify = FALSE, USE.NAMES = TRUE)
    # print(names(inputs))
  })
  
  
  
  # Add/remove factors
  observeEvent(input$factor_add, {
    req(values[["factor_total"]] < 3,
        !grepl("^\\s*$", input$factor_name),
        !input$factor_name %in% c(values[["factor_vector"]], "ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age", "lclass"))
    add_factor(values, input$factor_name, 0)
    updateTextInput(inputId = "factor_name", value = "")
  })
  observeEvent(input$factor_rmv, {
    req(values[["factor_total"]] > 0)
    rmv_factor(values)
  })
  
  
  
  # Store sensitivity analysis choices
  observe({
    values[["flb_v1"]] = input$flb_v1
    values[["flb_v2"]] = input$flb_v2
  })
  
  # Update sensitivity analysis choices
  observeEvent(ignoreInit = TRUE, c(values[["flb_v"]], input$lclass_mode), {
    # observe({
    allvars = values[["flb_v"]]
    
    # fix names
    names(allvars) = allvars
    # afreq
    names(allvars) = gsub("^afreq$", "log10(afreq)", names(allvars))
    # phenotype names
    if(values[["pheno_total"]] > 0)
      names(allvars) = stringr::str_replace_all(names(allvars), setNames(values[["pheno_vector"]], paste0("^(pheno", seq(values[["pheno_total"]]), ")")))
    # risk factor names
    if(values[["factor_total"]] > 0)
      names(allvars) = stringr::str_replace_all(names(allvars), setNames(values[["factor_vector"]], paste0("^(factor", seq(values[["factor_total"]]), ")")))
    # sex-specific parameters
    names(allvars) = gsub("(.*)_([m,f])$", "(\\2) \\1", names(allvars))
    # parameter name
    names(allvars) = stringr::str_replace_all(names(allvars), setNames(c(" f0", " f1", " f2", " f0 shape", " f0 scale", " f2 shape", " f2 scale", " f1 log(coef)", " log(risk)"),
                                                                       c("_f0$", "_f1$", "_f2$", "_f0a$", "_f0b$", "_f2a$", "_f2b$", "_f1v$", "_risk")))
    
    to_keep = ifelse(input$lclass_mode, c("^afreq$|_f0$|_f1$|_f2$"), c("^afreq$|_f0a(_[f,m])?$|_f0b(_[f,m])?$|_f2a(_[f,m])?$|_f2b(_[f,m])?$|_f1v(_[f,m])?$|_risk$"))
    flb_choices = allvars[grep(to_keep, allvars)]
    updateSelectInput(inputId = "flb_v1", choices = flb_choices, selected = values[["flb_v1"]])
    updateSelectInput(inputId = "flb_v2", choices = flb_choices, selected = values[["flb_v2"]])
    values[["flb_choices"]] = flb_choices
    
    # fhelp transfer options
    if(!input$lclass_mode & values[["pheno_total"]]>0) {
      fhelp_choices = grep("^pheno[[:digit:]]+_f[0,2]", flb_choices, value = TRUE)
      names(fhelp_choices) = gsub("^(.*) ([[:alnum:]]+)$", "\\1", names(fhelp_choices))
      values[["fhelp_choices"]] = c("", fhelp_choices[!duplicated(names(fhelp_choices))])
    }
    else
      values[["fhelp_choices"]] = ""
  })
  
  # Update sliders
  observeEvent(input$flb_v1, {
    slidervals = switch(gsub(".*_", "", gsub("(.*)_[m,f]$", "\\1", input$flb_v1)),
                        "afreq" = c(-5, -1),
                        "f0a" = c(1, 5),
                        "f2a" = c(1, 5),
                        "f0b" = c(5, 500),
                        "f2b" = c(5, 500),
                        "f1v" = c(-3, +3),
                        "risk" = c(-3, +3),
                        "f0" = c(0.001, 0.999),
                        "f1" = c(0.001, 0.999),
                        "f2" = c(0.001, 0.999))
    stepsize = (slidervals[2]-slidervals[1])/100
    updateSliderInput(inputId = "flb_s1", min = slidervals[1], max = slidervals[2], step = stepsize, value = slidervals)
  })
  observeEvent(input$flb_v2, {
    slidervals = switch(gsub(".*_", "", gsub("(.*)_[m,f]$", "\\1", input$flb_v2)),
                        "afreq" = c(-5, -1),
                        "f0a" = c(1, 5),
                        "f2a" = c(1, 5),
                        "f0b" = c(5, 500),
                        "f2b" = c(5, 500),
                        "f1v" = c(-3, +3),
                        "risk" = c(-3, +3),
                        "f0" = c(0.001, 0.999),
                        "f1" = c(0.001, 0.999),
                        "f2" = c(0.001, 0.999))
    stepsize = (slidervals[2]-slidervals[1])/100
    updateSliderInput(inputId = "flb_s2", min = slidervals[1], max = slidervals[2], step = stepsize, value = slidervals)
  })
  
  
  # fhelp values
  observe({
    values[["fhelp_p"]] = values[["fhelpdata"]][complete.cases(values[["fhelpdata"]]), "quantile"]
    values[["fhelp_q"]] = values[["fhelpdata"]][complete.cases(values[["fhelpdata"]]), "age"]
    
    values[["fhelp_sol"]] =
      tryCatch(
        switch(input$fhelp_dist,
               "Weibull" = optim(par = c(0, 1), function(params) {
                 summand = suppressWarnings(pweibull(q = values[["fhelp_q"]], shape = params[1], scale = params[2]) - values[["fhelp_p"]])
                 sum(summand^2)},
                 method = "L-BFGS-B", lower = c(0.001, 0.001), upper = c(10000, 10000)),
               "Log-logist" = optim(par = c(0, 1), function(params) {
                 summand = suppressWarnings(pllogis(q = values[["fhelp_q"]], shape = params[1], scale = params[2]) - values[["fhelp_p"]])
                 sum(summand^2)},
                 method = "L-BFGS-B", lower = c(0.001, 0.001), upper = c(10000, 10000))),
        error = function(err) NULL)
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
    
    BFplot(prod(values[["flb"]]))
  })
  
  
  
  observeEvent(input$link1, {
    showModal(modalDialog(
      # h5("Data Guidelines"),
      # tags$ol(
      #   tags$li("Must have Resp_ID as the first column, occasion_ID as second and dependent variable as the third"),
      #   tags$li("Must have no missing value in any fields")
      # ),
      fluidRow(
        column(5,
               inputPanel(
                 W_fhelp_dist,
                 div(style = "margin-top:-10px;margin-bottom:20px;margin-right:20px;",
                     verbatimTextOutput("fhelp_text")),
                 rHandsontableOutput("fhelpTable"),
                 selectInput(inputId = "fhelp_transfer", label = "Transfer to", choices = values[["fhelp_choices"]])
               )
        ),
        column(7, plotOutput(outputId = "fhelp_plot", height = "350px"))
      ),
      easyClose = TRUE, footer = NULL, size = "m")
    )
  })
  
  
  
  # fhelp table
  output$fhelpTable = renderRHandsontable({
    req(values[["fhelpdata"]])
    rhandsontable(values[["fhelpdata"]],
                  useTypes = TRUE,
                  manualColumnResize = TRUE,
                  # rowHeaders = NULL,
                  height = if(nrow(values[["fhelpdata"]])> 12) 300 else NULL) %>%
      hot_validate_numeric(col = 'quantile', min = 0, max = 1, allowInvalid = FALSE) %>%
      hot_validate_numeric(col = 'age', min = 0, max = 100, allowInvalid = FALSE) %>%
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })
  
  
  
  # Update fhelp data from table edits (this runs twice...)
  observe(priority = 1, {
    req(!is.null(input$fhelpTable))
    message("Update fhelp from table edits (this runs twice)")
    
    temp = hot_to_r(input$fhelpTable)
    values[["fhelpdata"]] = temp
    
  })
  
  
  # Transfer parameters (selection inside modal)
  observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, input$fhelp_transfer, {
    req(input$fhelp_transfer != "")
    message("Transfering parameters")
    updateNumericInput(inputId = input$fhelp_transfer, value = values[["fhelp_sol"]]$par[1])
    updateNumericInput(inputId = gsub("(f[0,2])a(_[m,f])?$", "\\1b\\2", input$fhelp_transfer), value = values[["fhelp_sol"]]$par[2])
    updateSelectInput(inputId = "fhelp_transfer", selected = "")
  })
  
  
  
  # Pedigree plot selector
  observeEvent(input$ped_prev, {
    req(values[["ped_current"]]>1)
    values[["ped_current"]] = values[["ped_current"]] - 1
  })
  observeEvent(input$ped_next, {
    req(values[["ped_current"]]<values[["ped_total"]])
    values[["ped_current"]] = values[["ped_current"]] + 1
  })
  output$ped_current = renderUI({
    helpText(paste0("Pedigree ", values[["ped_current"]], "/", values[["ped_total"]]))
  })
  
  
}

# End ---------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

# shiny::reactlogShow()
