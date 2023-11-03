# Input widgets -----------------------------------------------------------

w_help = function(id)
  actionBttn(
    inputId = NS(id, "help"),
    label = NULL,
    style = "jelly",
    color = "warning",
    size = "s",
    icon = icon("question")
  )

# w_addCI = function(id)
#   popover(
#     actionBttn(
#       inputId = NS(id, "addCI"),
#       label = "ADD",
#       style = "jelly",
#       size = "xs",
#       icon = icon("plus")
#     ),
#     title = NULL,
#     content = "Add an entry."
#   )
# 
# w_rmvCI = function(id)
#   popover(
#     actionBttn(
#       inputId = NS(id, "rmvCI"),
#       label = "RMV",
#       style = "jelly",
#       size = "xs",
#       icon = icon("minus")
#     ),
#     title = NULL,
#     content = "Delete the last entry."
#   )

w_transfer = function(id)
  pickerInput(
    inputId = NS(id, "transfer"),
    label = NULL,
    choices = NULL,
    multiple = TRUE,
    selected = "",
    options = list(
      `icon-base` = "fa-solid fa-fw",
      "max-options" = 1,
      "none-selected-text" = "Use these parameters for...",
      "live-search" = TRUE,
      `style` = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
    )
  )


# Module code -------------------------------------------------------------

assistantModalUI = function(id) {
  modalDialog(
    title = 
      div(
        div(class = "leftcolumn", HTML("<i class='fa fa-circle-info'></i> Assistant")),
        div(class = "rightcolumn", w_help(id))
      ),
    size = "l",
    easyClose = TRUE,
    footer = NULL,
    div(
      div(
        class = "leftcolumn",
        style = "width: 300px;",
        div("Incidence data", class = "heading"),
        # fluidRow(
        #   div(w_addCI(id), style = "margin-top: 0.2rem;"),
        #   div(w_rmvCI(id), style = "margin-top: 0.2rem;")
        # ),
        div(rHandsontableOutput(NS(id, "assisTable")), style = "margin-top: 0.5rem; margin-bottom: 2rem;"),
        div("Model parameters", class = "heading"),
        div(verbatimTextOutput(NS(id, "assisSol")), style = "margin-top: 0.1rem;")
      ),
      div(
        class = "rightcolumn",
        div("Fitted values", class = "heading"),
        plotlyOutput(NS(id, "assisPlot"), width = "425px", height = "360px"),
        div(w_transfer(id), style = "float:right; margin-top: 2rem; width: 250px;")
      )
    )
  )
}

assistantModalServer = function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Default values
    values[["spl5"]] = bs(1:100, df = 5, intercept = TRUE)
    values[["assisData"]] =
      data.table(
        age = c(30L, 40L, 50L, 60L, 70L, 80L, rep(NA_integer_, 4)),
        f0CI = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07, rep(NA_real_, 4)),
        f2CI = c(0.10, 0.15, 0.25, 0.35, 0.45, 0.50, rep(NA_real_, 4))
      )
    
    # Help
    observeEvent(input$help, {
      shinyalert(
        className = "helpbox",
        html = TRUE,
        text = read_file("modals/assistantModal.html"),
        animation = "slide-from-bottom",
        showConfirmButton = FALSE,
        closeOnClickOutside = TRUE,
        size = "m"
      )
    })
    
    # Incidence data
    output$assisTable = renderRHandsontable({
      req(values[["assisData"]])
      rhandsontable(
        values[["assisData"]],
        digits = 14,
        useTypes = TRUE,
        manualColumnResize = TRUE,
        rowHeaders = NULL,
        height = 225,
        # height = if (nrow(values[["assisData"]]) > 6) 175 else NULL,
        colHeaders = c("age", gsub("risk", "CI", values[["rriskNames"]][c(1,4)])),
        overflow = "visible"
      ) %>%
        hot_table(colWidths = "75px") %>%
        hot_validate_numeric(col = 1, min = 1, max = 100, allowInvalid = FALSE) %>%
        hot_col(1, format = "0,0") %>% 
        hot_validate_numeric(col = 2:3, min = 0, max = 1, allowInvalid = FALSE) %>%
        hot_col(2:3, format = "0,0000000") %>% 
        hot_context_menu(
          highlightRow = TRUE,
          allowRowEdit = TRUE,
          allowColEdit = FALSE,
          customOpts =
            list(
              remove_row =
                list(
                  name = 'Remove row',
                  hidden = JS("function() {return this.getSelectedLast()[0] === 0;}")
                ),
              undo =
                list(
                  hidden = JS("function() {return true;}")
                ),
              redo =
                list(
                  hidden = JS("function() {return true;}")
                ),
              alignment =
                list(
                  hidden = JS("function() {return true;}")
                )
            )
        )
    })
    
    # Update data from table edits
    observeEvent(priority = 1, input$assisTable, {
      req(!is.null(input$assisTable))
      message("Update assis data from table edits")
      assisData = hot_to_r(input$assisTable)
      values[["assisData"]] = assisData
    })
    
    # # (lclass) Add/rmv table entry
    # observeEvent(input$addCI, {
    #   values[["assisData"]] = rbind(values[["assisData"]], values[["assisData"]][NA])
    # })
    # observeEvent(input$rmvCI, {
    #   req(nrow(values[["assisData"]]) > 1)
    #   values[["assisData"]] = values[["assisData"]][1:(nrow(values[["assisData"]]) - 1),]
    # })
    
    # Optimization
    observe({
      values[["OptPar"]][["data"]][["f0"]] = values[["assisData"]][complete.cases(values[["assisData"]][, .(age, f0CI)]), .(age, f0CI)]
      values[["OptPar"]][["data"]][["f2"]] = values[["assisData"]][complete.cases(values[["assisData"]][, .(age, f2CI)]), .(age, f2CI)]
      
      # Baseline rates
      optf0 =
        optim(par = c(0.5, 50, 20), function(params) {
          fitted = params[1]*ptrunc(values[["OptPar"]][["data"]][["f0"]][["age"]], "norm", mean = params[2], sd = params[3], a = 0, b = 100)
          sum((fitted - values[["OptPar"]][["data"]][["f0"]][["f0CI"]])^2)
        },
        method = "L-BFGS-B",
        lower = c(0.001, 0.001, 0.001), upper = c(0.999, 1000, 1000),
        control = list(factr = 1e6))
      values[["OptPar"]][["f0"]] = round(optf0$par, 4) # round
      values[["OptPar"]][["f0CI"]] = values[["OptPar"]][["f0"]][1]*ptrunc(1:100, "norm", mean = values[["OptPar"]][["f0"]][2], sd = values[["OptPar"]][["f0"]][3], a = 0, b = 100)
      values[["OptPar"]][["f0Hz"]] = diff(c(0, -log(1 - values[["OptPar"]][["f0CI"]])))
      
      # Variant-associated rates
      optf2 =
        optim(par = rep(0, 5), function(params) {
          fitted = cumsum(exp(values[["spl5"]] %*% (as.vector(params))) * values[["OptPar"]][["f0Hz"]])[values[["OptPar"]][["data"]][["f2"]][["age"]]]
          sum((fitted + log(1 - values[["OptPar"]][["data"]][["f2"]][["f2CI"]]))^2)
        },
        method = "L-BFGS-B",
        lower = rep(0, 5), upper = rep(100, 5),
        control = list(factr = 1e6))
      values[["OptPar"]][["f2"]] = round(optf2$par, 2) # round
      values[["OptPar"]][["f2Hz"]] = exp(values[["spl5"]] %*% as.vector(values[["OptPar"]][["f2"]])) * values[["OptPar"]][["f0Hz"]]
      values[["OptPar"]][["f2CI"]] = 1 - exp(-cumsum(values[["OptPar"]][["f2Hz"]]))
      
    })
    
    # Display optimal parameters
    output$assisSol = renderText({
      rriskNames = values[["rriskNames"]][1:4]
      rriskNames = gsub("\n", " ", rriskNames)
      rriskNames[2:4] = paste0("\n", rriskNames[2:4])
      paste0(
        rriskNames[1], ": ", values[["OptPar"]][["f0"]][1],
        rriskNames[2], ": ", values[["OptPar"]][["f0"]][2],
        rriskNames[3], ": ", values[["OptPar"]][["f0"]][3],
        rriskNames[4], ": ", round(values[["OptPar"]][["f2CI"]][100], 4),
        "\nspline coefs: ", paste(values[["OptPar"]][["f2"]], collapse = ",")
      )
    })
    
    # Plot
    output$assisPlot = renderPlotly({
      dat = 
        data.table(
          age = 1:100,
          f0CI = values[["OptPar"]][["f0CI"]],
          f2CI = values[["OptPar"]][["f2CI"]]
        )
      g = ggplot(dat, aes(x = age)) +
        geom_line(aes(y = f0CI, group = 1), linewidth = .5, linetype = "42") +
        geom_line(aes(y = f2CI, group = 1), linewidth = .5, linetype = "solid") +
        geom_point(data = values[["OptPar"]][["data"]][["f0"]], mapping = aes(y = f0CI), alpha = .75, color = "#2D82B7") +
        geom_point(data = values[["OptPar"]][["data"]][["f2"]], mapping = aes(y = f2CI), alpha = .75, color = "#DC3545") +
        scale_x_continuous("age") +
        scale_y_continuous("Cumulative incidence") +
        theme_bw() +
        theme(
          panel.border = element_rect(color = "#4e4e4e"),
          legend.position = "none",
          text = element_text(family = "helvetica")
        )
      ggplotly(g, tooltip = c("x", "y")) %>%
        config(displayModeBar = FALSE)
    })
    
    # Update transfer choices
    observeEvent(values[["assisChoices"]], {
      delay(100, {
        updatePickerInput(
          session = getDefaultReactiveDomain(),
          inputId = "transfer",
          choices = values[["assisChoices"]],
          selected = NULL
        )
      })
    })
    
    # Transfer parameters
    observeEvent(input$transfer, {
      message("Transfering parameters")
      values[["phenoData"]][as.numeric(input$transfer), 3:8] = 
        data.table(
          values[["OptPar"]][["f0"]][1], 
          values[["OptPar"]][["f0"]][2],
          values[["OptPar"]][["f0"]][3],
          round(values[["OptPar"]][["f2CI"]][100], 4),
          "custom",
          paste(values[["OptPar"]][["f2"]], collapse = ",")
        )
      updatePickerInput(
        session = getDefaultReactiveDomain(),
        inputId = "transfer",
        selected = ""
      )
    })
    
  })
}
