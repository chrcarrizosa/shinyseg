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

w_pedCases = function(id)
  pickerInput(
    inputId = NS(id, "pedCases"),
    label = NULL,
    choices = c("Trio", "Full siblings", "Grandparent", "1st cousins", "Avuncular"),
    multiple = TRUE,
    options = list(
      `max-options` = 1,
      `none-selected-text` = "Add basic pedigree",
      `style` = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
    )
  )

w_loadPed = function(id)
  fileInput2(
    inputId = NS(id, "loadPed"),
    label = "Load pedfile",
    labelIcon = "file-import",
    progress = FALSE,
    divClass = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
  )

# w_addChild = function(id)
#   popover(
#     actionBttn(
#       inputId = NS(id, "addChild"),
#       label = NULL,
#       style = "jelly",
#       size = "s",
#       icon = icon("person-circle-plus")
#     ),
#     title = NULL,
#     content = "Add children."
#   )

# w_rmvPeople = function(id)
#   popover(
#     actionBttn(
#       inputId = NS(id, "rmvPeople"),
#       label = NULL,
#       style = "jelly",
#       size = "s",
#       icon = icon("person-circle-xmark")
#     ),
#     title = NULL,
#     content = "Remove people."
#   )

# w_swapSex = function(id)
#   popover(
#     actionBttn(
#       inputId = NS(id, "swapSex"),
#       label = NULL,
#       style = "jelly",
#       size = "s",
#       icon = icon("restroom")
#     ),
#     title = NULL,
#     content = "Swap sex."
#   )

w_rmvPed = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "rmvPed"),
      label = NULL,
      style = "jelly",
      size = "s",
      icon = icon("users-slash")
    ),
    title = NULL,
    content = "Remove family."
  )

w_modify = function(id)
  dropdown(
    label = "Modify",
    icon = icon("person-digging"),
    style = "jelly",
    size = "s",
    status = "default",
    div(
      class = "flexcontainer",
      # w_addChild(id),
      # w_rmvPeople(id),
      # w_swapSex(id),
      w_rmvPed(id)
    )
  )

w_quickPed = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "QuickPed"),
      label = "Use QuickPed",
      style = "jelly",
      size = "s",
      icon = icon("globe"),
      value = "Open popup",
      onclick = "window.open('https://magnusdv.shinyapps.io/quickped/')"
    ),
    title = NULL,
    content = "Make a pedigree using the QuickPed app."
  )


# Module code -------------------------------------------------------------

pedigreeBoxUI = function(id) {
  box(
    width = 12,
    title = 
      div(
        div(class = "leftcolumn", "Pedigree table"),
        div(class = "rightcolumn", w_help(id))
      ),
    collapsible = FALSE,
    fluidRow(
      div(
        w_pedCases(id),
        style = "width: 175px; margin-right: 1rem;"
      ),
      w_modify(id),
      w_loadPed(id),
      div(w_quickPed(id))
    ),
    div(rHandsontableOutput(NS(id, "pedTable")), style = "margin-top: 1rem;")
  )
}

pedigreeBoxServer = function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Help
    observeEvent(input$help, {
      shinyalert(
        className = "helpbox",
        html = TRUE,
        text = read_file("help/pedigreeBox.html"),
        animation = "slide-from-bottom",
        showConfirmButton = FALSE,
        closeOnClickOutside = TRUE,
        size = "m"
      )
    })
    
    # Basic cases
    observeEvent(input$pedCases, {
      values[["pedToAdd"]] = 
        switch(
          input$pedCases,
          "Trio" = nuclearPed(),
          "Full siblings" = nuclearPed(2),
          "Grandparent" = linearPed(2),
          "1st cousins" = cousinPed(1),
          "Avuncular" = avuncularPed()
        )
      updatePickerInput(
        session = getDefaultReactiveDomain(),
        inputId = "pedCases",
        selected = "")
    })
    # Read from file
    observeEvent(input$loadPed, {
      pedToAdd = 
        suppressWarnings(
          tryCatch(
            {
              pedToAdd = read.table(input$loadPed$datapath, header = TRUE)
              pedToAdd = as.ped(pedToAdd[c("id", "fid", "mid", "sex")])
              pedToAdd = relabel(pedToAdd, "asPlot")
            },
            error = function(err) NULL
          )
        )
      if (is.null(pedToAdd))
        showNotification(
          HTML("<i class='fas fa-triangle-exclamation'></i> Invalid pedigree file."),
          type = "error",
          duration = 3
        )
      
      values[["pedToAdd"]] = pedToAdd
    })
    
    # Plot pedigree to add
    output$pedToAdd = renderPlot({
      req(values[["pedToAdd"]])
      plot(values[["pedToAdd"]], margins = c(1, 2, 2, 2))
    }, execOnResize = TRUE)
    
    # Add pedigree
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["pedToAdd"]], {
      shinyalert(
        html = TRUE,
        text = 
          tagList(
            div(
              "The following pedigree will be added:",
              plotOutput(outputId = NS(id, "pedToAdd"), height = "300px", width = "85%"),
              align = "center"
            )
          ),
        animation = "slide-from-bottom",
        confirmButtonCol = "#39A0ED",
        showCancelButton = TRUE,
        size = "s",
        callbackR = function(x) {
          if (x) {
            message("Adding pedigree")
            values[["pedTotal"]] = as.integer(values[["pedTotal"]] + 1)
            pedData = data.table(
              ped = values[["pedTotal"]],
              as.data.frame(values[["pedToAdd"]]),
              phenotype = factor("nonaff", levels = unique(c("", "nonaff", "aff", values[["phenoVector"]]))),
              carrier = factor("", levels = c("", "neg", "het", "hom")),
              proband = FALSE,
              age = as.integer(50)
            )
            values[["pedData"]] = rbind(values[["pedData"]], pedData)
            values[["pedCurrent"]] = values[["pedTotal"]]
          }
          values[["pedToAdd"]] = NULL
        }
      )
    })
    
    # Remove family
    observeEvent(input$rmvPed, {
      req(values[["pedTotal"]] > 0)
      shinyalert(
        html = TRUE,
        text = 
          tagList(
            div(
              "The family currently displayed will be removed. (Note that this will delete all of its associated data)",
              # plotOutput(outputId = "pedToAdd", height = "300px", width = "85%")#,
              align = "center"
            )
          ),
        animation = "slide-from-bottom",
        confirmButtonCol = "#39A0ED",
        showCancelButton = TRUE,
        size = "s",
        callbackR = function(x) {
          if (x) {
            pedData = values[["pedData"]][ped != values[["pedCurrent"]], ]
            if (nrow(pedData) > 0)
              pedData[["ped"]] = as.integer(as.factor(pedData[["ped"]]))
            else
              pedData = NULL
            values[["pedData"]] = pedData
            values[["pedCurrent"]] = values[["pedTotal"]] - 1
            values[["pedTotal"]] = values[["pedTotal"]] - 1
            if (values[["pedTotal"]] == 0) values[["phenoTotal"]] = 0
          }
        }
      )
    })
    
    # Pedigree table
    values[["pedNames"]] = c("ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age")
    output$pedTable = renderRHandsontable({
      req(values[["pedData"]])
      rhandsontable(
        id = "pedTable",
        values[["pedData"]],
        useTypes = TRUE,
        manualColumnResize = TRUE,
        rowHeaders = NULL,
        height = if (nrow(values[["pedData"]]) > 6) 175 else NULL,
        colHeaders = values[["pedNames"]],
        overflow = "visible",
        selectCallback = TRUE
      ) %>%
        hot_validate_numeric(col = 9, min = 1, max = 100, allowInvalid = FALSE) %>%
        hot_col(c("ped", "id", "fid", "mid", "sex"), readOnly = TRUE) %>%
        hot_col(6, type = "autocomplete", strict = FALSE, colWidths = "110px") %>% 
        hot_col(7, colWidths = "75px") %>% 
        hot_col(8, colWidths = "90px", halign = "htCenter") %>%
        hot_table(highlightRow = TRUE, contextMenu = FALSE) %>%
        onRender(
          "function(el) {
          var hot = this.hot;
          disablePaste(hot);
          disableFill(hot);
        }")
    })
    
    # Update pedigree data from table edits
    observeEvent(input$pedTable$changes$changes, {
      before = input$pedTable$changes$changes[[1]][[3]]
      after = input$pedTable$changes$changes[[1]][[4]]
      if (is.null(before) || before != after) {
        message("Update pedigree data from table edits")
        temp = hot_to_r(input$pedTable)
        
        # Check if proband column was modified (col 7 for handsontable)
        if (input$pedTable$changes$changes[[1]][[2]] == 7) {
          message("Only one proband per pedigree")
          lapply(1:values[["pedTotal"]], function(pedid) {
            idxs = which(temp[["ped"]] == pedid)
            currentproband = temp[["proband"]][idxs]
            if (!is.null(values[["lastProband"]][idxs]) & sum(currentproband) > 1)
              newproband = values[["lastProband"]][idxs] != currentproband
            else
              newproband = currentproband
            values[["lastProband"]][idxs] = newproband
            temp[idxs, proband := newproband]
          })
        }
        
        values[["pedData"]] = temp
      }
    })
    
    # Update phenotype vector, FLB indexes
    observeEvent(ignoreNULL = FALSE, values[["pedData"]], {
      
      if (!is.null(values[["pedData"]])) {
        message("Check for changes in phenotypes")
        all_phenotypes = levels(droplevels(values[["pedData"]][["phenotype"]]))
        phenotypes = setdiff(all_phenotypes, c("", "nonaff"))
        values[["phenoVector"]] = phenotypes
        values[["phenoTotal"]] = length(phenotypes)
        
        message("Update FLB indexes")
        values[["affected"]] = !values[["pedData"]][["phenotype"]] %in% c("", "nonaff")
        values[["unknown"]] = values[["pedData"]][["phenotype"]] == ""
        values[["proband"]] = values[["pedData"]][["proband"]] == 1
        values[["carriers"]] = values[["pedData"]][["carrier"]] == "het"
        values[["homozygous"]] = values[["pedData"]][["carrier"]] == "hom"
        values[["noncarriers"]] = values[["pedData"]][["carrier"]] == "neg"
        
        lclasses = copy(values[["pedData"]][, c("sex", "phenotype", "age")])
        lclasses[, sex := factor(sex, levels = 1:2, labels = c("male", "female"))]
        values[["lclasses"]] = lclasses
        values[["okAge"]] = !any(is.na(lclasses[["age"]]))
      }
      
      else {
        values[["phenoVector"]] = NULL
        values[["phenoTotal"]] = 0
        values[["pedTotal"]] = 0
      }
    })
    
  })
}
