# Input widgets -----------------------------------------------------------

w_help = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "help"),
      label = NULL,
      style = "jelly",
      color = "warning",
      size = "s",
      icon = icon("question")
    ),
    title = NULL,
    content = "See help on this panel."
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
  popover(
    fileInput2(
      inputId = NS(id, "loadPed"),
      label = "Load pedfile",
      labelIcon = "file-import",
      progress = FALSE,
      divClass = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
    ),
    title = NULL,
    content = "Upload a pedigree from a ped file."
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
    content = "Remove a family."
  )

w_modify = function(id)
  popover(
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
    ),
    title = NULL,
    content = "Make changes on loaded families."
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
      pedLoaded = 
        switch(
          input$pedCases,
          "Trio" = nuclearPed(),
          "Full siblings" = nuclearPed(2),
          "Grandparent" = linearPed(2),
          "1st cousins" = cousinPed(1),
          "Avuncular" = avuncularPed()
        )
      values[["pedLoaded"]] = as.data.table(pedLoaded)
      updatePickerInput(
        session = getDefaultReactiveDomain(),
        inputId = "pedCases",
        selected = "")
    })
    # Read from file
    observeEvent(input$loadPed, {
      pedLoaded = 
        suppressWarnings(
          tryCatch(
            {
              fullData = read.table(input$loadPed$datapath, header = TRUE)
              if (exists("ped", fullData) & !any(is.na(fullData[["ped"]]))) {
                fullData$ped = as.integer(factor(fullData$ped, levels = unique(fullData$ped)))
                pedList = split(fullData[c("id", "fid", "mid", "sex")], fullData[["ped"]])
              }
              else
                pedList = list(fullData[c("id", "fid", "mid", "sex")])
              if (!is.pedList(pedList)) # if (!is.ped(fullData[c("id", "fid", "mid", "sex")]))
                stop # If pedigree is malformed, stop   # pedToAdd = relabel(pedToAdd, "asPlot")
              colSelect = intersect(c("ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age"), colnames(fullData))
              pedLoaded = fullData[colSelect]
              pedLoaded = setDT(pedLoaded)
            },
            error = function(err) NULL
          )
        )
      if (is.null(pedLoaded))
        showNotification(
          HTML("<i class='fas fa-triangle-exclamation'></i> Invalid pedigree file."),
          type = "error",
          duration = 3
        )
      
      values[["pedLoaded"]] = pedLoaded
    })
    
    # Fix/check input
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["pedLoaded"]], {
      message("Checking loaded pedfile")
      pedToAdd = copy(values[["pedLoaded"]])
      
      within(pedToAdd, {
        # Family identifier
        if (!exists("ped", pedToAdd))
          pedToAdd[, ped := as.integer(values[["pedTotal"]] + 1)]
        else
          pedToAdd[, ped := as.integer(values[["pedTotal"]] + ped)]
        # Fix phenotypes
        if (!exists("phenotype"))
          pedToAdd[, phenotype := factor("nonaff", levels = unique(c("", "nonaff", "aff", values[["phenoVector"]])))]
        else
          pedToAdd[, phenotype := factor(phenotype, levels = unique(c("", "nonaff", "aff", values[["phenoVector"]], phenotype)))]
        # Fix carrier
        if (!exists("carrier"))
          pedToAdd[, carrier := factor("", levels = c("", "neg", "het", "hom"))]
        else
          pedToAdd[, carrier := factor(carrier, levels = c("", "neg", "het", "hom"))]
        # Fix proband
        if (!exists("proband"))
          pedToAdd[, proband := FALSE]
        else {
          sapply(1:max(pedToAdd$ped), function(pedid) {
            idxs = which(pedToAdd$ped == pedid)
            if (any(!proband[idxs] %in% c(NA, "", "0", "1")) || sum(proband[idxs], na.rm = TRUE) > 1)
              pedToAdd[idxs, proband := FALSE]
            else
              pedToAdd[idxs, proband := proband %in% 1]
          })
          pedToAdd[, proband := as.logical(proband)]
        }
        # Fix age
        if (!exists("age"))
          pedToAdd[, age := as.integer(50)]
        else {
          pedToAdd[!age %in% 1:100, age := NA_integer_]
          pedToAdd[, age := as.integer(age)]
        }
      })
      setcolorder(pedToAdd, c("ped", "id", "fid", "mid", "sex", "phenotype", "carrier", "proband", "age"))
      
      values[["pedToAdd"]] = pedToAdd
      values[["pedToAddCurrent"]] = 1
      values[["pedLoaded"]] = NULL
    })
    
    # Plot pedigree to add
    output$pedToAdd = renderPlot({
      req(values[["pedToAdd"]])
      par(family = "helvetica")
      
      # Phenotypes
      all_phenotypes = levels(droplevels(values[["pedToAdd"]][["phenotype"]]))
      phenotypes = setdiff(all_phenotypes, c("", "nonaff"))
      phenoVector = phenotypes
      phenoTotal = length(phenotypes)
      # FLB indexes
      affected = !values[["pedToAdd"]][["phenotype"]] %in% c("", "nonaff")
      unknown = values[["pedToAdd"]][["phenotype"]] == ""
      proband = values[["pedToAdd"]][["proband"]] == 1
      carriers = values[["pedToAdd"]][["carrier"]] == "het"
      homozygous = values[["pedToAdd"]][["carrier"]] == "hom"
      noncarriers = values[["pedToAdd"]][["carrier"]] == "neg"
      # Colors
      pal = c("white", rep(c("#0072B2", "#D55E00", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7"), ceiling(phenoTotal/8)))
      names(pal) = c("nonaff", phenoVector)
      fillcols = pal[as.character(values[["pedToAdd"]][["phenotype"]])]
      
      idxs = which(values[["pedToAdd"]][["ped"]] == values[["pedToAddCurrent"]] + values[["pedTotal"]])
      plotSegregation(
        as.ped(values[["pedToAdd"]][idxs, c("id", "fid", "mid", "sex")]),
        affected = NULL,  # important to keep this, otherwise symbols may not be plotted correctly
        fill = unname(fillcols[idxs]),
        unknown = which(unknown[idxs]),
        proband = which(proband[idxs]),
        if (length(which(carriers[idxs]) > 0)) carriers = which(carriers[idxs]),
        if (length(which(homozygous[idxs]) > 0)) homozygous = which(homozygous[idxs]),
        if (length(which(noncarriers[idxs]) > 0)) noncarriers = which(noncarriers[idxs]),
        labs = setNames(seq_along(idxs), str_replace_na(values[["pedToAdd"]][["age"]][idxs], " ")),
        margins = c(2 + 3, 2, 2, 2)
      )
      legend(
        "bottomright",
        inset = c(-0.1, -0.15),
        legend = c("nonaff", phenoVector),
        fill = pal,
        ncol = phenoTotal + 1,
        bty = "n",
        cex = 1.2)
    }, execOnResize = TRUE)
    
    # Add pedigree
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["pedToAdd"]], {
      shinyalert(
        html = TRUE,
        text =
          tagList(
            div(
              "The following pedigree(s) will be added:",
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
            values[["pedData"]] = rbind(values[["pedData"]], values[["pedToAdd"]])
            values[["pedTotal"]] = max(values[["pedData"]][["ped"]])
            values[["lastProband"]] = values[["pedData"]][["proband"]]
            values[["pedCurrent"]] = values[["pedTotal"]]
          }
          values[["pedToAdd"]] = NULL
          pedToAdd$suspend()
        }
      )
      if (length(unique(values[["pedToAdd"]][["ped"]])) > 1)
        pedToAdd$resume()
    })
    # Cycle plots
    pedToAdd = observe(suspended = TRUE, {
      invalidateLater(2000)
      isolate({
      if (values[["pedToAddCurrent"]] < length(unique(values[["pedToAdd"]][["ped"]])))
        values[["pedToAddCurrent"]] = values[["pedToAddCurrent"]] + 1
      else
        values[["pedToAddCurrent"]] = 1
      })
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
        values[["phenoVector"]] = unique(c(phenotypes, values[["extraPheno"]]))
        values[["phenoTotal"]] = length(values[["phenoVector"]])
        
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
        values[["extraPheno"]] = NULL
        values[["phenoVector"]] = NULL
        values[["phenoTotal"]] = 0
        values[["pedTotal"]] = 0
      }
    })
    
  })
}
