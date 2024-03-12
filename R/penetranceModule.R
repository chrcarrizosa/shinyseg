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

w_mode = function(id)
  popover(
    radioGroupButtons(
      inputId = NS(id, "mode"),
      label = NULL,
      choices = c("Relative risk" = "rrisk", "Liability class" = "lclass"),
      justified = TRUE,
      individual = TRUE,
      status = "gray"
    ),
    title = NULL,
    content = "Switch between penetrance modes."
  )

w_inheritance = function(id)
  popover(
    radioGroupButtons(
      inputId = NS(id, "inheritance"),
      label = NULL,
      choices = c("AD", "AR", "AI", "XD", "XR", "XI"),
      justified = TRUE,
      individual = TRUE,
      status = "gray"
    ),
    title = NULL,
    content = "Select the inheritance pattern."
  )

w_loadLclass = function(id)
  popover(
    fileInput2(
      inputId = NS(id, "loadLclass"),
      label = "Load file",
      labelIcon = "file-import",
      progress = FALSE,
      divClass = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
    ),
    title = NULL,
    content = "Upload a liability class table."
  )

w_addLclass = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "addLclass"),
      label = "ADD",
      style = "jelly",
      size = "xs",
      icon = icon("plus")
    ),
    title = NULL,
    content = "Add another liability class."
  )

w_rmvLclass = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "rmvLclass"),
      label = "RMV",
      style = "jelly",
      size = "xs",
      icon = icon("minus")
    ),
    title = NULL,
    content = "Delete the last liability class."
  )

w_assistant = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "assistant"),
      label = "Assistant",
      style = "jelly",
      size = "s",
      icon = icon("circle-info")
    ),
    title = NULL,
    content = "Optimize model parameters from incidence data."
  )

w_plotType = function(id)
  popover(
    pickerInput(
      inputId = NS(id, "plotType"),
      label = HTML("<i class='fa fa-chart-line'></i> Plot"),
      choices = c("Cumulative incidence" = "CI", "Hazard" = "Hz", "Hazard ratio" = "HR"),
      selected = "HR",
      multiple = FALSE,
      options = list(
        `style` = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
      )
    ),
    title = NULL,
    content = "Change summary to display."
  )

w_polDegree = function(id)
  popover(
    pickerInput(
      inputId = NS(id, "polDegree"),
      label = HTML("<i class='fa fa-wave-square'></i> Splines"),
      choices = c("Linear" = 1, "Quadratic" = 2, "Cubic" = 3),
      selected = 2,
      multiple = FALSE,
      options = list(
        `style` = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
      )
    ),
    title = NULL,
    content = "Change the spline basis degree. Only relevant for age-dependent hazard ratios."
  )

w_extraPheno = function(id)
  popover(
    selectizeInput(
      inputId = NS(id, "extraPheno"),
      label = HTML("<i class='fa fa-plus'></i> Extra phenotypes"),
      choices = "",
      multiple = TRUE,
      options =
        list(
          create = TRUE,
          createOnBlur = TRUE,
          createFilter = I("/^(?!(nonaff)$).*$/"),
          plugins = list("remove_button")
        )
    ),
    title = NULL,
    content = "Add extra phenotypes associated with the variant."
  )

# Module code -------------------------------------------------------------

penetranceBoxUI = function(id) {
  div(
    id = NS(id, "box"),
    box(
      width = 12,
      title = 
        div(
          div(
            class = "leftcolumn flexcontainer",
            "Penetrance",
            div(w_mode(id), style = "margin-left: 1rem;"),
            div(w_inheritance(id), style = "margin-left: 1rem;")
          ),
          div(
            class = "rightcolumn",
            w_help(id)
          )
        ),
      hidden(
        div(
          id = NS(id, "rriskBox"),
          fluidRow(
            div(w_assistant(id)),
            div(
              class = "inline inlinetext inlinetext1",
              style = "margin-left: 1rem;",
              w_plotType(id)
            )
          ),
          div(rHandsontableOutput(NS(id, "rrisktable")), style = "margin-top: 2rem;"),
          div(plotlyOutput(NS(id, "rriskPlot"), height = "250px"), style = "margin-top: 2rem;"),
          fluidRow(
            div(
              class = "inline inlinetext inlinetext2",
              style = "margin-left: 1rem; margin-top: 1rem;",
              w_polDegree(id),
            ),
            div(
              class = "inline inlinetext inlinetext2",
              w_extraPheno(id),
              style = "margin-left: 1.5rem;"
            )
          )
        )
      ),
      
      div(
        id = NS(id, "lclassBox"),
        fluidRow(
          div(w_loadLclass(id)),
          div(w_addLclass(id), style = "margin-top: 0.2rem;"),
          div(w_rmvLclass(id), style = "margin-top: 0.2rem;")
        ),
        div(rHandsontableOutput(NS(id, "lclasstable")), style = "margin-top: 1rem;")
      ),
      collapsible = FALSE
    )
  )
}

penetranceBoxServer = function(id, values) {
  
  # Nested modules
  assistantModalServer("assistant", values)
  
  moduleServer(id, function(input, output, session) {
    
    # Default values
    values[["lclassData"]] = 
      data.table(
        f0 = 0.1,
        f1 = 0.8,
        f2 = 0.8,
        sex = NA_character_,
        phenotype = NA_character_,
        ages = NA_character_
      )
    values[["lclassCols"]] = 3
    values[["lclassNames"]] = 
      list(
        param = c("f0", "f1", "f2"),
        other = c("sex", "phenotype", "ages")
      )

    # Help
    observeEvent(input$help, {
      shinyalert(
        className = "helpbox",
        html = TRUE,
        text = read_file("help/penetranceBox.html"),
        animation = "slide-from-bottom",
        showConfirmButton = FALSE,
        closeOnClickOutside = TRUE,
        size = "m"
      )
    })
    
    # Mode changes
    observeEvent(priority = 2, input$mode, {
      
      toggleElement("lclassBox")
      toggleElement("rriskBox")
      
      switch(
        input$mode,
        "rrisk" = {
          values[["mode"]] = "rrisk"
          updateRadioGroupButtons(
            inputId = "inheritance",
            disabledChoices = c("AI", "XI")
          )
        },
        "lclass" = {
          values[["mode"]] = "lclass"
          updateRadioGroupButtons(
            inputId = "inheritance",
            disabledChoices = NULL
          )
        })
    })
    
    # Inheritance UI changes
    observeEvent(priority = 1, input$inheritance, {
      message("Inheritance changes")
      values[["inheritance"]] = input$inheritance
      switch(
        input$inheritance,
        "AD" = {
          values[["chrom"]] = "auto"
          values[["rriskNames"]] = c("neg\nrisk", "neg\nmean", "neg\nSD", "het/hom\nrisk")
          values[["lclassNames"]][["param"]] = c("f0" = "neg\nrisk", "f2" = "het/hom\nrisk")
          values[["lclassCols"]] = 2
          suppressWarnings(values[["lclassData"]][, f1 := NULL])
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = NULL
          )
        },
        "XD" = {
          values[["chrom"]] = "x"
          values[["rriskNames"]] = c("neg\nrisk", "neg\nmean", "neg\nSD", "het/hom\nrisk")
          values[["lclassNames"]][["param"]] = c("f0" = "neg\nrisk", "f2" = "het/hom\nrisk")
          values[["lclassCols"]] = 2
          suppressWarnings(values[["lclassData"]][, f1 := NULL])
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = NULL
          )
        },
        "AR" = {
          values[["chrom"]] = "auto"
          values[["rriskNames"]] = c("neg/het\nrisk", "neg/het\nmean", "neg/het\nSD", "hom\nrisk")
          values[["lclassNames"]][["param"]] = c("f0" = "neg/het\nrisk", "f2" = "hom\nrisk")
          values[["lclassCols"]] = 2
          suppressWarnings(values[["lclassData"]][, f1 := NULL])
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = NULL
          )
        },
        "XR" = {
          values[["chrom"]] = "x"
          values[["rriskNames"]] = c("neg/♀het\nrisk", "neg/♀het\nmean", "neg/♀het\nSD", "♂het/hom\nrisk")
          values[["lclassNames"]][["param"]] = c("f0" = "neg/♀het\nrisk", "f2" = "♂het/hom\nrisk")
          values[["lclassCols"]] = 2
          suppressWarnings(values[["lclassData"]][, f1 := NULL])
          updateRadioGroupButtons(
            inputId = "penetrance_mode",
            disabledChoices = NULL
          )
        },
        "AI" = {
          values[["chrom"]] = "auto"
          values[["rriskNames"]] = NULL
          values[["lclassNames"]][["param"]] = c("f0" = "neg\nrisk", "f1" = "het\nrisk", "f2" = "hom\nrisk")
          if (values[["lclassCols"]] == 2) {
            values[["lclassCols"]] = 3
            setcolorder(values[["lclassData"]][, f1 := NA_real_], c("f0", "f1", "f2", "sex", "phenotype", "ages"))
          }
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = "rrisk"
          )
        },
        "XI" = {
          values[["chrom"]] = "x"
          values[["rriskNames"]] = NULL
          values[["lclassNames"]][["param"]] = c("f0" = "neg\nrisk", "f1" = "het\nrisk", "f2" = "hom\nrisk")
          if (values[["lclassCols"]] == 2) {
            values[["lclassCols"]] = 3
            setcolorder(values[["lclassData"]][, f1 := NA_real_], c("f0", "f1", "f2", "sex", "phenotype", "ages"))
          }
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = "rrisk"
          )
        }
      )
    })
    
    # Remove homozygous males if X-linked
    observeEvent(priority = 1, c(values[["pedData"]][["carrier"]], input$inheritance), {
      req(values[["pedData"]], input$inheritance %in% c("XR", "XD", "XI"))
      message("X-linked model changes")
      to_change = which(values[["pedData"]][["carrier"]] == "hom" & values[["pedData"]][["sex"]] == 1)
      newcarrier = values[["pedData"]][["carrier"]]
      newcarrier[to_change] = "het"
      values[["pedData"]][["carrier"]] = newcarrier
    })
    
    # (rrisk) Phenotype table
    output$rrisktable = renderRHandsontable({
      req(values[["phenoData"]])
      rhandsontable(
        values[["phenoData"]],
        digits = 14,
        useTypes = TRUE,
        manualColumnResize = TRUE,
        rowHeaders = NULL,
        height = if (nrow(values[["phenoData"]]) > 6) 175 else NULL,
        colHeaders = c("sex", "phenotype", values[["rriskNames"]], "hazard ratio(s)"),
        overflow = "visible"
      ) %>%
        hot_col(1, type = "dropdown", source = c("both", "male", "female"), allowInvalid = FALSE, colWidths = "75px") %>%
        hot_col(2, readOnly = TRUE, colWidths = "110px") %>% 
        hot_col(c(3, 6), format = "0,0000") %>% 
        hot_validate_numeric(c(3, 6), min = 0, max = 0.9999, allowInvalid = FALSE) %>%
        hot_validate_numeric(4, min = 0.0001, max = 1000, allowInvalid = FALSE) %>%
        hot_validate_numeric(5, min = 5, max = 1000, allowInvalid = FALSE) %>%
        hot_col(7, type = "autocomplete", colWidths = "130px") %>%
        hot_table(highlightRow = TRUE, contextMenu = FALSE) %>%
        onRender(
          "function(el) {
          var hot = this.hot;
          disablePaste(hot);
          disableFill(hot);
        }")
    })
    
    # (rrisk) Update phenotype data from table edits
    observeEvent(input$rrisktable$changes$changes, {
      before = input$rrisktable$changes$changes[[1]][[3]]
      after = input$rrisktable$changes$changes[[1]][[4]]
      if (is.null(before) || before != after) {
        message("Update phenotype data from table edits")
        temp = hot_to_r(input$rrisktable)
        
        # Check for modified row/column
        row_index = input$rrisktable$changes$changes[[1]][[1]] + 1
        col_index = input$rrisktable$changes$changes[[1]][[2]]
        if (col_index == 0) { # sex
          pheno = as.character(temp[row_index, "phenotype"])
          
          if (after == "both") # removes the other sex
            temp = temp[!(phenotype == pheno & sex != "both")]
          else {
            if (before == "both") { # expands
              temp[, sexList := .(list(list(as.character(sex)))), by = 1:nrow(temp)]
              temp[phenotype == pheno, sexList := .(list(list("male", "female")))]
              temp = temp[, .(sex = unlist(sexList)), by = .(phenotype, f0R, f0mu, f0sigma, f2R, HR)]
              setcolorder(temp, c("sex", "phenotype", "f0R", "f0mu", "f0sigma", "f2R", "HR"))
            }
            else { # swap values
              swap = copy(temp[phenotype == pheno, ])
              temp[phenotype == pheno, ] = swap[2:1, ][, sex := factor(c("female", "male"), levels = c("both", "male", "female"))]
            }
          }
        }
        else if (col_index %in% 2:5) { # f0R, f0mu, f0sigma, f2R -> re-adjust HRs
          with(temp[row_index, ], {
            if (is.na(f0R)) {
              f0R = 0
              temp[row_index, f0R := 0]
            }

            if (is.na(f2R) || f0R >= f2R) # set HR to 1
              temp[row_index, HR := "1"]
            
            else if (!is.na(f0R) && !is.na(f0mu) && !is.na(f0sigma)) { # recalculate HRs
              f0CI = f0R*ptrunc(1:100, "norm", mean = f0mu, sd = f0sigma, a = 0, b = 100)
              f0Hz = diff(c(0, -log(1 - unlist(f0CI))))
              logHR = log(as.numeric(strsplit(HR, ",")[[1]]))
              if (length(logHR) == 1)
                logHR = rep(logHR, 4)
              scaledHR = optimHR(f0Hz, f2R, logHR, values[["polDegree"]])
              
              # undo if scaled HR cannot be calculated
              if(any(is.na(scaledHR))) {
                showNotification(
                  HTML("<i class='fas fa-triangle-exclamation'></i> Hazard ratio(s) could not be calculated. Try with other values."),
                  type = "error",
                  duration = 5
                )
                values[["phenoData"]][row_index, (col_index + 1) := NA_real_]  # to re-render table
                temp[row_index, (col_index + 1) := before]
              }
              else{
                if (diff(range(scaledHR)) == 0)
                  temp[row_index, HR := as.character(round(scaledHR[1], 2))]
                else
                  temp[row_index, HR := paste(round(scaledHR, 2), collapse = ", ")]
              }
            }
          })
        }
        else if (col_index == 6) { # HRs -> check input validity
          scaledHR = as.numeric(strsplit(temp[row_index, HR], ",")[[1]])
          scaledHR = ifelse(scaledHR >= 1, scaledHR, NA)
          df = length(scaledHR)
          values[["phenoData"]][row_index, HR := NA_character_] # re-render table
          if (df == 0) # set to 1 if deleted
            scaledHR = 1
          else if (df > 10 || any(is.na(scaledHR))) # undo if wrongly specified
            scaledHR = before
          else if (df %in% 2:3) # re-adjust if length is 2 or 3
            scaledHR = rep(scaledHR, each = 2)
          
          temp[row_index, HR := paste(scaledHR, collapse = ", ")]
        }
        
        values[["phenoData"]] = temp
      }
    })
    
    # (rrisk) Update phenotype table
    observeEvent(ignoreNULL = FALSE, values[["phenoVector"]], {
      message("Update phenotype table")
      if (length(values[["phenoVector"]]) > 0) {
        phenoDataOld = values[["phenoData"]]
        phenotypesOld = intersect(phenoDataOld$phenotype, values[["phenoVector"]])
        phenoDataNew = phenoDataOld[phenoDataOld$phenotype %in% phenotypesOld, ] # keep current
        phenotypesNew = setdiff(values[["phenoVector"]], phenotypesOld)
        if (length(phenotypesNew) > 0) {
          new_rows = data.table(
            sex = factor("both", levels = c("both", "male", "female")),
            phenotype = phenotypesNew,
            f0R = 0.5, f0mu = 50, f0sigma = 10,
            f2R = 0.5, HR = "1"
          )
          phenoDataNew = rbind(phenoDataNew, new_rows)
        }
        # Rearrange rows (Otherwise the order can be different due to automated removal of extra phenotypes)
        phenoDataNew[, pheno_order := match(phenotype, values[["phenoVector"]])]
        phenoDataNew = phenoDataNew[order(pheno_order, sex)]
        phenoDataNew[, pheno_order := NULL]
        values[["phenoData"]] = phenoDataNew
      }
      else {
        values[["phenoData"]] = NULL
      }
    })
    
    # (rrisk) Compute penetrances
    observeEvent(c(input$mode, values[["phenoData"]], values[["polDegree"]]), {
      req(input$mode == "rrisk", values[["phenoTotal"]] > 0, values[["phenoData"]], input$mode == values[["mode"]])
      
      for (i in 6:7)
        hideElement(paste0("message", i), asis = TRUE)
      
      message("Expanding phenotype table")
      
      fBase = copy(values[["phenoData"]])
      fBase[, rowid := seq(.N)]
      notNA = complete.cases(fBase[, !"f2R"])
      fBase = fBase[notNA, ]
      
      # Compute incidences
      if (any(notNA)) {
        fBase[, logHR := list(list(log(as.numeric(strsplit(HR, ",")[[1]])))), by = .(rowid)]
        fBase[, logHR := lapply(fBase[["logHR"]], function(i) if (length(i) == 1) rep(i, 4) else i)]
        rriskPlot = copy(fBase)
        rriskPlot[, f0CI := list(list(f0R*ptrunc(1:100, "norm", mean = f0mu, sd = f0sigma, a = 0, b = 100))), by = .(rowid)]
        rriskPlot[, f0Hz := list(list(diff(c(0, -log(1 - unlist(f0CI)))))), by = .(rowid)]
        rriskPlot[, f2Hz := list(list(getf2Hz(unlist(f0Hz), unlist(logHR), values[["polDegree"]]))), by = .(rowid)]
        rriskPlot[, f2CI := list(list(1 - exp(-cumsum(unlist(f2Hz))))), by = .(rowid)]
        rriskPlot = rriskPlot[, list(age = 1:100,
                                     f0Hz = unlist(f0Hz),
                                     f2Hz = unlist(f2Hz),
                                     f0CI = unlist(f0CI),
                                     f2CI = unlist(f2CI)), by = .(sex, phenotype)]
        
        # Re-adjust f2 lifetime risks
        phenoData = copy(values[["phenoData"]]) # to re-render table (makes this observer run twice, but needed to update f2R after changing polDegree)
        phenoData[rriskPlot[age == 100, ], f2R := round(i.f2CI, 4), on = c("sex", "phenotype")]
        values[["phenoData"]] = phenoData
        
        # Expand sex (here so the plot can be faceted)
        rriskPlot[, sex := as.character(sex)]
        rriskPlot[, sexList := .(list(list(sex))), by = .(sex, phenotype, age, f0Hz, f2Hz, f0CI, f2CI)]
        rriskPlot[sex == "both", sexList := .(list(list("male", "female")))]
        rriskPlot = rriskPlot[, .(sex = unlist(sexList)), by = .(phenotype, age, f0Hz, f2Hz, f0CI, f2CI)]
        rriskPlot[, sex := factor(sex, levels = c("male", "female"))]
      }
      else {
        rriskPlot = NULL
        fBase = NULL
      }
      
      if (!all(notNA)) {
        showElement("message8", asis = TRUE)
        fBase = NULL
      }
      else {
        hideElement("message8", asis = TRUE)
        fBase = melt(rriskPlot,
                     measure.vars = list(c("f0Hz", "f2Hz"), c("f0CI", "f2CI")),
                     value.name = c("Hz", "CI"))
        fBase[, variable := factor(variable, levels = 1:2, labels = c("f0", "f2"))]
        
        # Survival penetrances
        fBase[, nonaff := sum(Hz), by = .(age, sex, variable)]
        fBase[, nonaff := 1-exp(-cumsum(nonaff)), by = .(sex, phenotype, variable)]
        fBase[, sp := shift(1 - nonaff, fill = 1) * Hz, by = .(sex, phenotype, variable)]
        faff = dcast(fBase, sex + phenotype + age ~ variable, value.var = "sp")
        fnonaff = dcast(fBase, sex + age ~ variable, value.var = "nonaff", subset = .(phenotype == values[["phenoVector"]][1]))
        fnonaff[, phenotype := "nonaff"]
        fBase = rbind(faff, fnonaff)
      }
      
      values[["rriskPlot"]] = rriskPlot
      values[["fBase"]] = fBase
      session$sendCustomMessage(type = "update-dropdown", message = "NULL")
    })
    
    # (rrisk) CI, Hz, HR plot
    output$rriskPlot = renderPlotly({
      req(input$plotType, values[["rriskPlot"]], values[["phenoTotal"]] > 0)
      
      rriskPlot = highlight_key(values[["rriskPlot"]], ~phenotype)
      
      switch(
        input$plotType,
        "HR" = {
          g = ggplot(rriskPlot) +
            geom_line(aes(x = age, y = f2Hz/f0Hz, color = phenotype, group = phenotype), linewidth = .5) +
            labs(y = "Hazard ratio")
        },
        "CI" = {
          g = ggplot(rriskPlot) +
            geom_line(aes(x = age, y = f0CI, color = phenotype, group = phenotype), linewidth = .5, linetype = "42") +
            geom_line(aes(x = age, y = f2CI, color = phenotype, group = phenotype), linewidth = .5, linetype = "solid") +
            labs(y = "Cumulative incidence")
        },
        "Hz" = {
          g = ggplot(rriskPlot) +
            geom_line(aes(x = age, y = f0Hz, color = phenotype, group = phenotype), linewidth = .5, linetype = "42") +
            geom_line(aes(x = age, y = f2Hz, color = phenotype, group = phenotype), linewidth = .5, linetype = "solid") +
            labs(y = "Hazard")
        }
      )
      g = g +
        facet_grid(~ sex, scales = "fixed", drop = FALSE) +
        # Color palette (Okabe-Ito colors, without black)
        scale_color_manual(values = rep(c("#0072B2", "#D55E00", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7"), ceiling(values[["phenoTotal"]]/7)), breaks = values[["phenoVector"]]) +
        theme_bw() +
        theme(
          strip.background = element_rect(fill = "#f4f6f9", color = "#4e4e4e"),
          panel.border = element_rect(color = "#4e4e4e"),
          # axis.line = element_rect(color = "#4e4e4e"),
          panel.spacing = unit(2, "line"),
          legend.position = "none",
          axis.title.x.bottom = element_text(hjust = 0),
          text = element_text(family = "helvetica")
        )
      ggplotly(g, tooltip = c("x", "y")) %>%
        # layout(hovermode = "x unified") %>%
        highlight(on = "plotly_click", off = "plotly_doubleclick") %>%
        config(displayModeBar = FALSE)
    })
    
    #  (rrisk) Assistant pop-up
    observeEvent(input$assistant, {
      
      # Update choice list
      values[["assisChoices"]] = NULL
      choiceList = seq(nrow(values[["phenoData"]]))
      nameList = apply(values[["phenoData"]][, .(sex, phenotype)], 1, paste, collapse = " | ")
      names(choiceList) = nameList
      values[["assisChoices"]] = choiceList
      
      # Pop-up
      showModal(
        assistantModalUI("assistant")
      )
      
    })
    
    # (rrisk) Basis degree
    observeEvent(priority = 1, input$polDegree, {
      message("Updating spline basis degree")
      values[["polDegree"]] = input$polDegree
    })
    
    # (rrisk) Extra phenotypes
    observeEvent(ignoreInit = TRUE, ignoreNULL = FALSE, input$extraPheno, {
      message("Updating extra phenotypes")
      values[["extraPheno"]] = input$extraPheno
      all_phenotypes = levels(droplevels(values[["pedData"]][["phenotype"]]))
      phenotypes = setdiff(all_phenotypes, c("", "nonaff"))
      values[["phenoVector"]] = unique(c(phenotypes, values[["extraPheno"]]))
      values[["phenoTotal"]] = length(values[["phenoVector"]])
    })
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["pedData"]][["phenotype"]], {
      message("Disabling pedigree phenotypes")
      all_phenotypes = levels(droplevels(values[["pedData"]][["phenotype"]]))
      values[["extraPheno"]] = setdiff(values[["extraPheno"]], all_phenotypes)
      updateSelectizeInput(
        inputId = "extraPheno",
        choices = c("", values[["extraPheno"]]),
        selected = values[["extraPheno"]],
        options =
          list(
            create = TRUE,
            createOnBlur = TRUE,
            createFilter = I(paste0("/^(?!(nonaff|", paste(all_phenotypes, collapse = "|"), ")$).*$/")),
            plugins = list("remove_button")
          )
      )
    })
    
    # (lclass) Lclass table
    output$lclasstable = renderRHandsontable({
      req(values[["lclassData"]])
      rhandsontable(
        values[["lclassData"]],
        digits = 14,
        useTypes = TRUE,
        manualColumnResize = TRUE,
        rowHeaders = NULL,
        height = if (nrow(values[["lclassData"]]) > 6) 175 else NULL,
        colHeaders = unname(unlist(values[["lclassNames"]])),
        overflow = "visible"
      ) %>%
        hot_table(colWidths = "75px") %>%
        hot_col(1:values[["lclassCols"]], format = "0,0000000") %>%
        # hot_validate_numeric(1:values[["lclassCols"]], min = 0, max = 1, allowInvalid = FALSE) %>%
        hot_col("sex", type = "dropdown", source = c("", "both", "male", "female"), allowInvalid = FALSE) %>%
        hot_col(values[["lclassCols"]] + 2, colWidths = "110px") %>% 
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
    
    # (lclass) Update lclass data from table edits
    observeEvent(input$lclasstable, {
      message("Update liability classes from table edits")
      lclassData = hot_to_r(input$lclasstable)
      values[["lclassData"]] = lclassData
    })
    # (lclass) Add/rmv lclasses
    observeEvent(input$addLclass, {
      values[["lclassData"]] = rbind(values[["lclassData"]], values[["lclassData"]][NA])
    })
    observeEvent(input$rmvLclass, {
      req(nrow(values[["lclassData"]]) > 1)
      values[["lclassData"]] = values[["lclassData"]][1:(nrow(values[["lclassData"]]) - 1),]
    })
    
    # (lclass) Read table from file
    observeEvent(input$loadLclass, {
      lclassToAdd = 
        suppressWarnings(
          tryCatch(
            {
              lclassToAdd = read.table(input$loadLclass$datapath, header = FALSE)
              if(values[["lclassCols"]] == 2) {
                lclassToAdd = as.data.table(lclassToAdd[, 1:5])
                colnames(lclassToAdd) = c("f0", "f2", "sex", "phenotype", "ages")
                lclassToAdd[, c("f0", "f2") := lapply(.SD, as.numeric), .SDcols = c("f0", "f2")]
                lclassToAdd[, f1 := NULL] # required
              }
              else {
                lclassToAdd = as.data.table(lclassToAdd[, 1:6])
                colnames(lclassToAdd) = c("f0", "f1", "f2", "sex", "phenotype", "ages")
                lclassToAdd[, c("f0", "f1", "f2") := lapply(.SD, as.numeric), .SDcols = c("f0", "f1", "f2")]
              }
              lclassToAdd[, sex := factor(sex, levels = c("", "both", "male", "female"))]
            },
            error = function(err) NULL
          )
        )
      if (is.null(lclassToAdd))
        showNotification(
          HTML("<i class='fas fa-triangle-exclamation'></i> Invalid liability class file."),
          type = "error",
          duration = 3
        )
      
      values[["lclassToAdd"]] = lclassToAdd
    })
    # (lclass) Display loaded table
    output$lclassToAdd = renderRHandsontable({
      req(values[["lclassToAdd"]])
      rhandsontable(
        values[["lclassToAdd"]],
        digits = 14,
        useTypes = TRUE,
        manualColumnResize = TRUE,
        rowHeaders = NULL,
        height = if (nrow(values[["lclassToAdd"]]) > 6) 175 else NULL,
        colHeaders = c(unname(values[["lclassNames"]][["param"]]), "sex", "phenotype", "ages"),
        # overflow = "visible",
        readOnly = TRUE
      ) %>%
        hot_table(colWidths = "75px") %>%
        hot_col(1:values[["lclassCols"]], format = "0,0000000") %>%
        # hot_validate_numeric(1:values[["lclassCols"]], min = 0, max = 1, allowInvalid = FALSE) %>%
        hot_col("sex", type = "dropdown", source = c("", "both", "male", "female"), allowInvalid = FALSE) %>%
        hot_col(values[["lclassCols"]] + 2, colWidths = "110px") %>% 
        hot_context_menu(
          highlightRow = TRUE,
          allowColEdit = FALSE
        )
    })
    # (lclass) Replace table
    observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["lclassToAdd"]], {
      shinyalert(
        html = TRUE,
        text = 
          tagList(
            div(
              div("The liability class table will be replaced with the following:", style = "margin-bottom: 10px;"),
              rHandsontableOutput(outputId = NS(id, "lclassToAdd"), height = "300px", width = "100%"),
              align = "center"
            )
          ),
        animation = "slide-from-bottom",
        confirmButtonCol = "#39A0ED",
        showCancelButton = TRUE,
        size = "s",
        callbackR = function(x) {
          if (x) {
            values[["lclassData"]] = values[["lclassToAdd"]]
          }
          values[["lclassToAdd"]] = NULL
        }
      )
    })
    
    # (lclass) Expand base table
    observeEvent(priority = 1, c(input$mode, values[["phenoVector"]], values[["lclassCols"]], values[["lclassData"]]), {
      req(input$mode == "lclass", input$mode == values[["mode"]])
      hideElement("message8", asis = TRUE)
      
      message("Expanding liability class table")
      
      fBase = copy(values[["lclassData"]])
      fBase[, rowid := seq(.N)]
      
      # Get age ranges
      fBase[ages == "" | is.na(ages), "ages"] = "1-100"
      fBase[, c("minAge", "maxAge") := data.table(str_split_fixed(ages, "-", 2))]
      suppressWarnings({
        fBase[, c("minAge", "maxAge") := lapply(.SD, as.integer), .SDcols = c("minAge","maxAge")]
      })
      # Check that they were correctly specified [1-100]-[1-100]
      if (!(all(unlist(fBase[, c("minAge", "maxAge")]) %in% 1:100) & all(fBase$minAge <= fBase$maxAge))) {
        showElement("message6", asis = TRUE)
        hideElement("message7", asis = TRUE)
        hideElement("message8", asis = TRUE)
        values[["lclassNames"]][["other"]][3] = "<i class='fa fa-triangle-exclamation fa-fw' style='color:#db1f48'></i>ages"
        fBase = NULL
      }
      else {
        hideElement("message6", asis = TRUE)
        values[["lclassNames"]][["other"]][3] = "ages"
        
        # Expand sex
        fBase[, sexList := .(list(list(as.character(sex)))), by = 1:nrow(fBase)]
        fBase[sex == "both" | sex == "" | is.na(sex), sexList := .(list(list("male", "female")))]
        fBase = fBase[, .(sex = unlist(sexList)), by = setdiff(names(fBase), c("ages", "sex", "sexList"))]
        
        # Expand phenotype
        fBase[, phenoList := .(list(list(phenotype))), by = 1:nrow(fBase)]
        fBase[phenotype == "" | is.na(phenotype), phenoList := .(list(list(c(values[["phenoVector"]], "nonaff"))))]
        fBase = fBase[, .(phenotype = unlist(phenoList)), by = setdiff(names(fBase), c("phenotype", "phenoList"))]
        
        # Expand age
        suppressWarnings({
          fBase = fBase[, .(f0, f1 = if ("f1" %in% names(fBase)) f1 else numeric(), f2, sex, phenotype, age = seq(minAge, maxAge), rowid), by = 1:nrow(fBase)]
        })
        
        # Check for class overlaps
        if (any(table(fBase[, .(sex, phenotype, age)]) > 1)) {
          showElement("message7", asis = TRUE)
          fBase = NULL
        }
        else
          hideElement("message7", asis = TRUE)
      }
      
      values[["fBase"]] = fBase
      session$sendCustomMessage(type = "update-dropdown", message = "NULL")
    })
    
    # Set f1 based on inheritance model
    observeEvent(c(values[["fBase"]], input$inheritance), {
      req(values[["fBase"]], input$inheritance == values[["inheritance"]])
      message("Setting f1 based on inheritance model")
      
      fFull = copy(values[["fBase"]])
      switch(
        input$inheritance,
        "AD" = ,
        "XD" = {fFull[, f1 := f2]},
        "AR" = {fFull[, f1 := f0]},
        "XR" = {
          fFull[, f1 := f0]
          fFull[sex == "male", f1 := f2]}
      )
      
      values[["fFull"]] = fFull
    })
    
    # Get final penetrances
    observeEvent(c(values[["fFull"]], values[["chrom"]], values[["lclasses"]]), {
      req(values[["fFull"]], values[["lclasses"]]) # values[["pedData"]],
      message("Get final penetrances")
      
      # Subset f based on needed lclasses
      fSubset = values[["fFull"]][values[["lclasses"]], on = c("sex", "phenotype", "age")]
      # Replace missing phenotype with 1,1,1
      fSubset[, c("f0", "f1", "f2") := lapply(.SD, function(x) fifelse(phenotype == "", 1, x)), .SDcols = c("f0", "f1", "f2")]
      
      # Get final penetrances
      if (values[["chrom"]] == "auto") {
        fSubset[, liability := sequence(.N)]
        f = fSubset[, c("f0", "f1", "f2")]
      }
      else {
        fSubset[, liability := sequence(.N), by = "sex"]
        f = list(male = fSubset[sex == "male", c("f0", "f1")],
                 female = fSubset[sex == "female", c("f0", "f1", "f2")])
        f = lapply(f, function(i) if (nrow(i) == 1) unlist(i) else i)  # Deal with single-row lclass table
      }
      
      # Check if there is any NA in f (ages in the pedTable take precedence)
      if (any(is.na(unlist(f))) & values[["okAge"]]) {
        showElement("message8", asis = TRUE)
        f = NULL
      }
      else
        hideElement("message8", asis = TRUE)
      
      values[["f"]] = f
      values[["liability"]] = fSubset[["liability"]]
      session$sendCustomMessage(type = "update-dropdown", message = "NULL")
    })
    
  })
}
