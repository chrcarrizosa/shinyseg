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

w_mode = function(id)
  radioGroupButtons(
    inputId = NS(id, "mode"),
    label = NULL,
    choices = c("Relative risk" = "rrisk", "Liability class" = "lclass"),
    justified = TRUE,
    individual = TRUE,
    status = "gray"
  )

w_inheritance = function(id)
  radioGroupButtons(
    inputId = NS(id, "inheritance"),
    label = NULL,
    choices = c("AD", "AR", "AI", "XD", "XR", "XI"),
    justified = TRUE,
    individual = TRUE,
    status = "gray"
  )

# w_loadLclass = function(id)
#   fileInput2(
#     inputId = NS(id, "loadLclass"),
#     label = "Load file",
#     labelIcon = "file-import",
#     progress = FALSE,
#     divClass = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
#   )

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

# w_assistant = function(id)
#   popover(
#     actionBttn(
#       inputId = NS(id, "assistant"),
#       label = "Assistant",
#       style = "jelly",
#       size = "s",
#       icon = icon("circle-info")
#     ),
#     title = NULL,
#     content = "Use assistant."
#   )

w_plotType = function(id)
  popover(
    pickerInput(
      inputId = NS(id, "plotType"),
      label = HTML("<i class='fa fa-chart-line'></i> Plot"),
      choices = c("Cumulative incidence" = "CI", "Hazard" = "Hz", "Hazard ratio" = "HR"),
      selected = "HR",
      multiple = TRUE,
      options = list(
        `max-options` = 1,
        `none-selected-text` = "Choose a parameter",
        `style` = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
      )
    ),
    title = NULL,
    content = "Change parameter to display."
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
            # div(w_assistant(id)),
            div(
              class = "inline inlinetext",
              style = "margin-left: 1rem;",
              w_plotType(id)
            )
          ),
          div(rHandsontableOutput(NS(id, "rrisktable")), style = "margin-top: 2rem;"),
          div(plotlyOutput(NS(id, "rriskPlot"), height = "250px"), style = "margin-top: 2rem;")
        )
      ),
      
      div(
        id = NS(id, "lclassBox"),
        fluidRow(
          # div(w_loadLclass(id)),
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
    
    # Help
    observeEvent(input$help, {
      shinyalert(
        className = "helpbox",
        html = TRUE,
        text = read_file("modals/penetranceBox.html"),
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
          values[["rriskNames"]] = c("neg\nrisk", "neg\nmean", "neg\nSD", "het/hom\nrisk", "het/hom\nHR")
          values[["lclassNames"]] = c("f0" = "neg\nrisk", "f2" = "het/hom\nrisk", "sex", "phenotype", "ages")
          values[["lclassCols"]] = 2
          suppressWarnings(values[["lclassData"]][, f1 := NULL])
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = NULL
          )
        },
        "XD" = {
          values[["chrom"]] = "x"
          values[["rriskNames"]] = c("neg\nrisk", "neg\nmean", "neg\nSD", "het/hom\nrisk", "het/hom\nHR")
          values[["lclassNames"]] = c("f0" = "neg\nrisk", "f2" = "het/hom\nrisk", "sex", "phenotype", "ages")
          values[["lclassCols"]] = 2
          suppressWarnings(values[["lclassData"]][, f1 := NULL])
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = NULL
          )
        },
        "AR" = {
          values[["chrom"]] = "auto"
          values[["rriskNames"]] = c("neg/het\nrisk", "neg/het\nmean", "neg/het\nSD", "hom\nrisk", "hom\nHR")
          values[["lclassNames"]] = c("f0" = "neg/het\nrisk", "f2" = "hom\nrisk", "sex", "phenotype", "ages")
          values[["lclassCols"]] = 2
          suppressWarnings(values[["lclassData"]][, f1 := NULL])
          updateRadioGroupButtons(
            inputId = "mode",
            disabledChoices = NULL
          )
        },
        "XR" = {
          values[["chrom"]] = "x"
          values[["rriskNames"]] = c("neg/♀het\nrisk", "neg/♀het\nmean", "neg/♀het\nSD", "♂het/hom\nrisk", "♂het/hom\nHR")
          values[["lclassNames"]] = c("f0" = "neg/♀het\nrisk", "f2" = "♂het/hom\nrisk", "sex", "phenotype", "ages")
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
          values[["lclassNames"]] = c("f0" = "neg\nrisk", "f1" = "het\nrisk", "f2" = "hom\nrisk", "sex", "phenotype", "ages")
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
          values[["lclassNames"]] = c("f0" = "neg\nrisk", "f1" = "het\nrisk", "f2" = "hom\nrisk", "sex", "phenotype", "ages")
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
        colHeaders = c("sex", "phenotype", values[["rriskNames"]], "spline\ncoefs"),
        overflow = "visible"
      ) %>%
        hot_col(1, type = "dropdown", source = c("both", "male", "female"), allowInvalid = FALSE, colWidths = "75px") %>%
        hot_col(2, readOnly = TRUE, colWidths = "110px") %>% 
        hot_col(c(3, 6), format = "0,0000") %>% 
        hot_validate_numeric(c(3, 6), min = 0.0001, max = 0.9999, allowInvalid = FALSE) %>%
        hot_validate_numeric(4, min = 0.0001, max = 1000, allowInvalid = FALSE) %>%
        hot_validate_numeric(5, min = 5, max = 1000, allowInvalid = FALSE) %>%
        hot_col(7, type = "dropdown", source = c("proportional", "converging", "diverging", "custom"), allowInvalid = FALSE) %>%
        hot_col(8, type = "autocomplete", colWidths = "90px") %>%
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
        
        # Check for modified column
        col_index = input$rrisktable$changes$changes[[1]][[2]]
        if (col_index == 0) { # sex
          row_index = input$rrisktable$changes$changes[[1]][[1]] + 1
          pheno = as.character(temp[row_index, "phenotype"])
          
          if (after == "both") # removes the other sex
            temp = temp[!(phenotype == pheno & sex != "both")]
          else {
            if (before == "both") { # expands
              temp[, sexList := .(list(list(as.character(sex)))), by = 1:nrow(temp)]
              temp[phenotype == pheno, sexList := .(list(list("male", "female")))]
              temp = temp[, .(sex = unlist(sexList)), by = .(phenotype, f0R, f0mu, f0sigma, f2R, f2HR, f2coef)]
              setcolorder(temp, c("sex", "phenotype", "f0R", "f0mu", "f0sigma", "f2R", "f2HR", "f2coef"))
            }
            else { # swap values
              swap = copy(temp[phenotype == pheno, ])
              temp[phenotype == pheno, ] = swap[2:1, ][, sex := factor(c("female", "male"), levels = c("both", "male", "female"))]
            }
          }
        } 
        if (col_index == 6) # hazard ratios
          temp[input$rrisktable$changes$changes[[1]][[1]] + 1,
               f2coef := fcase(after == "proportional", "1,1,1,1",
                               after == "diverging", "0,0,1,1",
                               after == "converging", "1,1,0,0",
                               default = f2coef)]
        if (col_index == 7) # basis coefficients
          temp[input$rrisktable$changes$changes[[1]][[1]] + 1,
               f2HR := fcase(after == "1,1,1,1", "proportional",
                             after == "0,0,1,1", "diverging",
                             after == "1,1,0,0", "converging",
                             default = "custom")]
        
        values[["phenoData"]] = temp
      }
    })
    
    # (rrisk) Update phenotype table
    observeEvent(ignoreNULL = FALSE, values[["phenoVector"]], {
      message("Update phenotype table")
      if(!is.null(values[["phenoVector"]])) {
        phenoDataOld = values[["phenoData"]]
        phenotypesOld = intersect(phenoDataOld$phenotype, values[["phenoVector"]])
        phenoDataNew = phenoDataOld[phenoDataOld$phenotype %in% phenotypesOld, ] # keep current
        phenotypesNew = setdiff(values[["phenoVector"]], phenotypesOld)
        if (length(phenotypesNew) > 0) {
          new_rows = data.table(
            sex = factor(rep(c("male", "female"), each = length(phenotypesNew)), levels = c("both", "male", "female")),
            phenotype = rep(phenotypesNew, 2),
            f0R = 0.5, f0mu = 50, f0sigma = 10,
            f2R = 0.5, f2HR = "proportional", f2coef = "1,1,1,1"
          )
          phenoDataNew = rbind(phenoDataNew, new_rows)
        }
        values[["phenoData"]] = as.data.table(phenoDataNew)
      }
      else {
        values[["phenoData"]] = NULL
      }
    })
    
    # (rrisk) Compute penetrances
    observeEvent(c(input$mode, values[["phenoData"]]), {
      req(input$mode == "rrisk", values[["phenoTotal"]] > 0, values[["phenoData"]], input$mode == values[["mode"]])
      
      for (i in 6:7)
        hideElement(paste0("message", i), asis = TRUE)
      
      message("Expanding phenotype table")
      
      fBase = copy(values[["phenoData"]])
      fBase[, rowid := seq(.N)]
      rowTotal = nrow(fBase)
      
      # Get spline coefficients, removing missing or invalid
      fBase[, coefs := list(list(suppressWarnings(as.numeric(strsplit(f2coef, ",")[[1]])))), by = .(rowid)]
      fBase[, coefs := lapply(fBase[["coefs"]], function(i) if (any(is.na(i) | i < 0)) NA else i)]
      fBase = na.omit(fBase)
      fBase[, df := length(unlist(coefs)), by = .(rowid)]
      fBase = fBase[df >= 4 & df <= 10, ]
      rowBase = nrow(fBase)
      
      # Compute incidences
      if (rowBase > 0) {
        rriskPlot = copy(fBase)
        rriskPlot[, f0CI := list(list(f0R*ptrunc(1:100, "norm", mean = f0mu, sd = f0sigma, a = 0, b = 100))), by = .(rowid)]
        rriskPlot[, f0Hz := list(list(diff(c(0, -log(1 - unlist(f0CI)))))), by = .(rowid)]
        rriskPlot[, f2Hz := list(list(optimHR(unlist(f0Hz), f2R, df, unlist(coefs)))), by = .(rowid)]
        rriskPlot[, f2CI := list(list(1 - exp(-cumsum(unlist(f2Hz))))), by = .(rowid)]
        rriskPlot = rriskPlot[, list(age = 1:100,
                                     f0Hz = unlist(f0Hz),
                                     f2Hz = unlist(f2Hz),
                                     f0CI = unlist(f0CI),
                                     f2CI = unlist(f2CI)), by = .(sex, phenotype)]
        
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
      
      if (rowBase < rowTotal) {
        showElement("message8", asis = TRUE)
        hideElement("message9", asis = TRUE)
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
        faff[f0 > f2, f2 := f0] # fix phi > beta
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
        scale_color_manual(values = rep(c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF"), ceiling(values[["phenoTotal"]]/5)), breaks = values[["phenoVector"]]) +
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
        colHeaders = unname(values[["lclassNames"]]),
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
        hideElement("message9", asis = TRUE)
        values[["lclassNames"]][values[["lclassCols"]] + 3] = "<i class='fa fa-triangle-exclamation fa-fw' style='color:#db1f48'></i>ages"
        fBase = NULL
      }
      else {
        hideElement("message6", asis = TRUE)
        values[["lclassNames"]][values[["lclassCols"]] + 3] = "ages"
        
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
          hideElement("message9", asis = TRUE)
          fBase = NULL
        }
        else
          hideElement("message7")
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
        showElement("message9", asis = TRUE)
        f = NULL
      }
      else
        hideElement("message9", asis = TRUE)
      
      values[["f"]] = f
      values[["liability"]] = fSubset[["liability"]]
      session$sendCustomMessage(type = "update-dropdown", message = "NULL")
    })
    
  })
}
