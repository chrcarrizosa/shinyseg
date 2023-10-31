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

w_calculate = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "calculate"),
      label = "Calculate",
      style = "jelly",
      size = "s",
      icon = icon("calculator")
    ),
    title = NULL,
    content = "Calculate and display the Bayes factor value."
  )

w_sensitivity = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "sensitivity"),
      label = "Sensitivity",
      style = "jelly",
      size = "s",
      icon = icon("rocket")
    ),
    title = NULL,
    content = "Perform sensitivity analyses."
  )

w_senV1 = function(id)
  pickerInput(
    inputId = NS(id, "senV1"),
    label = NULL,
    choices = NULL,
    multiple = TRUE,
    selected = NULL,
    options = list(
      `icon-base` = "fa-solid fa-fw",
      "max-options" = 1,
      "none-selected-text" = "Select 1st parameter",
      "live-search" = TRUE,
      `style` = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
    )
  )

w_senX1 = function(id)
  sliderInput(
    inputId = NS(id, "senX1"),
    label = NULL,
    min = 0,
    max = 1,
    step = 1,
    value = c(0, 1),
    ticks = FALSE
  )

w_senV2 = function(id)
  pickerInput(
    inputId = NS(id, "senV2"),
    label = NULL,
    choices = NULL,
    multiple = TRUE,
    selected = NULL,
    options = list(
      `icon-base` = "fa-solid fa-fw",
      "max-options" = 1,
      "none-selected-text" = "Select 2nd parameter",
      "live-search" = TRUE,
      `style` = "action-button bttn bttn-jelly bttn-sm bttn-default bttn-no-outline shiny-bound-input"
    )
  )

w_senX2 = function(id)
  sliderInput(
    inputId = NS(id, "senX2"),
    label = NULL,
    min = 0,
    max = 1,
    step = 1,
    value = c(0, 1),
    ticks = FALSE
  )

w_plot = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "plot"),
      label = "Plot",
      style = "jelly",
      size = "s",
      icon = icon("chart-area")
    ),
    title = NULL,
    content = "Display contour plot."
  )

# Module code -------------------------------------------------------------

bayesBoxUI = function(id) {
  div(
    id = NS(id, "box"),
    box(
      width = 12,
      title = 
        div(
          div(
            class = "leftcolumn",
            "FLB"
          ),
          div(
            class = "rightcolumn",
            w_help(id)
          )
        ),
      collapsible = FALSE,
      fluidRow(
        verbatimTextOutput(NS(id, "flb")),
        div(w_calculate(id), style = "margin-left: 1rem;"),
        div(w_sensitivity(id)),
        div(w_plot(id))
      ),
      div(plotOutput(NS(id, "colorbar"), height = "60px"), style = "margin-top: -0.5rem;"),
      hidden(
        div(
          id = NS(id, "sensBox"),
          style = "margin-top: 2.5rem;",
          fluidRow(
            div(w_senV1(id), style = "width: 235px;"),
            div(w_senX1(id), style = "width: 75px; margin-left: 1rem;")
          ),
          fluidRow(
            div(w_senV2(id), style = "width: 235px;"),
            div(w_senX2(id), style = "width: 75px; margin-left: 1rem;")
          ),
          plotOutput(NS(id, "contourplot"), height = "300px")
        )
      )
    )
  )
}

bayesBoxServer = function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Help
    observeEvent(input$help, {
      shinyalert(
        className = "helpbox",
        html = TRUE,
        text = read_file("modals/bayesBox.html"),
        animation = "slide-from-bottom",
        showConfirmButton = FALSE,
        closeOnClickOutside = TRUE,
        size = "m"
      )
    })
    
    # Calculate FLB
    observeEvent(input$calculate, {
      message("Calculate BF")
      flb = 
        sapply(1:values[["pedTotal"]], function(pedid) {
          idxs = which(values[["pedData"]][["ped"]] == pedid)
          tryCatch(
            FLB(
              x = as.ped(values[["pedData"]][idxs, c("id", "fid", "mid", "sex")]),
              affected = which(values[["affected"]][idxs]),
              unknown = which(values[["unknown"]][idxs]),
              proband = which(values[["proband"]][idxs]),
              if (length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
              if (length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
              if (length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs]),
              freq = 10^values[["afreq"]],
              penetrances = values[["f"]],
              liability = values[["liability"]][idxs],
              Xchrom = ifelse(values[["chrom"]] == "x", TRUE, FALSE),
              details = FALSE
            ),
            error = function(err) NA)
        })
      flb = prod(flb)
      
      if (!is.na(flb)) {
        values[["flb"]] = flb
        showElement("sensitivity")
      }
      else {
        values[["flb"]] = NULL
        hideElement("sensitivity")
        showNotification(
          HTML("<i class='fas fa-triangle-exclamation'></i> FLB could not be calculated."),
          type = "error",
          duration = 3
        )
      }
      hideElement("plot")
      hideElement("sensBox")
    })
    
    # Hide if something changes
    observeEvent(
      c(values[["afreq"]],
        values[["mode"]],
        values[["inheritance"]],
        values[["pedData"]],
        values[["phenoData"]],
        values[["lclassData"]],
        values[["updateSession"]]), # needed in case the same example/report is reloaded
      handlerExpr = {
        req(values[["flb"]])
        values[["flb"]] = NULL
        hideElement("sensitivity")
        hideElement("plot")
        hideElement("sensBox")
      })
    
    # Display value and colorbar
    output$flb = renderText({
      values[["flb"]]
    })
    output$colorbar = renderPlot({
      BFplot(values[["flb"]], values[["pedTotal"]])
    })
    
    # Sensitivity analysis choices
    observeEvent(input$sensitivity, {
      choiceList = "afreq"
      nameList = "log10(MAF)"
      if (values[["mode"]] == "lclass") {
        lclassData = copy(values[["lclassData"]])
        lclassData[, rowid := seq(.N)]
        values[["lclassDataLong"]] = melt(lclassData, id.vars = c("rowid", "sex", "phenotype", "ages"), variable.name = "param")
        choiceList = c(choiceList, apply(values[["lclassDataLong"]][, c("param", "rowid")], 1, paste, collapse = "_"))
        nameList = c(nameList, apply(values[["lclassDataLong"]][, -c("rowid", "value"), with = FALSE], 1, function(x) paste(stri_remove_empty_na(x), collapse = " | ")))
        nameList = str_replace_all(nameList, values[["lclassNames"]][1:values[["lclassCols"]]])
      }
      else {
        phenoData = copy(values[["phenoData"]][, .(sex, phenotype, f0R, f0mu, f0sigma, f2R)])
        phenoData[, rowid := seq(.N)]
        values[["phenoDataLong"]] = melt(phenoData, id.vars = c("rowid", "sex", "phenotype"), variable.name = "param")
        choiceList = c(choiceList, apply(values[["phenoDataLong"]][, c("param", "rowid")], 1, paste, collapse = "_"))
        nameList = c(nameList, apply(values[["phenoDataLong"]][, c("sex", "phenotype", "param")], 1, paste, collapse = ", "))
        nameList = str_replace_all(nameList, setNames(values[["rriskNames"]][1:4], c("f0R", "f0mu", "f0sigma", "f2R")))
      }
      names(choiceList) = nameList
      values[["senChoices"]] = choiceList
      
      updatePickerInput(
        session = getDefaultReactiveDomain(),
        inputId = "senV1",
        choices = values[["senChoices"]],
        selected = NULL
      )
      hideElement("sensitivity")
      hideElement("plot")
      toggleElement("sensBox")
    })
    
    # Update when 1st variable is selected
    observeEvent(ignoreNULL = FALSE, input$senV1, {
      if (is.null(input$senV1)) {
        hideElement("senX1")
        hideElement("senV2")
        hideElement("senX2")
        hideElement("plot")
        hideElement("contourplot")
        values[["flbs"]] = NULL
      }
      else {
        # update slider X1
        senV1 = gsub("(.)|_.*", "\\1", input$senV1)
        slidervals = switch(
          senV1,
          "afreq" = c(-5, -1),
          "f0R" = c(0.0001, 0.50),
          "f0mu" = c(0.0001, 1000),
          "f0sigma" = c(5, 1000),
          "f2R" = c(0.50, 0.9999),
          "f0" = c(0.0001, 0.50),
          "f1" = c(0.0001, 0.9999),
          "f2" = c(0.50, 0.9999)
        )
        updateSliderInput(
          inputId = "senX1",
          min = slidervals[1],
          max = slidervals[2],
          step = (slidervals[2] - slidervals[1]) / 100,
          value = slidervals
        )
        showElement("senX1")
        
        # update choices V2
        updatePickerInput(
          session = getDefaultReactiveDomain(),
          inputId = "senV2",
          choices = values[["senChoices"]][-which(values[["senChoices"]] %in% input$senV1)],
          selected = NULL
        )
        showElement("senV2")
      }
    })
    
    # Update when 2nd variable is selected
    observeEvent(ignoreNULL = FALSE, input$senV2, {
      if (is.null(input$senV2)) {
        hideElement("senX2")
        hideElement("plot")
        hideElement("contourplot")
        values[["flbs"]] = NULL
      }
      else {
        # update slider X2
        senV2 = gsub("(.)|_.*", "\\1", input$senV2)
        slidervals = switch(
          senV2,
          "afreq" = c(-5, -1),
          "f0R" = c(0.0001, 0.50),
          "f0mu" = c(0.0001, 1000),
          "f0sigma" = c(5, 1000),
          "f2R" = c(0.50, 0.9999),
          "f0" = c(0.0001, 0.50),
          "f1" = c(0.0001, 0.9999),
          "f2" = c(0.50, 0.9999)
        )
        updateSliderInput(
          inputId = "senX2",
          min = slidervals[1],
          max = slidervals[2],
          step = (slidervals[2] - slidervals[1]) / 100,
          value = slidervals
        )
        showElement("senX2")
        showElement("plot")
        hideElement("contourplot")
        values[["flbs"]] = NULL
      }
    })
    
    # Contourplot computation
    observeEvent(input$plot, {
      message("Running sensitivity analysis")
      
      # Create grid of values
      vars = c(input$senV1, input$senV2)
      values[["grid"]] = 
        expand.grid(
          seq(input$senX1[1], input$senX1[2], l = 10),
          seq(input$senX2[1], input$senX2[2], l = 10)
        )
      names(values[["grid"]]) = vars
      newNames = gsub("\n", " ", names(values[["senChoices"]]))
      newNames = setNames(newNames, values[["senChoices"]])[vars]
      
      # Remove afreq from internal grid
      isAfreq = which("afreq" == vars)
      if (length(isAfreq) > 0) {
        afreq = values[["grid"]][[isAfreq]]
        grid2 = values[["grid"]][-isAfreq]
        vars = vars[-isAfreq]
      }
      else {
        afreq = rep(values[["afreq"]], nrow(values[["grid"]]))
        grid2 = values[["grid"]]
      }
      names(values[["grid"]]) = newNames
      
      # Find indexes to substitute
      idx = list()
      for (v in vars) {
        vattr = str_split(v, "_")[[1]] # creates a 2-length vector (e.g. "f0" "1")
        idx[[v]]$col = vattr[1]
        idx[[v]]$row = as.integer(vattr[2])
      }
      
      # Base penetrances
      if (values[["mode"]] == "lclass") {
        temp = copy(values[["fBase"]])
        
        # Substitute parameters
        fBase = lapply(seq(nrow(grid2)), function(i) {
          fBase = temp
          for(v in vars)
            fBase[rowid == idx[[v]]$row, idx[[v]]$col] = grid2[i, v]
          return(fBase)
        })
      }
      else {
        temp = copy(values[["phenoData"]])
        temp[, rowid := seq(.N)]
        temp[, coefs := list(list(suppressWarnings(as.numeric(strsplit(f2coef, ",")[[1]])))), by = .(rowid)]
        temp[, df := length(unlist(coefs)), by = .(rowid)]
        
        # Substitute parameters and recalculate
        fBase = lapply(seq(nrow(grid2)), function(i) {
          rriskPlot = temp
          for(v in vars)
            rriskPlot[rowid == idx[[v]]$row, idx[[v]]$col] = grid2[i, v]
          # Compute hazards
          rriskPlot[, f0CI := list(list(f0R*ptrunc(1:100, "norm", mean = f0mu, sd = f0sigma, a = 0, b = 100))), by = .(rowid)]
          rriskPlot[, f0Hz := list(list(diff(c(0, -log(1 - unlist(f0CI)))))), by = .(rowid)]
          rriskPlot[, f2Hz := list(list(optimHR(unlist(f0Hz), f2R, df, unlist(coefs)))), by = .(rowid)]
          rriskPlot = rriskPlot[, list(age = 1:100, f0Hz = unlist(f0Hz), f2Hz = unlist(f2Hz)), by = .(sex, phenotype)]
          # Expand sex
          rriskPlot[, sex := as.character(sex)]
          rriskPlot[, sexList := .(list(list(sex))), by = .(sex, phenotype, age, f0Hz, f2Hz)]
          rriskPlot[sex == "both", sexList := .(list(list("male", "female")))]
          rriskPlot = rriskPlot[, .(sex = unlist(sexList)), by = .(phenotype, age, f0Hz, f2Hz)]
          # Back to long format
          fBase = melt(rriskPlot, measure.vars = c("f0Hz", "f2Hz"), value.name = "Hz")
          fBase[, variable := factor(variable, levels = c("f0Hz", "f2Hz"), labels = c("f0", "f2"))]
          # Survival penetrances
          fBase[, nonaff := sum(Hz), by = .(age, sex, variable)]
          fBase[, nonaff := 1-exp(-cumsum(nonaff)), by = .(sex, phenotype, variable)]
          fBase[, sp := shift(1 - nonaff, fill = 1) * Hz, by = .(sex, phenotype, variable)]
          faff = dcast(fBase, sex + phenotype + age ~ variable, value.var = "sp")
          faff[f0 > f2, f2 := f0] # fix phi > beta
          fnonaff = dcast(fBase, sex + age ~ variable, value.var = "nonaff", subset = .(phenotype == values[["phenoVector"]][1]))
          fnonaff[, phenotype := "nonaff"]
          fBase = rbind(faff, fnonaff)
          return(fBase)
        })
      }
      
      # Final penetrances
      f = switch(
        values[["inheritance"]],
        "AD" = {
          lapply(seq(nrow(grid2)), function(i) {
            fFull = fBase[[i]]
            fFull[, f1 := f2]
            fSubset = fFull[values[["lclasses"]], on = c("sex", "phenotype", "age")]
            fSubset[, c("f0", "f1", "f2") := lapply(.SD, function(x) fifelse(phenotype == "", 1, x)), .SDcols = c("f0", "f1", "f2")]
            f = fSubset[, c("f0", "f1", "f2")]
          })
        },
        "XD" = {
          lapply(seq(nrow(grid2)), function(i) {
            fFull[, f1 := f2]
            fSubset = fFull[values[["lclasses"]], on = c("sex", "phenotype", "age")]
            fSubset[, c("f0", "f1", "f2") := lapply(.SD, function(x) fifelse(phenotype == "", 1, x)), .SDcols = c("f0", "f1", "f2")]
            f = list(male = lclass_full[sex == "male", c("f0", "f1")],
                     female = lclass_full[sex == "female", c("f0", "f1", "f2")])
          })
        },
        "AR" = {
          lapply(seq(nrow(grid2)), function(i) {
            fFull[, f1 := f0]
            fSubset = fFull[values[["lclasses"]], on = c("sex", "phenotype", "age")]
            fSubset[, c("f0", "f1", "f2") := lapply(.SD, function(x) fifelse(phenotype == "", 1, x)), .SDcols = c("f0", "f1", "f2")]
            f = fSubset[, c("f0", "f1", "f2")]
          })
        },
        "XR" = {
          lapply(seq(nrow(grid2)), function(i) {
            fFull[, f1 := f0]
            fFull[sex == "male", f1 := f2]
            fSubset = f_final[values[["lclasses"]], on = c("sex", "phenotype", "age")]
            fSubset[, c("f0", "f1", "f2") := lapply(.SD, function(x) fifelse(phenotype == "", 1, x)), .SDcols = c("f0", "f1", "f2")]
            f = list(male = fSubset[sex == "male", c("f0", "f1")],
                     female = fSubset[sex == "female", c("f0", "f1", "f2")])
          })
        }
      )
      
      # FLB
      flbs = sapply(seq(nrow(grid2)), function(i) {
        flb = 
          sapply(1:values[["pedTotal"]], function(pedid) {
            idxs = which(values[["pedData"]][["ped"]] == pedid)
            tryCatch(
              FLB(
                x = as.ped(values[["pedData"]][idxs, c("id", "fid", "mid", "sex")]),
                affected = which(values[["affected"]][idxs]),
                unknown = which(values[["unknown"]][idxs]),
                proband = which(values[["proband"]][idxs]),
                if (length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
                if (length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
                if (length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs]),
                freq = 10^afreq[i],
                penetrances = f[[i]],
                liability = values[["liability"]][idxs],
                Xchrom = ifelse(values[["chrom"]] == "x", TRUE, FALSE),
                details = FALSE
              ),
              error = function(err) NA)
          })
        flb = prod(flb)
      })
      
      if (!any(is.na(flbs))) {
        values[["flbs"]] = flbs
        showElement("contourplot")
      }
      else {
        showNotification(
          HTML("<i class='fas fa-triangle-exclamation'></i> FLB could not be calculated."),
          type = "error",
          duration = 3
        )
      }
    })
    
    # Display contourplot
    output$contourplot = renderPlot({
      req(values[["flbs"]])
      contourplot(values[["grid"]], round(values[["flbs"]], 7))
    })
    
  })
}
