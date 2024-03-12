# Libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(bs4Dash)
  library(shinyWidgets)
  library(shinyalert)
  library(readr) # read html
  library(pedtools)
  library(segregatr)
  library(data.table)
  library(stringr)
  library(rhandsontable)
  library(htmlwidgets)
  library(truncdist)
  library(splines)
  library(plotly)
  library(stringi) # stri_remove_empty_na
  library(rvest)
  library(jsonlite)
})


# Input widgets -----------------------------------------------------------

w_saveReport = 
  popover(
    downloadBttn(
      outputId = "saveReport",
      icon = icon("save"),
      style = "jelly",
      size = "s",
      block = FALSE,
      no_outline = TRUE,
      label = "Save report",
      color = "primary"
    ),
    title = NULL,
    content = "Save the session data in an HTML file."
  )

w_loadReport =
  popover(
    fileInput2(
      inputId = "loadReport",
      label = "Load report",
      labelIcon = "file-import",
      progress = FALSE,
      divClass = "action-button bttn bttn-jelly bttn-sm bttn-primary bttn-no-outline shiny-bound-input"
    ),
    title = NULL,
    content = "Restore a previous session by uploading a saved HTML file."
  )

w_afreq =
  popover(
    numericInputIcon(
      inputId = "afreq",
      label = HTML("<i class='fa fa-dna'></i> Allele frequency"),
      value = 0.001,
      min = 0.00001,
      max = 0.1,
      step = 0.00001
    ),
    title = NULL,
    content = "Select the frequency of the rare variant in the population.",
    placement = "right"
  )

w_example1 = 
  popover(
    boxDropdownItem(
      boxLabel("1", status = "gray"),
      HTML("&nbsp;Constant relative risk"),
      id = "example1"
    ),
    title = NULL,
    content = "A simple case to showcase the relative risk mode and the importance of accounting for age of onset.",
    placement = "right"
  )

w_example2 = 
  popover(
    boxDropdownItem(
      boxLabel("2", status = "gray"),
      HTML("&nbsp;X-linked inheritance"),
      id = "example2"
    ),
    title = NULL,
    content = "An example for the analysis of an X-linked inheritance case.",
    placement = "right"
  )

w_example3 = 
  popover(
    boxDropdownItem(
      boxLabel("3", status = "gray"),
      HTML("&nbsp;Two phenotypes"),
      id = "example3"
    ),
    title = NULL,
    content = "A consanguineous family with two disease phenotypes: one mild and common, and other rare and severe.",
    placement = "right"
  )

w_example4 =
  popover(
    boxDropdownItem(
      boxLabel("4", status = "gray"),
      HTML("&nbsp;Breast cancer and BRCA1"),
      id = "example4"
    ),
    title = NULL,
    content = "A breast cancer case from Belman et al. (2020) requiring additional phenotypes and age-dependent relative risks.",
    placement = "right"
  )


w_examples =
  popover(
    dropdown(
      label = "Examples",
      icon = icon("person-chalkboard"),
      style = "jelly",
      size = "s",
      width = "240px",
      status = "primary",
      w_example1,
      w_example2,
      w_example3,
      w_example4
    ),
    title = NULL,
    content = "Load a worked example.",
    placement = "right"
  )

w_docs =
  popover(
    actionBttn(
      inputId = "docs",
      label = "Documentation",
      style = "jelly",
      color = "primary",
      size = "s",
      icon = icon("github"),
      value = "Open popup",
      onclick = "window.open('https://chrcarrizosa.github.io/shinyseg/')"
    ),
    title = NULL,
    content = "Go to documentation website."
  )


# UI ----------------------------------------------------------------------

# Define UI
ui = dashboardPage(
  
  dark = NULL,
  help = TRUE,

  # Header
  dashboardHeader(
    
    status = "primary",
    
    # Title
    title = 
      div(
        class = "apptitle",
        "shinyseg"
      ),
    
    # Save/load
    leftUi =
      tagList(
        tags$li(
          class = "dropdown",
          w_saveReport
        ),
        tags$li(
          class = "dropdown",
          w_loadReport
        ),
        tags$li(
          class = "dropdown",
          w_examples
        ),
        tags$li(
          class = "dropdown",
          w_docs
        )
      ),
    
    rightUi =
      tagList(
        tags$li(
          class = "dropdown",
          div(
            class = "inline inlinenumeric",
            w_afreq
          )
        ),
        tags$li(
          class = "dropdown",
          popover(
            dropdownMenu(
              type = "notifications",
              badgeStatus = "danger",
              icon = icon("bell"),
              .list = list(
                notificationItem(inputId = "message1", status = "danger", text = "No families added yet"),
                notificationItem(inputId = "message2", status = "danger", text = "1+ phenotype per family required"),
                notificationItem(inputId = "message3", status = "danger", text = "1+ carrier per family required"),
                notificationItem(inputId = "message4", status = "danger", text = "1 proband per family required"),
                notificationItem(inputId = "message5", status = "danger", text = "Some members have missing age"),
                notificationItem(inputId = "message6", status = "danger", text = "Misspecified liability class ages"),
                notificationItem(inputId = "message7", status = "danger", text = "Overlapping liability classes"),
                notificationItem(inputId = "message8", status = "danger", text = "Missing penetrance parameters")
              )
            ),
            title = NULL,
            content = "Minimum steps that are still required in your analysis.",
            placement = "right"
          )
        )
      )
  ),
  
  # Sidebar
  dashboardSidebar(disable = TRUE, width = 0, minified = FALSE),
  
  # Main panel
  dashboardBody(
    
    useShinyjs(),
    
    chooseSliderSkin("Flat"),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    includeScript("www/customHandlers.js"),
    
    fluidRow(
      column(7,
             pedigreeBoxUI("pedigree"),
             penetranceBoxUI("penetrance")
      ),
      column(5,
             plotBoxUI("plot"),
             bayesBoxUI("bayes")
      )
    )
  )
)


# Server ------------------------------------------------------------------

server = function(input, output, session) {
  
  # Set time zone to Norwegian time
  Sys.setenv(TZ = "Europe/Oslo")
  
  # Reactive values object
  values = reactiveValues()
  observe({
    values[["afreq"]] = input$afreq
  })
  
  # Server modules
  pedigreeBoxServer("pedigree", values)
  penetranceBoxServer("penetrance", values)
  plotBoxServer("plot", values)
  bayesBoxServer("bayes", values)
  
  
  ##### Flags
  # At least 1 family
  observeEvent(values[["pedTotal"]], {
    # message("Checking ped")
    if (values[["pedTotal"]]) {
      hideElement("message1")
      showElement("plot-box")
    }
    else {
      showElement("message1")
      for (i in 2:9)
        hideElement(paste0("message", i))
      hideElement("plot-box")
      hideElement("penetrance-box")
    }
    session$sendCustomMessage(type = "update-dropdown", message = "NULL")
  })
  # At least 1 affected per family
  observeEvent(values[["affected"]], {
    # message("Checking affected")
    ok = 
      all(
        sapply(1:values[["pedTotal"]], function(pedid) {
          idxs = which(values[["pedData"]][["ped"]] == pedid)
          sum(values[["affected"]][idxs]) >= 1})
      )
    if (ok) {
      values[["pedNames"]][6] = "phenotype"
      hideElement("message2")
    }
    else {
      values[["pedNames"]][6] = "<i class='fa fa-triangle-exclamation fa-fw' style='color:#db1f48'></i>phenotype"
      showElement("message2")
    }
    session$sendCustomMessage(type = "update-dropdown", message = "NULL")
  })
  # At least 1 phenotype
  observeEvent(values[["phenoTotal"]], {
    if (values[["phenoTotal"]] > 0)
      showElement("penetrance-box")
    else {
      hideElement("message8")
      hideElement("penetrance-box")
    }
    session$sendCustomMessage(type = "update-dropdown", message = "NULL")
  })
  # At least 1 carrier per family
  observeEvent(c(values[["carriers"]], values[["homozygous"]]), {
    # message("Checking carriers")
    ok =
      all(
        sapply(1:values[["pedTotal"]], function(pedid) {
          idxs = which(values[["pedData"]][["ped"]] == pedid)
          sum(values[["carriers"]][idxs] | values[["homozygous"]][idxs]) >= 1})
      )
    if (ok) {
      values[["pedNames"]][7] = "carrier"
      hideElement("message3")
    }
    else {
      values[["pedNames"]][7] = "<i class='fa fa-triangle-exclamation fa-fw' style='color:#db1f48'></i>carrier"
      showElement("message3")
    }
    session$sendCustomMessage(type = "update-dropdown", message = "NULL")
  })
  # At least 1 affected carrier proband per family
  observeEvent(c(values[["affected"]], values[["carriers"]], values[["homozygous"]], values[["proband"]]), {
    # message("Checking proband")
    ok = sum(values[["proband"]] & values[["affected"]] & (values[["carriers"]] | values[["homozygous"]])) == values[["pedTotal"]]
    if (ok) {
      values[["pedNames"]][8] = "proband"
      hideElement("message4")
    }
    else {
      values[["pedNames"]][8] = "<i class='fa fa-triangle-exclamation fa-fw' style='color:#db1f48'></i>proband"
      showElement("message4")
    }
    session$sendCustomMessage(type = "update-dropdown", message = "NULL")
  })
  # Missing ages
  observeEvent(values[["okAge"]], {
    if (values[["okAge"]]) {
      values[["pedNames"]][9] = "age"
      hideElement("message5")
    }
    else {
      values[["pedNames"]][9] = "<i class='fa fa-triangle-exclamation fa-fw' style='color:#db1f48'></i>age"
      showElement("message5")
    }
    session$sendCustomMessage(type = "update-dropdown", message = "NULL")
  })
  
  
  ##### Report
  output$saveReport = downloadHandler(
    filename = "report.html",
    content = function(file) {
      # Copy to a temporary directory to avoid writing restrictions
      file.copy("report.Rmd", file.path(tempdir(), "report.Rmd"), overwrite = TRUE)
      file.copy("www/styles.css", file.path(tempdir(), "styles.css"), overwrite = TRUE)
      file.copy("R/colourbar.R", file.path(tempdir(), "colourbar.R"), overwrite = TRUE)
      # Knit the document in a child of the global environment
      rmarkdown::render(
        file.path(tempdir(), "report.Rmd"),
        output_file = file,
        params = list(values = reactiveValuesToList(values)),
        envir = new.env(parent = globalenv()),
        quiet = TRUE
      )
    }
  )
  observeEvent(input$loadReport, {
    updateSession = 
      suppressWarnings(
        tryCatch(
          {
            # Read
            report = read_html(input$loadReport$datapath)
            
            # Pedigree
            pedigree = html_text(html_elements(report, "#pedigree-table"))
            pedData = str_extract(pedigree, "\\[\\{.*?\\}\\]") # non-greedy match for JSON
            if (!is.na(pedData)) {
              pedData = setDT(fromJSON(pedData))
              pedData[["phenotype"]] = factor(pedData[["phenotype"]], levels = unique(c("", "nonaff", "aff", pedData[["phenotype"]])))
              pedTotal = as.integer(max(pedData[["ped"]]))
              lastProband = pedData[["proband"]]
            }
            else {
              pedData = NULL
              pedTotal = 0
              lastProband = NULL
            }
            
            # Parameters
            parameters = html_text(html_elements(report, "#parameters"))
            afreq = gsub(".*Allele frequency: (\\S+)\\..*", "\\1", parameters)
            afreq = as.numeric(afreq)
            table = str_extract(parameters, "\\[\\{.*?\\}\\]") # non-greedy match for JSON
            if (!is.na(table)) {
              mode = gsub(".*Mode:\\s*([^\\.]+)\\..*", "\\1", parameters)
              inheritance = gsub(".*Inheritance:\\s*([A-Za-z]{2}).*", "\\1", parameters)
              if (grepl("Relative risk", mode)) {
                polDegree =
                  switch(
                    word(mode, -1),
                    "linear" = 1,
                    "quadratic" = 2,
                    "cubic" = 3,
                  )
                mode = "rrisk"
                phenoData = setDT(fromJSON(table))
                phenoData[, c("f0mu", "f0sigma") := lapply(.SD, as.numeric), .SDcols = c("f0mu", "f0sigma")]
                extraPheno = setdiff(unique(phenoData[["phenotype"]]), pedData[["phenotype"]])
                lclassData =
                  data.table(
                    f0 = 0.1,
                    f2 = 0.8,
                    sex = NA_character_,
                    phenotype = NA_character_,
                    ages = NA_character_
                  )
              }
              else {
                mode = "lclass"
                polDegree = 2
                extraPheno = NULL
                phenoData = NULL
                lclassData = setDT(fromJSON(table))
                if (inheritance %in% c("AI", "XI"))
                  lclassData[, c("f0", "f1", "f2") := lapply(.SD, as.numeric), .SDcols = c("f0", "f1", "f2")]
                else
                  lclassData[, c("f0", "f2") := lapply(.SD, as.numeric), .SDcols = c("f0", "f2")]
                lclassData[, c("phenotype", "ages") := lapply(.SD, as.character), .SDcols = c("phenotype", "ages")]
              }
            }
            else {
              mode = "rrisk"
              polDegree = 2
              inheritance = "AD"
              extraPheno = NULL
              phenoData = NULL
              lclassData =
                data.table(
                  f0 = 0.1,
                  f2 = 0.8,
                  sex = NA_character_,
                  phenotype = NA_character_,
                  ages = NA_character_
                )
            }
            
            # Return
            updateSession =
              list(
                "afreq" = afreq,
                "mode" = mode,
                "polDegree" = polDegree,
                "inheritance" = inheritance,
                "pedData" = pedData,
                "pedTotal" = pedTotal,
                "lastProband" = lastProband,
                "extraPheno" = extraPheno,
                "phenoData" = phenoData,
                "lclassData" = lclassData
              )
          },
          error = function(err) NULL
        )
      )
    
    if (is.null(updateSession))
      showNotification(
        HTML("<i class='fas fa-triangle-exclamation'></i> Invalid report file."),
        type = "error",
        duration = 3
      )
    else
      shinyalert(
        type = "warning",
        html = TRUE,
        text = 
          tagList(
            div(
              "This will override all data and settings from the current session. Are you sure you want to continue?",
              align = "center"
            )
          ),
        animation = "pop",
        confirmButtonCol = "#39A0ED",
        showCancelButton = TRUE,
        size = "xs",
        callbackR = function(x) {
          if (x) {
            values[["pedData"]] = NULL
            values[["updateSession"]] = updateSession
          }
        }
      )
  })
  
  
  ##### Examples
  observeEvent(input$example1, {
    values[["example"]] = 1
  })
  observeEvent(input$example2, {
    values[["example"]] = 2
  })
  observeEvent(input$example3, {
    values[["example"]] = 3
  })
  observeEvent(input$example4, {
    values[["example"]] = 4
  })
  observeEvent(ignoreInit = TRUE, ignoreNULL = TRUE, values[["example"]], {
    shinyalert(
      type = "warning",
      html = TRUE,
      text = 
        tagList(
          div(
            "This will override all data and settings from the current session. Are you sure you want to continue?",
            align = "center"
          )
        ),
      animation = "pop",
      confirmButtonCol = "#39A0ED",
      showCancelButton = TRUE,
      size = "xs",
      callbackR = function(x) {
        if (x) {
          # message("Loading example")
          switch(
            values[["example"]],
            
            `1` = {
              # UI changes
              afreq = 0.001
              mode = "rrisk" # values[["mode"]]
              polDegree = 2
              inheritance = "AD"
              
              # Pedigree table
              # Family and indexes
              pedTotal = as.integer(1)
              pedToAdd = nuclearPed(nch = 2) |>
                addChildren(fa = 3, nch = 2, verbose = FALSE) |>
                addChildren(fa = 4, nch = 2, verbose = FALSE) |>
                addChildren(fa = 7, nch = 3, sex = c(2, 2, 1), verbose = FALSE) |>
                addChildren(fa = 9, nch = 1, verbose = FALSE) |>
                relabel("asPlot")
              affected = c(5, 8, 10, 13)
              unknown = c(1:2)
              carriers = c(5, 8, 10, 13, 15, 16)
              noncarriers = c(7, 12, 14)
              proband = 13
              age = c(80, 80, 40, 80, 60, 80, 60, 50, 60, 30, 50, 50, 30, 30, 30, 30)
              # Full vectors and data
              vecPheno = rep("nonaff", pedsize(pedToAdd))
              vecPheno[affected] = "affected"
              vecPheno[unknown] = ""
              vecCarrier = rep("", pedsize(pedToAdd))
              vecCarrier[carriers] = "het"
              vecCarrier[noncarriers] = "neg"
              lastProband = rep(FALSE, pedsize(pedToAdd))
              lastProband[proband] = TRUE
              pedData = data.table(
                ped = as.integer(1),
                as.data.frame(pedToAdd),
                phenotype = factor(vecPheno, levels = unique(c("", "nonaff", "aff", vecPheno))),
                carrier = factor(vecCarrier, levels = c("", "neg", "het", "hom")),
                proband = lastProband,
                age = as.integer(age)
              )
              
              # Penetrance
              extraPheno = NULL
              phenoData = 
                data.table(
                  sex = c("both"),
                  phenotype = c("affected"),
                  f0R = 0.001,
                  f0mu = 60,
                  f0sigma = 15,
                  f2R = 0.90,
                  HR = "2301.43"
                )
              lclassData = 
                data.table(
                  f0 = 0.001,
                  f2 = 0.90,
                  sex = NA_character_,
                  phenotype = NA_character_,
                  ages = NA_character_
                )
            },
            
            `2` = {
              # UI changes
              afreq = 0.001
              mode = "lclass" # values[["mode"]]
              polDegree = 2
              inheritance = "XR"
              
              # Pedigree table
              # Family and indexes
              pedTotal = as.integer(1)
              pedToAdd = avuncularPed(type = "maternal") |>
                addChildren(fa = 4, mo = 5, verbose = FALSE) |>
                relabel("asPlot")
              affected = c(3, 6, 7)
              unknown = c(1:2)
              carriers = c(3, 5, 6, 7)
              noncarriers = c(4)
              proband = 6
              age = c(80, 80, 40, 60, 60, 40, 40)
              # Full vectors and data
              vecPheno = rep("nonaff", pedsize(pedToAdd))
              vecPheno[affected] = "affected"
              vecPheno[unknown] = ""
              vecCarrier = rep("", pedsize(pedToAdd))
              vecCarrier[carriers] = "het"
              vecCarrier[noncarriers] = "neg"
              lastProband = rep(FALSE, pedsize(pedToAdd))
              lastProband[proband] = TRUE
              pedData = data.table(
                ped = as.integer(1),
                as.data.frame(pedToAdd),
                phenotype = factor(vecPheno, levels = unique(c("", "nonaff", "aff", vecPheno))),
                carrier = factor(vecCarrier, levels = c("", "neg", "het", "hom")),
                proband = lastProband,
                age = as.integer(age)
              )
              
              # Penetrance
              extraPheno = NULL
              phenoData = 
                data.table(
                  sex = "both",
                  phenotype = "affected",
                  f0R = 0.01,
                  f0mu = 70,
                  f0sigma = 15,
                  f2R = 0.75,
                  HR = "137.93"
                )
              lclassData = 
                data.table(
                  f0 = 0,
                  f2 = 1,
                  sex = NA_character_,
                  phenotype = NA_character_,
                  ages = NA_character_
                )
            },
            
            `3` = {
              # UI changes
              afreq = 0.001
              mode = "rrisk" # values[["mode"]]
              polDegree = 2
              inheritance = "AD"
              
              # Pedigree table
              # Family and indexes
              pedTotal = as.integer(1)
              pedToAdd = cousinPed(degree = 2, child = TRUE) |>
                addChildren(fa = 11, mo = 12, nch = 2, sex = 2) |>
                relabel("asPlot")
              affected = c(4, 7, 9, 12, 13, 14)
              unknown = c(1:3, 5)
              carriers = c(7, 12:14)
              noncarriers = 15
              proband = 13
              age = c(80, 90, 80, 70, 70, 80, 60, 80, 50, 80, 70, 60, 50, 50, 40)
              # Full vectors and data
              vecPheno = rep("nonaff", pedsize(pedToAdd))
              vecPheno[affected] = c("mild", "severe", "mild", "mild", "severe", "mild")
              vecPheno[unknown] = ""
              vecCarrier = rep("", pedsize(pedToAdd))
              vecCarrier[carriers] = "het"
              vecCarrier[noncarriers] = "neg"
              lastProband = rep(FALSE, pedsize(pedToAdd))
              lastProband[proband] = TRUE
              pedData = data.table(
                ped = as.integer(1),
                as.data.frame(pedToAdd),
                phenotype = factor(vecPheno, levels = unique(c("", "nonaff", "aff", vecPheno))),
                carrier = factor(vecCarrier, levels = c("", "neg", "het", "hom")),
                proband = lastProband,
                age = as.integer(age)
              )
              
              # Penetrance
              extraPheno = NULL
              phenoData =
                data.table(
                  sex = factor("both", levels = c("both", "male", "female")),
                  phenotype = c("mild", "severe"),
                  f0R = c(0.2, 0.01),
                  f0mu = c(75, 60),
                  f0sigma = c(20, 15),
                  f2R = c(0.7, 0.25),
                  HR = c("2.63, 4.16, 13.13, 1", "28.62")
                )
              lclassData =
                data.table(
                  f0 = c(0.208, 0.2, 0.01),
                  f2 = c(0.775, 0.7, 0.25),
                  sex = NA_character_,
                  phenotype = c("nonaff", "mild", "severe"),
                  ages = NA_character_
                )
            },
            
            `4` = {
              # UI changes
              afreq = 0.001
              mode = "rrisk" # values[["mode"]]
              polDegree = 2
              inheritance = "AD"
              
              # Pedigree table
              # Family and indexes
              pedTotal = as.integer(1)
              pedToAdd = nuclearPed(nch = 4, sex = c(2, 1, 2, 2)) |>
                addChildren(fa = 4, nch = 3, sex = 2, verbose = FALSE) |>
                addChildren(mo = 5, nch = 3, sex = c(2, 1, 2), verbose = FALSE) |>
                relabel("asPlot")
              affected = c(2, 3, 9, 10, 12, 14)
              unknown = 5
              carriers = c(3, 7, 9:14)
              noncarriers = 8
              proband = 12
              age = c(80, 65, 81, 41, 89, 80, 75, 60, 41, 50, 52, 49, 36, 48)
              # Full vectors and data
              vecPheno = rep("nonaff", pedsize(pedToAdd))
              vecPheno[affected] = "BrCa"
              vecPheno[unknown] = ""
              vecCarrier = rep("", pedsize(pedToAdd))
              vecCarrier[carriers] = "het"
              vecCarrier[noncarriers] = "neg"
              lastProband = rep(FALSE, pedsize(pedToAdd))
              lastProband[proband] = TRUE
              pedData = data.table(
                ped = as.integer(1),
                as.data.frame(pedToAdd),
                phenotype = factor(vecPheno, levels = unique(c("", "nonaff", "aff", vecPheno))),
                carrier = factor(vecCarrier, levels = c("", "neg", "het", "hom")),
                proband = lastProband,
                age = as.integer(age)
              )
              
              # Penetrance
              extraPheno = c("OvCa", "PanCa")
              phenoData = 
                data.table(
                  sex = factor(rep(c("male", "female"), 3), levels = c("both", "male", "female")),
                  phenotype = rep(c("BrCa", "OvCa", "PanCa"), each = 2),
                  f0R = c(0.001, 0.164, 0, 0.024, 0.032, 0.028),
                  f0mu = c(65.3, 69.6, 50.0, 71.5, 82.9, 84.7),
                  f0sigma = c(19.3, 19.2, 15, 15.0, 15.0, 15.2),
                  f2R = rep(NA_real_, 6), # c(0.008, 0.8271, 0, 0.5986, 0.0514, 0.0408)
                  HR = c("8, 8, 8, 8, 1, 1",
                         "73.7, 73.7, 31.7, 8.35, 1, 1",
                         "1",
                         "1, 1, 49.05, 61.2, 1, 1",
                         "4.68, 4.68, 4.68, 1.4, 1, 1",
                         "4.68, 4.68, 4.68, 1.4, 1, 1")
                )
              lclassData = 
                data.table(
                  f0 = 0.1,
                  f2 = 0.8,
                  sex = NA_character_,
                  phenotype = NA_character_,
                  ages = NA_character_
                )
            }
            
          )
          
          values[["pedData"]] = NULL
          values[["updateSession"]] =
            list(
              "afreq" = afreq,
              "mode" = mode,
              "polDegree" = polDegree,
              "inheritance" = inheritance,
              "pedData" = pedData,
              "pedTotal" = pedTotal,
              "lastProband" = lastProband,
              "extraPheno" = extraPheno,
              "phenoData" = phenoData,
              "lclassData" = lclassData
            )
        }
        values[["example"]] = NULL
      }
    )
  })
  
  observeEvent(priority = -1, values[["updateSession"]], {
    
    with(values[["updateSession"]], {
      updateNumericInput(
        inputId = "afreq",
        value = afreq
      )
      updateRadioGroupButtons(
        inputId = "penetrance-mode",
        selected = mode,
        disabledChoices = if (inheritance %in% c("AI", "XI")) "rrisk" else NULL
      )
      values[["mode"]] = mode
      updatePickerInput(
        session = getDefaultReactiveDomain(),
        inputId = "penetrance-polDegree",
        selected = polDegree
      )
      updateRadioGroupButtons(
        inputId = "penetrance-inheritance",
        selected = inheritance,
        disabledChoices = if (mode == "rrisk") c("AI", "XI") else NULL
      )
      values[["inheritance"]] = inheritance
      
      values[["pedData"]] = pedData
      values[["pedTotal"]] = pedTotal
      values[["lastProband"]] = lastProband
      values[["pedCurrent"]] = 1
      values[["extraPheno"]] = extraPheno
      values[["phenoData"]] = phenoData
      values[["lclassData"]] = lclassData
      
    })
    values[["updateSession"]] = NULL
  })
  
}


# Run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
