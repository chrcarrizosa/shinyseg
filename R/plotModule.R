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

w_pedPrev = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "pedPrev"),
      label = NULL,
      style = "jelly",
      size = "xs",
      icon = icon("backward-step")
    ),
    title = NULL,
    content = "Go to the previous family."
  )

w_pedNext = function(id)
  popover(
    actionBttn(
      inputId = NS(id, "pedNext"),
      label = NULL,
      style = "jelly",
      size = "xs",
      icon = icon("forward-step")
    ),
    title = NULL,
    content = "Go to the next family."
  )


# Module code -------------------------------------------------------------

plotBoxUI = function(id) {
  div(
    id = NS(id, "box"),
    box(
      width = 12,
      title = 
        div(
          div(
            class = "leftcolumn flexcontainer",
            "Plot",
            div(w_pedPrev(id), style = "margin-top: -0.1rem; margin-left: 0.5rem;"),
            div(uiOutput(NS(id, 'pedCurrent')), style = "margin-top: 0.1rem; font-size: 0.9rem; color: #4e4e4e;"),
            div(w_pedNext(id), style = "margin-top: -0.1rem;")
          ),
          div(
            class = "rightcolumn",
            w_help(id)
          )
        ),
      collapsible = FALSE,
      # fluidRow(
      plotOutput(NS(id, "pedplot"), height = "350px")
      # )
    )
  )
}

plotBoxServer = function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Help
    observeEvent(input$help, {
      shinyalert(
        className = "helpbox",
        html = TRUE,
        text = read_file("modals/plotBox.html"),
        animation = "slide-from-bottom",
        showConfirmButton = FALSE,
        closeOnClickOutside = TRUE,
        size = "m"
      )
    })
    
    # Pedigree selector
    observeEvent(input$pedPrev, {
      req(values[["pedTotal"]] > 1)
      if (values[["pedCurrent"]] == 1)
        values[["pedCurrent"]] = values[["pedTotal"]]
      else
        values[["pedCurrent"]] = values[["pedCurrent"]] - 1
    })
    observeEvent(input$pedNext, {
      req(values[["pedTotal"]] > 1)
      if (values[["pedCurrent"]] < values[["pedTotal"]])
        values[["pedCurrent"]] = values[["pedCurrent"]] + 1
      else
        values[["pedCurrent"]] = 1
    })
    output$pedCurrent = renderUI({
      helpText(paste0("Pedigree ", values[["pedCurrent"]], "/", values[["pedTotal"]]))
    })
    
    # Segregation plot
    output$pedplot = renderPlot({
      req(values[["pedData"]])
      message("Updating pedigree plot")
      par(family = "helvetica")
      
      # Color palette (Okabe-Ito colors, without black)
      pal = c("white", rep(c("#0072B2", "#D55E00", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7"), ceiling(values[["phenoTotal"]]/8)))
      # pal = c("white", palette.colors(n = values[["phenoTotal"]], "Okabe-Ito", recycle = TRUE))
      names(pal) = c("nonaff", values[["phenoVector"]])
      fillcols = pal[as.character(values[["pedData"]][["phenotype"]])]
      
      idxs = which(values[["pedData"]][["ped"]] == values[["pedCurrent"]])
      plotSegregation(
        as.ped(values[["pedData"]][idxs, c("id", "fid", "mid", "sex")]),
        affected = NULL,  # important to keep this, otherwise symbols may not be plotted correctly
        fill = unname(fillcols[idxs]),
        unknown = which(values[["unknown"]][idxs]),
        proband = which(values[["proband"]][idxs]),
        if (length(which(values[["carriers"]][idxs]) > 0)) carriers = which(values[["carriers"]][idxs]),
        if (length(which(values[["homozygous"]][idxs]) > 0)) homozygous = which(values[["homozygous"]][idxs]),
        if (length(which(values[["noncarriers"]][idxs]) > 0)) noncarriers = which(values[["noncarriers"]][idxs]),
        labs = setNames(seq_along(idxs), str_replace_na(values[["lclasses"]][["age"]][idxs], " ")),
        margins = c(2 + 3, 2, 2, 2)
      )
      legend(
        "bottomright",
        inset = c(-0.1, -0.15),
        legend = c("nonaff", values[["phenoVector"]]),
        fill = pal,
        ncol = values[["phenoTotal"]] + 1,
        bty = "n",
        cex = 1.2)
      
    }, execOnResize = TRUE)
    
  })
}
