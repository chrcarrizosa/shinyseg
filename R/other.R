optimHR = function(f0Hz, f2R, df, w) {
  spl = bs(1:100, df = df, intercept = TRUE)
  opt = optim(1, fn = function(x) {(sum(exp(spl %*% (x*w)) * f0Hz) + log(1 - f2R))^2},
              method = "L-BFGS-B", lower = 0)
  
  # Return f2Hz
  exp(spl %*% (opt$par*w)) * f0Hz
}

# https://stackoverflow.com/questions/40620176/getting-rid-of-the-status-bar-in-file-upload-in-shiny
fileInput2 <- function(inputId, label = NULL, labelIcon = NULL, multiple = FALSE,
                       accept = NULL, width = NULL, progress = TRUE,
                       divClass = "btn btn-default action-button", divStyle = NULL, ...) {
  # add class fileinput_2 defined in UI to hide the inputTag
  inputTag <- tags$input(id = inputId, name = inputId, type = "file",
                         class = "fileinput_2")
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse = ",")
  
  div(..., style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      inputTag,
      # label customized with an action button
      tags$label(`for` = inputId, div(icon(labelIcon), label, class = divClass, style = divStyle)),
      # optionally display a progress bar
      if(progress)
        tags$div(id = paste(inputId, "_progress", sep = ""),
                 class = "progress shiny-file-input-progress",
                 tags$div(class = "progress-bar")
        )
  )
}
