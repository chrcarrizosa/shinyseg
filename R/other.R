# Scale log-HRs to achieve lifetime risk f2R. Return scaled HRs
optimHR = function(f0Hz, f2R, logHR) {
  spl = bs(1:100, df = length(logHR), intercept = TRUE)
  opt = optim(1, fn = function(x) {(sum(exp(spl %*% (x*logHR)) * f0Hz) + log(1 - f2R))^2},
              method = "L-BFGS-B", lower = 0)
  
  # Return scaled HRs
  exp(opt$par * logHR)
}

# Scale log-HRs to achieve lifetime risk f2R. Return f2Hz
optimf2Hz = function(f0Hz, f2R, logHR) {
  spl = bs(1:100, df = length(logHR), intercept = TRUE)
  tryCatch(
    {
      opt = optim(1, fn = function(x) {(sum(exp(spl %*% (x*logHR)) * f0Hz) + log(1 - f2R))^2},
                  method = "L-BFGS-B", lower = 0)
      
      # Return f2Hz
      exp(spl %*% (opt$par * logHR)) * f0Hz
    },
    error = function(err) NA_real_
  )
}

# Get f2Hz from f0Hz and log-HRs
getf2Hz = function(f0Hz, logHR) {
  spl = bs(1:100, df = length(logHR), intercept = TRUE)
  
  # Return f2Hz
  exp(spl %*% logHR) * f0Hz
}

# Assistant: optimize f0 parameters
optimf0 = function(ages, f0CI) {
  opt = optim(par = c(0.5, 50, 20), function(params) {
    fitted = params[1]*ptrunc(ages, "norm", mean = params[2], sd = params[3], a = 0, b = 100)
    sum((fitted - f0CI)^2)
  },
  method = "L-BFGS-B",
  lower = c(0.001, 0.001, 0.001), upper = c(0.999, 1000, 1000),
  control = list(factr = 1e6))
  
  opt$par
}

# Assistant: optimize f2 parameters (optimize log-HRs, return HRs)
optimf2 = function(ages, f0Hz, f2CI, df) {
  spl = bs(1:100, df = df, intercept = TRUE)
  opt = optim(par = rep(1, df), function(params) {
    fitted = cumsum(exp(spl %*% (as.vector(params))) * f0Hz)[ages]
    sum((fitted + log(1 - f2CI))^2)
  },
  method = "L-BFGS-B",
  lower = rep(0, 6), upper = rep(100, 6),
  control = list(factr = 1e6))
  
  exp(opt$par)
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
