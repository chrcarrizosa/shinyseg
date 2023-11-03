# Functions for displaying the FLB on a contour plot

library(ggplot2)
library(metR)

craftbrewer_pal = function (type = "seq", palette = 1, direction = 1) 
{
  pal = scales:::pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette = RColorBrewer:::maxcolors[names(RColorBrewer:::maxcolors) == palette]
    
    if (n < 3) {
      pal = suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette created based on all colors of", 
                        palette))
      n_palette = RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc = grDevices::colorRampPalette(n_palette)
      pal = colfunc(n)
    }
    else {
      pal = RColorBrewer::brewer.pal(n, pal)
    }
    pal = pal[seq_len(n)]
    if (direction == -1) {
      pal = rev(pal)
    }
    pal
  }
}

scale_fill_craftfermenter = function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type = match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}


# Contour plot
contourplot = function(grid, current, values) {
  
  # Baseline plot
  g = ggplot(mapping = aes(x = grid[,1], y = grid[,2], z = log2(values))) +
    metR::geom_contour_fill(breaks = c(-1e+8, -5, -4, -3, -2, 0, 2, 3, 4, 5, 1e+8)) +
    scale_x_continuous(expand = expansion(0)) +
    scale_y_continuous(expand = expansion(0)) +
    labs(
      x = names(grid)[1],
      y = names(grid)[2]
    ) +
    scale_fill_craftfermenter(
      breaks = c(-5, -4, -3, -2, 0, 2, 3, 4, 5),
      labels = c("", "", "", "", "1", "4", "8", "16", "32"),
      palette = "RdBu",
      limits = c(-6, +6)
    ) +
    theme_classic() +
    theme(legend.position = "none")
  
  # Check if there are values > 64
  highvalues = values[values > 64]
  if(length(highvalues) > 1) {
    extrabreaks = unique(floor(log2(highvalues)))
    for(eb in extrabreaks) {
      g = g +
        metR::geom_contour2(
          label = NA,
          breaks = eb,
          linetype = "dotted",
          size = .4,
          color = "white"
        ) +
        metR::geom_text_contour(
          label = paste(2^eb, ".00"),
          breaks = eb,
          label.placer = label_placer_fraction(frac = 0.4),
          color = "white",
          stroke = 0.1,
          stroke.color = "#67001F")
    }
  }
  
  # Add current selection
  if(current[1] >= min(grid[, 1]) && current[1] <= max(grid[, 1]) &&
     current[2] >= min(grid[, 2]) && current[2] <= max(grid[, 2]))
    g = g +
    annotate(
      "point",
      x = current[1],
      y = current[2],
      shape = 4,
      size = 3,
      stroke = 1,
      color = "black"
    )
  
  # Return final plot
  g
}
