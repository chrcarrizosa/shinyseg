# Functions for displaying the FLB on a contour plot

library(ggplot2)
library(metR)

RdBu = function(name = waiver(), ..., values = NULL, space = "Lab", 
                na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  pal = ggplot2:::pal_binned(function(n) rev(RColorBrewer::brewer.pal(n, "RdBu")))
  binned_scale(aesthetics, name = name, palette = pal, na.value = na.value, guide = guide, ...)
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
    scale_fill_binned(
      breaks = c(-5, -4, -3, -2, 0, 2, 3, 4, 5),
      labels = c("", "", "", "", "1", "4", "8", "16", "32"),
      type = RdBu,
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
          linewidth = .4,
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
