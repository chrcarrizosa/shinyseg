# Functions for displaying the FLB on a gradient colour bar

library(ggplot2)
library(metR)

craftbrewer_pal <- function (type = "seq", palette = 1, direction = 1) 
{
  pal <- scales:::pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette <- RColorBrewer:::maxcolors[names(RColorBrewer:::maxcolors) == palette]
    
    if (n < 3) {
      pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette created based on all colors of", 
                        palette))
      n_palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc <- grDevices::colorRampPalette(n_palette)
      pal <- colfunc(n)
    }
    else {
      pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

scale_fill_craftfermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "fermenter", ggplot2:::binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}


# Contour plot
contourplot = function(grid, flb_vals, flb) {
  if(is.null(flb))
    return(ggplot())
  
  ggplot(mapping = aes(x = grid[,1], y = grid[,2], z = log2(flb_vals))) +
    metR::geom_contour_fill() +
    metR::geom_contour2(aes(label = formatC(2^..level.., digits = 3, format = "f"))) +
    scale_x_continuous(expand = expansion(0)) +
    scale_y_continuous(expand = expansion(0)) +
    labs(x = names(grid)[1], y = names(grid)[2]) +
    scale_fill_craftfermenter(
      breaks = c(-5,-3,0,3,5), 
      labels = c("1/32", "1/8", "1", "8", "32"),
      palette = "Spectral", 
      limits = c(-6, +6),
      # oob = scales::squish,
      guide = guide_colorsteps(
        even.steps = FALSE,
        # frame.colour = "black", 
        ticks.colour = "black", # you can also remove the ticks with NA
        barwidth = 20)
    ) +
    theme_classic() +
    theme(legend.position = "bottom")
    # geom_point(data = data.frame(x = 3, y = 70),
    #            mapping = aes(x, y), shape = 25, size = 4, fill = "gold")
}