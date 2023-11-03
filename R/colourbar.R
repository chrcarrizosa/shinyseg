# Functions for displaying the FLB on a gradient colour bar

library(ggplot2)
library(RColorBrewer)

# Setup
dat = data.frame(BF = 2^seq(-6, 6, length.out = 100))
thresh = 2^c(-5, -4, -3, -2, 0, 2, 3, 4, 5)
thresh_ex = c(-Inf, thresh, Inf)
cols = rev(brewer.pal(10, "RdBu"))
hjts = c(rep(0.15, 5), 0.5, rep(0.85, 4))

# The colour bar itself
colbar =
  ggplot(dat) +
  geom_tile(
    aes(x = 0.5, y = BF, fill = BF),
    width = 1,
    show.legend = FALSE) +
  scale_x_continuous(
    breaks = NULL,
    expand = expansion(0)
  )  +
  scale_y_continuous(
    limits = range(dat$BF),
    trans = "log2",
    breaks = thresh,
    labels = c("", "", "", "", "1", "4", "8", "16", "32"),
    expand = expansion(0),
    oob = scales::squish
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_fill_fermenter(
    palette = "RdBu",
    breaks = thresh,
    limits = range(dat$BF),
    oob = scales::squish,
    trans = "log2"
  ) +
  coord_flip(
    clip = "off",
    xlim = c(0, 1.3)
  ) +
  theme_minimal() +
  theme(
    plot.margin = margin(t = 2.5, unit = "lines"),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    # axis.ticks.x = element_line(colour = "black"),
    axis.text.x = element_text(color = "black", size = 12)
  )


# Display a given value (this is the function called in server.R)
BFplot = function(value, nfam) {
  if (is.null(value) | !is.numeric(value))
    return(colbar)
  
  idx = findInterval(value, thresh_ex)
  # Jarvik and Browning (2016) thresholds
  if (nfam == 1)
    labs = c(rep("Benign", 5), rep("No evidence", 2), "Pathogenic supporting", "Pathogenic moderate", "Pathogenic strong")
  else
    labs = c(rep("Benign", 5), "No evidence", "Pathogenic supporting", "Pathogenic moderate", rep("Pathogenic strong", 2))
  
  colbar +
    annotate(
      "segment",
      x = 1.08, xend = 1.07, y = value, yend = value, linewidth = 0.35,
      arrow = arrow(type = "closed", length = unit(1, "npc")),
      arrow.fill = cols[idx]
    ) +
    annotate(
      "text",
      x = 3.5,
      y = value,
      label = labs[idx],
      hjust = hjts[idx],
      size = 4
    )
}
