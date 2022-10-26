# Functions for displaying the FLB on a gradient colour bar

library(ggplot2)

# Setup
dat = data.frame(BF = 2^seq(-6,6,length.out = 100))

# The colour bar itself
colbar =
  ggplot(dat) +
  geom_tile(aes(x = 0.5, y = BF, fill = BF), width = 1, show.legend = F) +
  scale_x_continuous(breaks = NULL,
                     expand = expansion(0))  +
  scale_y_continuous(limits = c(range(dat$BF)),
                     trans = "log2",
                     breaks = 2^c(-5,-3,0,3,5),
                     labels = c("1/32", "1/8", "1", "8", "32"),
                     expand = expansion(0),
                     oob = scales::squish) +
  labs(x = NULL, y = NULL) +
  scale_fill_fermenter(palette = "Spectral",
                       breaks = 2^c(-5,-3,0,3,5),
                       limits = range(dat$BF),
                       oob = scales::squish,
                       trans = "log2") +
  coord_flip(clip = "off") +
  theme_minimal() +
  theme(plot.margin = margin(t = 2, unit = "lines"),
        panel.grid = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.text.x = element_text(color = "black", size = 12))


# Display a given value (this is the function called in server.R)
BFplot = function(value) {
  if(is.null(value))
    return(colbar)

  colbar +
    # annotate("segment", size = 0.75,
    #          x = 1.01, xend = 1, y = value, yend = value,
    #          arrow = arrow(type = "closed"), arrow.fill = "gold") +
  geom_point(data = data.frame(x = 1.1, y = value),
             mapping = aes(x, y), shape = 25, size = 4, fill = "gold")
}
