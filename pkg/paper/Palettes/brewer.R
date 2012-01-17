###############
## Objective ##
###############

## Try to mimic ColorBrewer.org palettes by
## diverge_hcl(), heat_hcl()/sequential_hcl(), and rainbow_hcl().

## For sequential/diverging palettes:
## ColorBrewer.org often uses palettes that are non-monotonic in
## chroma. Maybe we should have diverge_hcl2(), ..., that does something
## similar.
## Note: Sometimes ColorBrewer.org's colors are "over-saturated" (chroma > 100)
## due to RGB-HCL mapping.

## For qualitative palettes:
## ColorBrewer.org has more shuffling of colors and also allows more variation
## with respect to chroma and luminance. This is also probably a good idea
## if more than four colors are needed.


#######################
## Code and packages ##
#######################

## packages
library("RColorBrewer")
library("colorspace")

## convenience functions
hcl_coords <- function(x) as(hex2RGB(x), "polarLUV")@coords

pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

myplot1 <- function(palette1 = p1, palette2 = p2) {
  par(mfrow = c(2, 1), mar = rep(0, 4))
  pal(palette1)
  pal(palette2)
}

myplot2 <- function(palette1 = p1, palette2 = p2) {
  par(mfrow = c(1, 3), mar = rep(3, 4))
  plot(hcl_coords(palette1)[,1], main = "L", type = "b")
  points(hcl_coords(palette2)[,1], col = 2, type = "b")
  plot(hcl_coords(palette1)[,2], main = "C", type = "b")
  points(hcl_coords(palette2)[,2], col = 2, type = "b")
  plot(hcl_coords(palette1)[,3], main = "H", type = "b")
  points(hcl_coords(palette2)[,3], col = 2, type = "b")
}

############################
## ColorBrewer: Diverging ##
############################

## PiYG
p1 <- brewer.pal(11, "PiYG")
p2 <- diverge_hcl(11, h = c(340, 128), c = 54, l = c(35, 95), power = c(0.7, 1.3), fixup = FALSE)

## PRGn
p1 <- brewer.pal(11, "PRGn")
p2 <- diverge_hcl(11, h = c(300, 128), c = 45, l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE)

## PuOr
p1 <- brewer.pal(11, "PuOr")
p2 <- diverge_hcl(11, h = c(40, 270), c = 45, l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE)

## RdBu
p1 <- brewer.pal(11, "RdBu")
p2 <- diverge_hcl(11, h = c(12, 265), c = 80, l = c(25, 95), power = c(0.7, 1.3), fixup = FALSE)

## BrBG
p1 <- brewer.pal(11, "BrBG")
p2 <- diverge_hcl(11, h = c(55, 160), c = 30, l = c(35, 95), power = c(0.7, 1.3), fixup = FALSE)

## RdGy
## -> unbalanced wrt chroma (not possible with diverge_hcl)

## RdYlBu, RdYlGn, Spectral
## -> via yellow (not possible with diverge_hcl)


##########################################
## ColorBrewer: Sequential (single hue) ##
##########################################

## Greys
p1 <- brewer.pal(9, "Greys")
p2 <- rev(sequential_hcl(9, h = 0, c = 0, l = c(15, 95), power = 1.3, fixup = FALSE))

## Purples
p1 <- brewer.pal(9, "Purples")
p2 <- rev(heat_hcl(9, h = c(280, 260), c = c(60, 5), l = c(20, 95), power = c(0.7, 1.3), fixup = FALSE))

## Blues
p1 <- brewer.pal(9, "Blues")
p2 <- rev(heat_hcl(9, h = c(260, 230), c = c(80, 10), l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE))

## Greens
p1 <- brewer.pal(9, "Greens")
p2 <- rev(heat_hcl(9, h = c(135, 120), c = c(50, 10), l = c(40, 95), power = c(0.4, 1.3), fixup = FALSE))

## Oranges
p1 <- brewer.pal(9, "Oranges")
p2 <- rev(heat_hcl(9, h = c(20, 45), c = c(80, 5), l = c(35, 95), power = c(0.6, 1.3), fixup = FALSE))

## Reds
p1 <- brewer.pal(9, "Reds")
p2 <- rev(heat_hcl(9, h = c(10, 40), c = c(80, 10), l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE))


########################################
## ColorBrewer: Sequential (multihue) ##
########################################

## BuPu
p1 <- brewer.pal(9, "BuPu")
p2 <- rev(heat_hcl(9, h = c(300, 200), c = c(60, 0), l = c(25, 95), power = c(0.7, 1.3), fixup = FALSE))

## GnBu
p1 <- brewer.pal(9, "GnBu")
p2 <- rev(heat_hcl(9, h = c(260, 100), c = c(65, 10), l = c(25, 95), power = c(0.7, 1.7), fixup = FALSE))

## OrRd
p1 <- brewer.pal(9, "OrRd")
p2 <- rev(heat_hcl(9, h = c(10, 60), c = c(80, 10), l = c(25, 95), power = c(0.7, 1.6), fixup = FALSE))

## PuBu
p1 <- brewer.pal(9, "PuBu")
p2 <- rev(heat_hcl(9, h = c(250, 330), c = c(50, 5), l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE))

## PuBuGn
p1 <- brewer.pal(9, "PuBuGn")
p2 <- rev(heat_hcl(9, h = c(160, 330), c = c(30, 5), l = c(35, 95), power = c(0.7, 1.3), fixup = FALSE))

## PuRd
p1 <- brewer.pal(9, "PuRd")
p2 <- rev(heat_hcl(9, h = c(370, 280), c = c(80, 5), l = c(25, 95), power = c(0.7, 1.3), fixup = FALSE))

## YlGn
p1 <- brewer.pal(9, "YlGn")
p2 <- rev(heat_hcl(9, h = c(140, 80), c = c(40, 10), l = c(35, 95), power = c(0.7, 1.7), fixup = FALSE))

## YlGnBu
p1 <- brewer.pal(9, "YlGnBu")
p2 <- rev(heat_hcl(9, h = c(265, 80), c = c(60, 10), l = c(25, 95), power = c(0.7, 2), fixup = FALSE))

## YlOrBr
p1 <- brewer.pal(9, "YlOrBr")
p2 <- rev(heat_hcl(9, h = c(20, 85), c = c(60, 10), l = c(25, 95), power = c(0.7, 1.7), fixup = FALSE))

## YlOrRd
p1 <- brewer.pal(9, "YlOrRd")
p2 <- rev(heat_hcl(9, h = c(10, 85), c = c(80, 10), l = c(25, 95), power = c(0.4, 1.3), fixup = FALSE))


##############################
## ColorBrewer: Qualitative ##
##############################

## Pastel1
p1 <- brewer.pal(5, "Pastel1")
p2 <- rainbow_hcl(5, c = 35, l = 85, fixup = FALSE)

## Pastel2
p1 <- brewer.pal(5, "Pastel2")
p2 <- rainbow_hcl(5, c = 30, l = 85, fixup = FALSE)

## Pastel2
p1 <- brewer.pal(5, "Dark2")
p2 <- rainbow_hcl(5, c = 50, l = 60, fixup = FALSE)

## Set1
p1 <- brewer.pal(5, "Set1")
p2 <- rainbow_hcl(5, c = 50, l = 60, fixup = FALSE)

## Set2
p1 <- brewer.pal(5, "Set2")
p2 <- rainbow_hcl(5, c = 60, l = 70, fixup = FALSE)

## Set3
p1 <- brewer.pal(5, "Set3")
p2 <- rainbow_hcl(5, c = 50, l = 80, start = 10, end = 320, fixup = FALSE)

## Paired
## -> not possible with rainbow_hcl()

