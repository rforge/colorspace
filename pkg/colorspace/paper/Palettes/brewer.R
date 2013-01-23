###############
## Objective ##
###############

## Try to mimic ColorBrewer.org palettes by
## diverge_hcl(), heat_hcl()/sequential_hcl(), and rainbow_hcl().

## For sequential/diverging palettes:
## ColorBrewer.org often uses palettes that are non-monotonic in
## chroma. Maybe we should have diverge2_hcl(), ..., that does something
## similar. -> Added now.
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

## new palette functions
diverge2_hcl <- function(n, h = c(300, 130), c. = c(70, 30), l = c(95, 25),
  power = 1.3, fixup = TRUE, ...)
{
    if(n < 1) return(character(0L))

    h <- rep(h, length.out = 2L)
    c <- rep(c., length.out = 2L)
    l <- rep(l, length.out = 2L)
    power <- power[1L]
    
    maxat <- sqrt(c[2L]/c[1L])
    triang <- function(x) ifelse(x <= maxat, x/maxat,
      1 - (1 - c[2L]/c[1L]) * (x - maxat) / (1 - maxat))

    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(L = l[1L] + diff(l) * abs(rval)^power,
                         C = c[1L] * triang(abs(rval)),
                         H = ifelse(rval > 0, h[1L], h[2L])),
                fixup = fixup, ...)
    return(rval)
}

sequential2_hcl <- function(n, h = c(200, 310), c. = c(5, 65, 35), l = c(95, 20),
                     power = 1.3, fixup = TRUE, ...)
{
    if(n < 1) return(character(0L))

    h <- rep(h, length.out = 2L)
    c <- rep(c., length.out = 3L)
    l <- sort(rep(l, length.out = 2L), decreasing = TRUE)
    power <- power[1L]

    if(l[2L] > l[1L]) {
      reverse <- TRUE
      h <- rev(h)
      c <- rev(c)    
      l <- rev(l)
    } else {
      reverse <- FALSE
    }

    maxat <- if(c[2L] == c[3L]) 1 else sqrt((c[3L] - c[1L])/(c[2L] - c[1L]))
    triang <- function(x) ifelse(x <= maxat, x/maxat,
      1 - (1 - (c[3L] - c[1L])/(c[2L] - c[1L])) * (x - maxat) / (1 - maxat))

    rval <- seq(0, 1, length = n)        
    rval <- hex(polarLUV(L = l[1L] + diff(l) * rval^power,
                         C = c[1L] + diff(range(c)) * triang(rval),
                         H = h[1L] + diff(h) * rval),
                fixup = fixup, ...)
    if(reverse) rval <- rev(rval)
    return(rval)
}

## convenience functions
hcl_coords <- function(x) as(hex2RGB(x), "polarLUV")@coords

pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

myplot1 <- function(palette1 = p1, palette2 = p2, palette3 = p3) {
  par(mfrow = c(3, 1), mar = rep(0, 4))
  pal(palette1)
  pal(palette2)
  if(!is.null(p3)) pal(palette3)
}

myplot2 <- function(palette1 = p1, palette2 = p2, palette3 = p3) {
  par(mfrow = c(1, 3), mar = rep(3, 4))
  plot(hcl_coords(palette1)[,1], main = "L", type = "b")
  points(hcl_coords(palette2)[,1], col = 2, type = "b")
  if(!is.null(p3)) points(hcl_coords(palette3)[,1], col = 4, type = "b")
  plot(hcl_coords(palette1)[,2], main = "C", type = "b")
  points(hcl_coords(palette2)[,2], col = 2, type = "b")
  if(!is.null(p3)) points(hcl_coords(palette3)[,2], col = 4, type = "b")
  plot(hcl_coords(palette1)[,3], main = "H", type = "b")
  points(hcl_coords(palette2)[,3], col = 2, type = "b")
  if(!is.null(p3)) points(hcl_coords(palette3)[,3], col = 4, type = "b")
}

############################
## ColorBrewer: Diverging ##
############################

## PiYG
p1 <- brewer.pal(11, "PiYG")
p2 <- diverge2_hcl(11, h = c(340, 125), c = c(70, 40), l = c(95, 30), power = 1.3, fixup = FALSE)
p3 <- diverge_hcl(11, h = c(340, 128), c = 54, l = c(35, 95), power = c(0.7, 1.3), fixup = FALSE)

## PRGn
p1 <- brewer.pal(11, "PRGn")
p2 <- diverge2_hcl(11, h = c(300, 130), c = c(70, 30), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- diverge_hcl(11, h = c(300, 128), c = 45, l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE)

## PuOr
p1 <- brewer.pal(11, "PuOr")
p2 <- diverge2_hcl(11, h = c(40, 280), c = c(70, 30), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- diverge_hcl(11, h = c(40, 270), c = 45, l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE)

## RdBu
p1 <- brewer.pal(11, "RdBu")
p2 <- diverge2_hcl(11, h = c(15, 260), c = c(100, 50), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- diverge_hcl(11, h = c(12, 265), c = 80, l = c(25, 95), power = c(0.7, 1.3), fixup = FALSE)

## BrBG
p1 <- brewer.pal(11, "BrBG")
p2 <- diverge2_hcl(11, h = c(55, 160), c = c(50, 20), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- diverge_hcl(11, h = c(55, 160), c = 30, l = c(35, 95), power = c(0.7, 1.3), fixup = FALSE)

## RdGy
## -> unbalanced wrt chroma (not possible with diverge_hcl)

## RdYlBu, RdYlGn, Spectral
## -> via yellow (not possible with diverge_hcl)


##########################################
## ColorBrewer: Sequential (single hue) ##
##########################################

## Greys
p1 <- brewer.pal(9, "Greys")
p2 <- sequential2_hcl(9, h = 0, c = 0, l = c(95, 15), power = 1.3, fixup = FALSE)
p3 <- rev(sequential_hcl(9, h = 0, c = 0, l = c(15, 95), power = 1.3, fixup = FALSE))

## Purples
p1 <- brewer.pal(9, "Purples")
p2 <- sequential2_hcl(9, h = c(260, 280), c = c(0, 70, 55), l = c(95, 20), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(280, 260), c = c(60, 5), l = c(20, 95), power = c(0.7, 1.3), fixup = FALSE))

## Blues
p1 <- brewer.pal(9, "Blues")
p2 <- sequential2_hcl(9, h = c(220, 260), c = c(5, 75, 50), l = c(95, 20), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(260, 230), c = c(80, 10), l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE))

## Greens
p1 <- brewer.pal(9, "Greens")
p2 <- sequential2_hcl(9, h = c(115, 135), c = c(5, 70, 30), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(135, 120), c = c(50, 10), l = c(40, 95), power = c(0.4, 1.3), fixup = FALSE))

## Oranges
p1 <- brewer.pal(9, "Oranges")
p2 <- sequential2_hcl(9, h = c(45, 20), c = c(5, 150, 70), l = c(95, 25), power = 1.3, fixup = TRUE)
p3 <- rev(heat_hcl(9, h = c(20, 45), c = c(80, 5), l = c(35, 95), power = c(0.6, 1.3), fixup = FALSE))

## Reds
p1 <- brewer.pal(9, "Reds")
p2 <- sequential2_hcl(9, h = c(25, 10), c = c(5, 150, 60), l = c(95, 20), power = 1.3, fixup = TRUE)
p3 <- rev(heat_hcl(9, h = c(10, 40), c = c(80, 10), l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE))


########################################
## ColorBrewer: Sequential (multihue) ##
########################################

## BuPu
p1 <- brewer.pal(9, "BuPu")
p2 <- sequential2_hcl(9, h = c(200, 310), c = c(5, 65, 35), l = c(95, 20), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(300, 200), c = c(60, 0), l = c(25, 95), power = c(0.7, 1.3), fixup = FALSE))

## GnBu
p1 <- brewer.pal(9, "GnBu")
p2 <- sequential2_hcl(9, h = c(105, 255), c = c(10, 55, 45), l = c(95, 35), power = 1.7, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(260, 100), c = c(65, 10), l = c(25, 95), power = c(0.7, 1.7), fixup = FALSE))

## OrRd
p1 <- brewer.pal(9, "OrRd")
p2 <- sequential2_hcl(9, h = c(60, 10), c = c(10, 100, 80), l = c(95, 25), power = 1.6, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(10, 60), c = c(80, 10), l = c(25, 95), power = c(0.7, 1.6), fixup = FALSE))

## PuBu
p1 <- brewer.pal(9, "PuBu")
p2 <- sequential2_hcl(9, h = c(300, 240), c = c(5, 70, 30), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(250, 330), c = c(50, 5), l = c(30, 95), power = c(0.7, 1.3), fixup = FALSE))

## PuBuGn
p1 <- brewer.pal(9, "PuBuGn")
p2 <- sequential2_hcl(9, h = c(320, 130), c = c(5, 55, 20), l = c(95, 25), power = 1.5, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(160, 330), c = c(30, 5), l = c(35, 95), power = c(0.7, 1.3), fixup = FALSE))

## PuRd
p1 <- brewer.pal(9, "PuRd")
p2 <- sequential2_hcl(9, h = c(280, 370), c = c(5, 100, 60), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(370, 280), c = c(80, 5), l = c(25, 95), power = c(0.7, 1.3), fixup = FALSE))

## YlGn
p1 <- brewer.pal(9, "YlGn")
p2 <- sequential2_hcl(9, h = c(85, 145), c = c(20, 65, 25), l = c(95, 30), power = 1.7, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(140, 80), c = c(40, 10), l = c(35, 95), power = c(0.7, 1.7), fixup = FALSE))

## YlGnBu
p1 <- brewer.pal(9, "YlGnBu")
p2 <- sequential2_hcl(9, h = c(80, 260), c = c(25, 55, 30), l = c(95, 30), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(265, 80), c = c(60, 10), l = c(25, 95), power = c(0.7, 2), fixup = FALSE))

## YlOrBr
p1 <- brewer.pal(9, "YlOrBr")
p2 <- sequential2_hcl(9, h = c(90, 20), c = c(20, 80, 50), l = c(98, 25), power = 1.5, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(20, 85), c = c(60, 10), l = c(25, 95), power = c(0.7, 1.7), fixup = FALSE))

## YlOrRd
p1 <- brewer.pal(9, "YlOrRd")
p2 <- sequential2_hcl(9, h = c(90, 0), c = c(35, 90, 60), l = c(95, 25), power = 1.3, fixup = FALSE)
p3 <- rev(heat_hcl(9, h = c(10, 85), c = c(80, 10), l = c(25, 95), power = c(0.4, 1.3), fixup = FALSE))


##############################
## ColorBrewer: Qualitative ##
##############################

## Pastel1
p1 <- brewer.pal(5, "Pastel1")
p2 <- rainbow_hcl(5, c = 35, l = 85, fixup = FALSE)
p3 <- NULL

## Pastel2
p1 <- brewer.pal(5, "Pastel2")
p2 <- rainbow_hcl(5, c = 30, l = 85, fixup = FALSE)
p3 <- NULL

## Pastel2
p1 <- brewer.pal(5, "Dark2")
p2 <- rainbow_hcl(5, c = 50, l = 60, fixup = FALSE)
p3 <- NULL

## Set1
p1 <- brewer.pal(5, "Set1")
p2 <- rainbow_hcl(5, c = 50, l = 60, fixup = FALSE)
p3 <- NULL

## Set2
p1 <- brewer.pal(5, "Set2")
p2 <- rainbow_hcl(5, c = 60, l = 70, fixup = FALSE)
p3 <- NULL

## Set3
p1 <- brewer.pal(5, "Set3")
p2 <- rainbow_hcl(5, c = 50, l = 80, start = 10, end = 320, fixup = FALSE)
p3 <- NULL

## Paired
## -> not possible with rainbow_hcl()

