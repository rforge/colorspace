## packages
library("colorspace") ## 1.4-1
library("png")


# manipulate the image by adding new color palette
noaa_pal <- toupper(c("#7dfe00","#00cd00","#008b00", "#104d8b","#1d90fc","#01b1ee",
  "#00eeee","#8967cd","#912cee", "#8b008b","#8b0000", "#cd0000", "#f04100", "#8b008b"))

manipulate <- function(img, col = sequential_hcl(13, rev = TRUE), repl = noaa_pal)
{
  col <- c(col, "#FFFFFF")

  # - Loading RGB values of the original image
  R <- as.vector(img[,,1])
  G <- as.vector(img[,,2])
  B <- as.vector(img[,,3])
  # - Convert to hex colors
  hcols <- hex(sRGB(R = R, G = G, B = B))

  # - Now converting
  for(i in seq_along(repl)) hcols[hcols == repl[i]] <- col[i]

  # - Converting back to sRGB
  RGB <- hex2RGB(hcols)

  # - Add new RGB values to original image
  RGB <- attr(RGB,"coords")
  img[,,1] <- matrix(RGB[, "R"], dim(img)[1], dim(img)[2])
  img[,,2] <- matrix(RGB[, "G"], dim(img)[1], dim(img)[2])
  img[,,3] <- matrix(RGB[, "B"], dim(img)[1], dim(img)[2])

  return(img) 
}

if(FALSE) {

# - Reading the "original" (slightly modified) image of the noaa
img <- readPNG("noaa-orig.png")

# - Desaturated ones
writePNG(manipulate(img, col = deutan(noaa_pal)),     target = "noaa-orig-deutan.png")
writePNG(manipulate(img, col = desaturate(noaa_pal)), target = "noaa-orig-desaturate.png")

bams_pal <- sequential_hcl(13, "Purple-Yellow", rev = TRUE) ## slightly modified compared to Figure 4
writePNG(manipulate(img, col = bams_pal),             target = "noaa-purpleyellow.png")
writePNG(manipulate(img, col = deutan(bams_pal)),     target = "noaa-purpleyellow-deutan.png")
writePNG(manipulate(img, col = desaturate(bams_pal)), target = "noaa-purpleyellow-desaturate.png")

png(file = "noaa-orig-spectrum.png", width = 471, height = 315, pointsize = 13.5)
specplot(noaa_pal, lwd = 3)
dev.off()

png(file = "noaa-purpleyellow-spectrum.png", width = 471, height = 315, pointsize = 13.5)
specplot(bams_pal, lwd = 3)
dev.off()

}
