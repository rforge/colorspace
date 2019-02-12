library("colorspace")

source("../../paper/Palettes/Nigeria/bnd.R")
m <- read.bnd("../../paper/Palettes/Nigeria/nigeria37.bnd")
fit <- read.table("../../paper/Palettes/Nigeria/reml_purespatial_f_district_spatial.res", header = TRUE)
fit <- fit[order(fit$district),]

nigeria_map <- function(col, file) {

  png(file = file, height = 600 + 60, width = 700, res = 72)

  layout(rbind(1, c(3, 2, 4)), heights = c(600, 60), widths = c(300, 800, 300))
  par(mar = c(0.5, 0, 0, 0), oma = c(2, 1.9, 0, 1.9))

  plot(m, z = fit$pmode, fill = rev(col),
    zlim = c(-0.65, 0.65), legend = FALSE)

  plot(0, 0, type="n", xlim = c(-0.65, 0.65), ylim = c(0, 1),
    xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab="")
  abline(v = seq(from = -0.65, to = 0.65, length.out = 99), lwd = 6, col = rev(col))
  box()
  axis(1, cex.axis = 2)

  dev.off()
}

nigeria_rainbow <- rev(rainbow(99, start = 0, end = 1/3))
nigeria_map(nigeria_rainbow,         "nigeria-rainbow.png")
nigeria_map(protan(nigeria_rainbow), "nigeria-rainbow-protan.png")

## nigeria_csda <- diverging_hcl(99, h = c(300, 120), c = 60, l = c(45, 90), power = 1.2)
## nigeria_map(nigeria_csda,         "nigeria-csda.png")
## nigeria_map(protan(nigeria_csda), "nigeria-csda-protan.png")

nigeria_purplegreen <- diverging_hcl(99, "Purple-Green", rev = TRUE)
nigeria_map(nigeria_purplegreen,         "nigeria-purplegreen.png")
nigeria_map(protan(nigeria_purplegreen), "nigeria-purplegreen-protan.png")

png(file = "nigeria-rainbow-spectrum.png", height = 600 + 60, width = 700, pointsize = 20)
specplot(nigeria_rainbow, lwd = 3)
dev.off()

png(file = "nigeria-purplegreen-spectrum.png", height = 600 + 60, width = 700, pointsize = 20)
specplot(nigeria_purplegreen, lwd = 3)
dev.off()
