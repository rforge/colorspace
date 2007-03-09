## load visualization functions
library("colorspace")
source("visualizations.R")

## show color spaces
par(mfrow = c(2, 3))
for(l in seq(5, 95, length = 6)) hue_chroma_plane(l = l)
for(v in seq(10, 100, length = 6)) hue_saturation_plane(v = v)

for(h in seq(0, 300, by = 60)) chroma_luminance_plane(h = h)
for(h in seq(0, 300, by = 60)) saturation_value_plane(h = h)
par(mfrow = c(1, 1))

## palette selection in HCL space
hue_chroma_plane(axes = FALSE, inner = 50)
for(h in seq(0, 300, by = 60)) hue_chroma_point(h = h, c = 50)

chroma_luminance_plane()
lines(c(2, 2), c(30, 90), lty = 2)
for(l in c(30, 60, 90)) chroma_luminance_point(c = 2, l = l)
lines(c(2, 98), c(90, 50), lty = 2)
chroma_luminance_point(c = 2, l = 90)
chroma_luminance_point(c = 50, l = 70)
chroma_luminance_point(c = 98, l = 50)
