if(FALSE) {

## MAX CHROMA SEARCH IN INTEGERS

## set up HCL grid
d <- expand.grid(h = 0:360, l = 0:100, c = 0:200)

## force chroma to zero at the edges
d[d$l == 0 | d$l == 100, "c"] <- 0

## check whether ok in RGB
d$ok <- !is.na(hcl(d$h, d$c, d$l, fixup = FALSE))

## checking via colorspace is almost identical
d$ok2 <- !is.na(colorspace::hex(colorspace::polarLUV(as.numeric(d$l), as.numeric(d$c), as.numeric(d$h)), fixup = FALSE))
xtabs(~ ok + ok2, data = d)

## keep only ok values and extract the maximum chroma value
d <- d[d$ok, ]
d <- aggregate(d, list(d$h, d$l), max)
d <- d[, -c(1:2, 6:7)]


## MAX CHROMA SEARCH IN 0.01

## refine grid
d <- data.frame(h = rep(d$h, each = 101), l = rep(d$l, each = 101),
  c = as.numeric(t(outer(d$c, 0:100/100, "+"))))
#d$c <- pmin(d$c, 100)

## force chroma to zero at the edges
d[d$l == 0 | d$l == 100, "c"] <- 0
  
## check whether ok in RGB
d$ok <- !is.na(hcl(d$h, d$c, d$l, fixup = FALSE))

## checking via colorspace is almost identical
d$ok2 <- !is.na(colorspace::hex(colorspace::polarLUV(as.numeric(d$l), as.numeric(d$c), as.numeric(d$h)), fixup = FALSE))
xtabs(~ ok + ok2, data = d)

## keep only ok values and extract the maximum chroma value
d <- d[d$ok, ]
d <- aggregate(d, list(d$h, d$l), max)
d <- d[, -c(1:2, 6:7)]


## STORE MAX CHROMA TABLE

## turn into named vector
max_chroma_table <- structure(d$c,
  .Names = paste(d$h, d$l, sep = "-"))
save(max_chroma_table, file = "max_chroma_table.rda")

}
