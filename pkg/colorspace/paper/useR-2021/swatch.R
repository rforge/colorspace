palette.swatch <- function(palette = palette.pals(), n = 8, nrow = 8,
                           border = "black", cex = 1, ...)
{
     cols <- sapply(palette, palette.colors, n = n, recycle = TRUE)
     ncol <- ncol(cols)
     nswatch <- min(ncol, nrow)
     op <- par(mar = rep(0.1, 4),
               mfrow = c(1, min(5, ceiling(ncol/nrow))),
     	       cex = cex, ...)
     on.exit(par(op))
     while (length(palette)) {
 	subset <- seq_len(min(nrow, ncol(cols)))
 	plot.new()
 	plot.window(c(0, n), c(0.25, nrow + 0.25))
 	y <- rev(subset)
 	text(0, y + 0.1, palette[subset], adj = c(0, 0))
 	y <- rep(y, each = n)
 	rect(rep(0:(n-1), n), y, rep(1:n, n), y - 0.5,
 	     col = cols[, subset], border = border)
 	palette <- palette[-subset]
 	cols    <- cols [, -subset, drop = FALSE]
     }
}

hcl.swatch <- function(palette = NULL, type = NULL, n = 5, nrow = 11,
  border = if (n < 15) "black" else NA, ...) {
    if(is.null(palette)) palette <- hcl.pals(type)
    cols <- sapply(palette, hcl.colors, n = n)
    ncol <- ncol(cols)
    nswatch <- min(ncol, nrow)

    par(mar = rep(0.1, 4),
        mfrow = c(1, min(5, ceiling(ncol/nrow))),
        cex = 0.7, ...)

    while (length(palette)) {
        subset <- 1:min(nrow, ncol(cols))
        plot.new()
        plot.window(c(0, n), c(0, nrow + 1))
        text(0, rev(subset) + 0.1, palette[subset], adj = c(0, 0))
        y <- rep(subset, each = n)
        rect(rep(0:(n-1), n), rev(y), rep(1:n, n), rev(y) - 0.5,
             col = cols[, subset], border = border)
        palette <- palette[-subset]
        cols <- cols[, -subset, drop = FALSE]
    }

    par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1), cex = 1)
}

## height=5, width=12
palette.swatch(palette.pals()[-1], nrow = 5, oma = c(0, 0, 1.5, 0))
mtext("  Qualitative (palette.colors)", adj = 0, cex = 1.5, outer = TRUE, line = -2.5)

## height=10, width=8
hcl.swatch(type = "sequential", nrow = 14)
mtext(" Sequential (hcl.colors)", adj = 0, cex = 1.5, outer = TRUE, line = -2.5)

## height=8, width=8
hcl.swatch(c(hcl.pals("diverging"), hcl.pals("divergingx")), nrow = 9)
mtext(" Diverging (hcl.colors)", adj = 0, cex = 1.5, outer = TRUE, line = -2.5)
