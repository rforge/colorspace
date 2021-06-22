#' W3C Contrast Ratio
#' 
#' Compute (and visualize) the contrast ratio of pairs of colors, as defined by the World Wide
#' Web Consortium (W3C).
#' 
#' The W3C Content Accessibility Guidelines (WCAG) recommend a contrast
#' ratio of at least 4.5 for the color of regular text on the background color, and
#' a ratio of at least 3 for large text. See \url{https://www.w3.org/TR/WCAG21/#contrast-minimum}.
#'
#' The contrast ratio is defined in \url{https://www.w3.org/TR/WCAG21/#dfn-contrast-ratio}
#' as \code{(L1 + 0.05) / (L2 + 0.05)} where \code{L1} and \code{L2} are the relative luminances
#' (see \url{https://www.w3.org/TR/WCAG21/#dfn-relative-luminance}) of the lighter and darker
#' colors, respectively. The relative luminances are weighted sums of scaled sRGB coordinates:
#' \code{0.2126 * R + 0.7152 * G + 0.0722 * B} where each of \code{R}, \code{G}, and \code{B}
#' is defined as \code{ifelse(RGB <= 0.03928, RGB/12.92, ((RGB + 0.055)/1.055)^2.4 )} based on
#' the \code{RGB} coordinates between 0 and 1.
#' 
#' @param col vector of any of the three kind of R colors, i.e., either a color
#' name (an element of \code{\link[grDevices]{colors}}), a hexadecimal string
#' of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}}), or an integer \code{i} meaning
#' \code{palette()[i]}.
#' @param col2 vector of background colors (as for \code{col} above). If \code{NULL},
#' all subsequent pairs from \code{col} are used.
#' @param plot logical indicating whether the contrast ratios should also be
#' visualized by simple color swatches. Can also be a vector of length 2, indicating
#' whether the foreground color should be visualized on the background color and/or
#' the background color on the foreground color.
#' @return A numeric vector with the contrast ratios is returned (invisibly, if \code{plot} is \code{TRUE}).
#' @seealso \code{\link[colorspace]{desaturate}}
#' @references W3C (2018). \dQuote{Web Content Accessibility Guidelines (WCAG) 2.1.}
#' \url{https://www.w3.org/TR/WCAG21/}
#' @keywords color
#' @examples
#' # check contrast ratio of default palette on white background
#' contrast_ratio(palette(), "white")
#'
#' # visualize contrast ratio of default palette on white and black background
#' contrast_ratio(palette(), "white", plot = TRUE)
#' contrast_ratio(palette()[-1], "black", plot = TRUE)
#' @export contrast_ratio
#' @importFrom grDevices rgb
#' @importFrom graphics par
#' @importFrom graphics rect
#' @importFrom graphics text

contrast_ratio <- function(col, col2 = NULL, plot = FALSE) {
  ## https://www.w3.org/TR/WCAG21/#dfn-contrast-ratio
  ## determine number of color pairs
  n <- if(is.null(col2)) length(col) - 1L else max(c(length(col), length(col2)))
  if(n < 1L) stop("need to specify at least two colors")

  ## suitably recycle colors if necessary
  if(is.null(col2)) {
    col2 <- col[-1L]
    col <- col[-length(col)]
  }
  if(!(length(col) == n && length(col2) == n)) {
    col <- rep_len(col, n)
    col2 <- rep_len(col2, n)
  }
  
  ## compute contrast ratio
  ratio <- (relative_luminance(col) + 0.05)/(relative_luminance(col2) + 0.05)
  ratio[ratio < 1] <- 1/ratio[ratio < 1]

  ## optionally visualize
  plot <- rep_len(plot, 2L)
  if(any(plot)) {
    opar <- par(mar = rep(0.5, 4))
    on.exit(par(opar))
    plot(0, 0, xlim = c(0, sum(plot)), ylim = c(0, n), type = "n", axes = FALSE, xlab = "", ylab = "")
    if(plot[1L]) {
      rect(0L, 1L:n - 1L, 1L, 1L:n - 0.05, col = col, border = "transparent")
      text(0.5, 1L:n - 0.525, format(round(ratio, digits = 2L), nsmall = 2L), cex = 2, col = col2)
    }
    if(plot[2L]) {
      rect(0L + plot[1L], 0L:(n - 1L), 1L + plot[1L], 0L:(n - 1L) + 0.95, col = col2, border = "transparent")
      text(0.5 + plot[1L], 1L:n - 0.525, format(round(ratio, digits = 2L), nsmall = 2L), cex = 2, col = col)
    }
    invisible(ratio)
  } else {
    return(ratio)
  }
}

relative_luminance <- function(col) {
  ## https://www.w3.org/TR/WCAG21/#dfn-relative-luminance
  rgb <- t(col2rgb(col))/255
  rgb[] <- ifelse(rgb <= 0.03928, rgb/12.92, ((rgb + 0.055)/1.055)^2.4)
  as.numeric(rgb %*% c(0.2126, 0.7152, 0.0722))
}
