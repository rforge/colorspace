#' Desaturate Colors by Chroma Removal in HCL Space
#' 
#' Transform a vector of given colors to the corresponding colors with chroma
#' removed (collapsed to zero) in HCL space.
#' 
#' Given colors are first transformed to RGB (either using
#' \code{\link[colorspace]{hex2RGB}} or \code{\link[grDevices]{col2rgb}}) and
#' then to HCL (\code{\link[colorspace]{polarLUV}}).  In HCL, chroma is removed
#' (i.e., collapsed to zero) and then the color is transformed back to a
#' hexadecimal string.
#' 
#' @param col vector of any of the three kind of R colors, i.e., either a color
#' name (an element of \code{\link[grDevices]{colors}}), a hexadecimal string
#' of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}}), or an integer \code{i} meaning
#' \code{palette()[i]}.
#' @param amount numeric specifying the amount of desaturation where \code{1}
#' corresponds to complete desaturation, \code{0} to no desaturation, and
#' values in between to partial desaturation.
#' @return A character vector with (s)RGB codings of the colors in the palette.
#' @seealso \code{\link[colorspace]{polarLUV}}, \code{\link[colorspace]{hex}}
#' @keywords color
#' @examples
#' ## rainbow of colors and their desaturated counterparts
#' rainbow_hcl(12)
#' desaturate(rainbow_hcl(12))
#' 
#' ## convenience demo function
#' wheel <- function(col, radius = 1, ...)
#'   pie(rep(1, length(col)), col = col, radius = radius, ...) 
#' 
#' ## compare base and colorspace palettes
#' ## (in color and desaturated)
#' par(mar = rep(0, 4), mfrow = c(2, 2))
#' ## rainbow color wheel
#' wheel(rainbow_hcl(12))
#' wheel(rainbow(12))
#' wheel(desaturate(rainbow_hcl(12)))
#' wheel(desaturate(rainbow(12)))
#' @export desaturate

desaturate <- function(col, amount = 1) {
  ## col has to be hex code, otherwise col2rgb is used
  if(is.character(col) &&
    (all(substr(col, 1L, 1L) == "#") & all(nchar(col) %in% c(7L, 9L))))
  {
    ## extract alpha from hex (if any)
    alpha <- substr(col, 8L, 9L)
    ## retain only RGB in hex
    col <- substr(col, 1L, 7L)
    ## convert to colorspace::sRGB
    col <- hex2RGB(col)
  } else {
    col <- col2rgb(col, alpha = TRUE)
    ## extract alpha values (if non-FF)
    alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
    alpha[alpha == "FF"] <- ""
    ## retain only RGB
    col <- sRGB(t(col[1L:3L, ])/255)
  }
  
  ## convert to HCL and fix-up extreme luminance cases
  col <- as(col, "polarLUV")
  col@coords[, 2L] <- (1 - amount) * col@coords[, 2L]

  ## fix-up extreme luminance cases
  col@coords[col@coords[, 1L] <= 0 | col@coords[, 1L] >= 100, 2L:3L] <- 0
  
  ## convert back to hex and add alpha again (if any)
  col <- hex(col)
  col <- paste(col, alpha, sep = "")
  return(col)
}
