#' Lighten or Darken Colors by Modifying Luminance or Lightness in HCL or HLS Space
#' 
#' Transform a vector of given colors to the corresponding colors with luminance or lightness increased or 
#' decreased in HCL or HLS space, respectively. When darkening colors, chroma may be decreased as well if needed.
#' 
#' The given colors are first transformed to RGB (either using \code{\link[colorspace]{hex2RGB}} or
#' \code{\link[grDevices]{col2rgb}}) and then to HCL (\code{\link[colorspace]{polarLUV}}) or HLS.
#' In that space, the L component is adjusted and then the color is transformed back to a hexadecimal
#' string.
#' 
#' The function \code{lighten()} ligthens colors by either proportionally or absolutely increasing
#' the L component of the color. The mathematical formula used to transform L is \code{(1 + amount) * L} if
#' \code{method = "relative"}, and \code{L + amount} if \code{method = "absolute"}. If \code{amount}
#' is negative then colors are darkened rather than lightened. The function \code{darken()} is a convenience
#' function that multiplies \code{amount} with -1 and then calls \code{lighten()}.
#' 
#' @param col vector of any of the three kind of R colors, i.e., either a color
#' name (an element of \code{\link[grDevices]{colors}}), a hexadecimal string
#' of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}}), or an integer \code{i} meaning
#' \code{palette()[i]}.
#' @param amount numeric specifying the amount of lightening. This is applied either
#' multiplicatively or additively to the luminance value, depending on the
#' setting of \code{method} (either relative or absolute). Negative numbers
#' cause darkening.
#' @param method character string specifying the adjustment method. Can be either \code{"relative"} or \code{"absolute"}.
#' @param space character string specifying the color space in which adjustment happens. Can be either \code{"HCL"} or \code{"HLS"}.
#' @return A character vector with (s)RGB codings of the colors in the palette.
#' @seealso \code{\link[colorspace]{polarLUV}}, \code{\link[colorspace]{hex}}, \code{\link[colorspace]{desaturate}}
#' @keywords color
#' @examples
#' ## rainbow of colors and their desaturated counterparts
#' rainbow_hcl(12)
#' lighten(rainbow_hcl(12))
#' 
#' ## convenience demo function
#' wheel <- function(col, radius = 1, ...)
#'   pie(rep(1, length(col)), col = col, radius = radius, ...) 
#' 
#' ## compare original, lightened, and darkened colors
#' par(mar = rep(0, 4), mfrow = c(2, 3))
#' ## rainbow color wheel
#' wheel(rainbow_hcl(12))
#' wheel(lighten(rainbow_hcl(12), amount = 0.1))
#' wheel(lighten(rainbow_hcl(12), amount = 0.3)) 
#' wheel(rainbow_hcl(12))
#' wheel(darken(rainbow_hcl(12), amount = 0.1))
#' wheel(darken(rainbow_hcl(12), amount = 0.3))
#' 
#' @export lighten
#' @importFrom grDevices col2rgb
lighten <- function(col, amount = 0.1,
                    method = c("relative", "absolute"), space = c("HCL", "HLS"), fixup = TRUE)
{
  ## shortcut to make sure colors are not modified if amount is set to zero
  if (amount == 0) {
    return(col)
  }
  
  ## method
  space <- match.arg(space, c("HCL", "HLS"))
  method <- match.arg(method, c("relative", "absolute"))
  
  ## number of colors
  n <- max(c(length(col), length(amount)))
  col <- rep_len(col, length.out = n)
  amount <- rep_len(amount, length.out = n)
  
  ## col has to be hex code, otherwise col2rgb is used
  if(is.character(col) &&
     (all(substr(col, 1L, 1L) == "#") & all(nchar(col) %in% c(7L, 9L))))
  {
    ## extract alpha from hex (if any)
    alpha <- substr(col, 8L, 9L)
    ## retain only RGB in hex
    col <- substr(col, 1L, 7L)
    ## convert to colorspace::RGB
    col <- hex2RGB(col)
  } else {
    col <- col2rgb(col, alpha = TRUE)
    ## extract alpha values (if non-FF)
    alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
    alpha[alpha == "FF"] <- ""
    ## retain only RGB
    col <- RGB(t(col[1L:3L, ])/255)
  }
  
  if(space == "HCL") {
    ## convert to HCL and remove chroma
    col <- as(col, "polarLUV")
    ## fix-up extreme luminance cases
    col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))
    
    ## transform luminance
    Lold <- col@coords[, "L"]
    col@coords[, "L"] <- if(method == "relative") {
      Lold * (1 + amount)
    } else {
      Lold + amount * 100
    }
    col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))

    ## transform chroma correspondingly (relative to maximum chroma possible)
    col@coords[, "C"] <- col@coords[, "C"]/ceiling(max_chroma(col@coords[, "H"], Lold) + 1e-8) *
      max_chroma(col@coords[, "H"], col@coords[, "L"], floor = TRUE)
    # check once more that resulting chroma is within appropriate bounds
    col@coords[, "C"] <- pmin(max_chroma(col@coords[, "H"], col@coords[, "L"], floor = TRUE),
                              pmax(0, col@coords[, "C"]))
  } else {
    col <- as(col, "HLS")
    col@coords[, "L"] <- if(method == "relative") {
      col@coords[, "L"] * (1 + amount)
    } else {
      col@coords[, "L"] + amount
    }
    col@coords[, "L"] <- pmin(1, pmax(0, col@coords[, "L"]))
  }
  
  ## convert back to hex and add alpha again (if any)
  col <- hex(col, fixup = fixup)
  col[!is.na(col)] <- paste(col[!is.na(col)], alpha[!is.na(col)], sep = "")
  return(col)
}


#' @rdname lighten
#' @param ... Other parameters handed to the function \code{lighten()}.
#' @export
darken <- function(col, amount = 0.1, ...)
{
  lighten(col, -1*amount, ...)
}
