#' Lighten or Darken Colors by Modifying Luminance or Lightness in HLS or HCL Space
#' 
#' Transform a vector of given colors to the corresponding colors with luminance or lightness increased or 
#' decreased in HLS or HCL space, respectively.
#' 
#' The given colors are first transformed to RGB (either using \code{\link[colorspace]{hex2RGB}} or
#' \code{\link[grDevices]{col2rgb}}) and then to HLS or HCL (\code{\link[colorspace]{polarLUV}}).
#' In that space, the L component is adjusted and then the color is transformed back to a hexadecimal
#' string.
#' 
#' The function \code{lighten()} ligthens colors by either proportionally or absolutely increasing
#' the L component of the color. The mathematical formula used to transform L is \code{(1 + amount) * L} if
#' \code{method = "relative"}, and \code{L + amount} if \code{method = "absolute"}. If \code{amount}
#' is negative then colors are darkened rather than lightened. The function \code{darken()} is a convenience
#' function that multiplies \code{amount} with -1 and then calls \code{lighten()}.
#' 
#' Programmatically lightening and darkening colors can yield unexpected results. If you operate in HLS space
#' (the default here), it can happen that the overall amount of lightening or darkening appears non-uniform
#' among a group of colors that are lightened or darkened jointly. In HCL space, on the other hand, colors
#' can become either too gray or overly colorful (see examples). We recommend that if the results you obtain
#' don't look right to you, try the other colorspace and also try both the relative and the absolute methods.
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
#' @param space character string specifying the color space in which adjustment happens. Can be either \code{"HLS"} or \code{"HCL"}.
#' @param fixup logical If set to \code{TRUE}, colors that fall outside of the RGB color gamut are slightly modified
#'   by translating individual primary values so they lie between 0 and 255. If set to \code{FALSE}, out-of-gamut colors
#'   are replaced by \code{NA}.
#' @return A character vector with (s)RGB codings of the colors in the palette.
#' @seealso \code{\link[colorspace]{polarLUV}}, \code{\link[colorspace]{hex}}, \code{\link[colorspace]{desaturate}}
#' @keywords color
#' @examples
#' ## rainbow of colors and their desaturated counterparts
#' rainbow_hcl(12)
#' lighten(rainbow_hcl(12))
#' 
#' ## convenience demo function
#' pal <- function(col, border = "light gray") {
#'   n <- length(col)
#'   plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
#'        xlab = "", ylab = "")
#'   rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
#' }
#'
#' # darken light colors 
#' cl <- c("#CDE4F3","#E7F3D3","#F7F0C7","#EFCFE5","#D0D1E7")
#'
#' par(mfrow = c(3, 2), mar = rep(0, 4), oma = c(0, 0, 2, 0))
#' pal(cl); mtext("HLS")
#' pal(cl); mtext("HCL")
#' pal(darken(cl, 0.15))
#' pal(darken(cl, 0.15, space = "HCL"))
#' pal(darken(cl, 0.30))
#' pal(darken(cl, 0.30, space = "HCL"))
#' 
#' # lighten dark colors
#' cd <- c("#61A9D9", "#ADD668", "#E6D152", "#CE6BAF", "#797CBA")
#' 
#' par(mfrow = c(3, 2), mar = rep(0, 4), oma = c(0, 0, 2, 0))
#' pal(cd); mtext("HLS")
#' pal(cd); mtext("HCL")
#' pal(lighten(cd, 0.15))
#' pal(lighten(cd, 0.15, space = "HCL"))
#' pal(lighten(cd, 0.30))
#' pal(lighten(cd, 0.30, space = "HCL"))
#' 
#' @export lighten
#' @importFrom grDevices col2rgb
lighten <- function(col, amount = 0.1,
                    method = c("relative", "absolute"), space = c("HLS", "HCL"), fixup = TRUE)
{
  ## shortcut to make sure colors are not modified if amount is set to zero
  if (amount == 0) {
    return(col)
  }
  
  ## method
  space <- match.arg(space, c("HLS", "HCL"))
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
