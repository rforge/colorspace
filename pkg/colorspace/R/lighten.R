#' Algorithmically Lighten or Darken Colors
#' 
#' The functions \code{lighten} and \code{darken} take a vector of R colors and adjust the colors such that
#' they appear lightened or darkened, respectively.
#' 
#' The color adjustment can be calculated in three different color spaces. First, if \code{space = "HCL"},
#' the colors are transformed to HCL, (\code{\link[colorspace]{polarLUV}}), the luminance component L
#' is adjusted, and then the colors are transformed back to a hexadecimal RGB string. Second, if 
#' \code{space = "HLS"}, the colors are transformed to HLS, the lightness component L is adjusted, and
#' then the color is transformed back to a hexadecimal RGB string. Finally, if \code{space = "combined"},
#' the colors are first adjusted in both the HCL and HLS spaces. Then, the adjusted HLS colors are converted
#' into HCL, and then the chroma components of the adjusted HLS colors are copied to the adjusted HCL colors.
#' Thus, in effect, the combined model adjusts luminance in HCL space but chroma in HLS space. We have found
#' that, in general, \code{space = "HCL"} performs best for lightening colors and \code{space = "combined"}
#' performs best for darkening colors, and these are the default settings for \code{lighten} and \code{darken},
#' respectively.
#' 
#' Regardless of the chosen color space, the adjustment of the L component can occur by two methods, relative
#' (the default) and absolute. Under the relative method, the adjustment is \code{100 - (100 - L) * (1 - amount)} when
#' lightening colors and \code{L * (1 - amount)} when darkening colors. Under the absolute method, the adjustment
#' is \code{L +/- 100 * amount} when lightening/darkening colors.
#' 
#' Programmatically lightening and darkening colors can yield unexpected results (see examples). In HCL space,
#' colors can become either too gray or overly colorful. By contrast, in HLS space it can happen that the
#' overallamount of lightening or darkening appears to be non-uniform among a group of colors that are 
#' lightened or darkened jointly, and again, colors can become either too gray or overly colorful. We
#' recommend to try different color spaces if the default space for the chosen function (\code{lighten} 
#' or \code{darken}) does not look right in a specific application.
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
#' ## convenience demo function
#' pal <- function(col, border = "light gray") {
#'   n <- length(col)
#'   plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE,
#'        xlab = "", ylab = "")
#'   rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
#' }
#' 
#' # lighten dark colors, example 1
#' cl <- rainbow_hcl(5)
#' par(mfrow = c(3, 3), mar = rep(0, 4), oma = c(0, 0, 2, 0))
#' pal(cl); mtext("HCL")
#' pal(cl); mtext("HLS")
#' pal(cl); mtext("combined")
#' pal(lighten(cl, 0.15))
#' pal(lighten(cl, 0.15, space = "HLS"))
#' pal(lighten(cl, 0.15, space = "combined"))
#' pal(lighten(cl, 0.3))
#' pal(lighten(cl, 0.3, space = "HLS"))
#' pal(lighten(cl, 0.3, space = "combined"))
#'
#' # lighten dark colors, example 2
#' cl <- c("#61A9D9", "#ADD668", "#E6D152", "#CE6BAF", "#797CBA")
#' par(mfrow = c(3, 3), mar = rep(0, 4), oma = c(0, 0, 2, 0))
#' pal(cl); mtext("HCL")
#' pal(cl); mtext("HLS")
#' pal(cl); mtext("combined")
#' pal(lighten(cl, 0.15))
#' pal(lighten(cl, 0.15, space = "HLS"))
#' pal(lighten(cl, 0.15, space = "combined"))
#' pal(lighten(cl, 0.30))
#' pal(lighten(cl, 0.30, space = "HLS"))
#' pal(lighten(cl, 0.30, space = "combined"))
#' 
#' # darken light colors, example 1
#' cl <- rainbow_hcl(5, l = 90, c = 35)
#' par(mfrow = c(3, 3), mar = rep(0, 4), oma = c(0, 0, 2, 0))
#' pal(cl); mtext("combined")
#' pal(cl); mtext("HCL")
#' pal(cl); mtext("HLS")
#' pal(darken(cl, 0.15))
#' pal(darken(cl, 0.15, space = "HCL"))
#' pal(darken(cl, 0.15, space = "HLS"))
#' pal(darken(cl, 0.30))
#' pal(darken(cl, 0.30, space = "HCL"))
#' pal(darken(cl, 0.30, space = "HLS"))
#'
#' # darken light colors, example 2 
#' cl <- c("#CDE4F3","#E7F3D3","#F7F0C7","#EFCFE5","#D0D1E7")
#' par(mfrow = c(3, 3), mar = rep(0, 4), oma = c(0, 0, 2, 0))
#' pal(cl); mtext("combined")
#' pal(cl); mtext("HCL")
#' pal(cl); mtext("HLS")
#' pal(darken(cl, 0.15))
#' pal(darken(cl, 0.15, space = "HCL"))
#' pal(darken(cl, 0.15, space = "HLS"))
#' pal(darken(cl, 0.30))
#' pal(darken(cl, 0.30, space = "HCL"))
#' pal(darken(cl, 0.30, space = "HLS"))
#' @export lighten
#' @importFrom grDevices rgb
#' @importFrom grDevices col2rgb
lighten <- function(col, amount = 0.1,
                    method = c("relative", "absolute"), space = c("HCL", "HLS", "combined"), fixup = TRUE)
{
  ## method
  space <- match.arg(space, c("HCL", "HLS", "combined"))
  method <- match.arg(method, c("relative", "absolute"))
  
  ## number of colors
  n <- max(c(length(col), length(amount)))
  col <- rep_len(col, length.out = n)
  amount <- rep_len(amount, length.out = n)
  
  ## save original colors for later, to substitute any cases with amount == 0
  col_orig <- col
  
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
    # save original colors in hex format, in case some were specified as 
    # named colors or as palette entries
    col_orig <- rgb(t(col), maxColorValue = 255)
    ## extract alpha values (if non-FF)
    alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
    alpha[alpha == "FF"] <- ""
    ## retain only RGB
    col <- RGB(t(col[1L:3L, ])/255)
  }
  
  if (space == "HCL") {
    ## *** darkening/lightening in HCL space ***
    
    ## convert to HCL
    col <- as(col, "polarLUV")
    ## fix-up extreme luminance cases
    col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))

    ## adjust luminance
    Lold <- col@coords[, "L"]
    col@coords[, "L"] <- if(method == "relative") {
      (amount >= 0) * (100 - (100 - Lold) * (1 - amount)) +
        (amount < 0) * Lold * (1 + amount)
    } else {
      Lold + amount * 100
    }
    col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))
    
    ## transform chroma correspondingly (relative to maximum chroma possible)
    ## It seems better to not apply this adjustment here. Lighened colors look better without it, 
    ## and darkened colors look better under the combined model.
    #col@coords[, "C"] <- col@coords[, "C"]/ceiling(max_chroma(col@coords[, "H"], Lold) + 1e-8) *
    #  max_chroma(col@coords[, "H"], col@coords[, "L"], floor = TRUE)
  
    ## check that resulting chroma is within appropriate bounds
    col@coords[, "C"] <- pmin(max_chroma(col@coords[, "H"], col@coords[, "L"], floor = TRUE),
                              pmax(0, col@coords[, "C"]))
  } 
  else if (space == "HLS") {
    ## *** darkening/lightening in HLS space ***
    
    col <- as(col, "HLS")
    col@coords[, "L"] <- if(method == "relative") {
      (amount >= 0) * (1 - (1 - col@coords[, "L"]) * (1 - amount)) +
        (amount < 0) * col@coords[, "L"] * (1 + amount)
    } else {
      col@coords[, "L"] + amount
    }
    col@coords[, "L"] <- pmin(1, pmax(0, col@coords[, "L"]))
  } else {
    ## *** darkening/lightening in combined space ***
    
    ## first do adjustment in HLS space
    colHLS <- as(col, "HLS")
    colHLS@coords[, "L"] <- if(method == "relative") {
      (amount >= 0) * (1 - (1 - colHLS@coords[, "L"]) * (1 - amount)) +
        (amount < 0) * colHLS@coords[, "L"] * (1 + amount)
    } else {
      colHLS@coords[, "L"] + amount
    }
    colHLS@coords[, "L"] <- pmin(1, pmax(0, colHLS@coords[, "L"]))
    
    colHLSHCL <- as(as(colHLS, "RGB"), "polarLUV")
    
    ## now do adjustment in HCL space
    col <- as(col, "polarLUV")
    ## fix-up extreme luminance cases
    col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))

    ## transform luminance
    Lold <- col@coords[, "L"]
    col@coords[, "L"] <- if(method == "relative") {
      (amount >= 0) * (100 - (100 - Lold) * (1 - amount)) +
        (amount < 0) * Lold * (1 + amount)
    } else {
      Lold + amount * 100
    }
    
    ## fix-up L and copy C over from HLS-converted color
    col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))
    #col@coords[, "H"] <- colHLSHCL@coords[, "H"]
    col@coords[, "C"] <- colHLSHCL@coords[, "C"]
    
    ## make sure chroma is in allowed range
    col@coords[, "C"] <- pmin(max_chroma(col@coords[, "H"], col@coords[, "L"], floor = TRUE), col@coords[, "C"])
  }
  
  ## convert back to hex and add alpha again (if any)
  col <- hex(col, fixup = fixup)
  col[!is.na(col)] <- paste(col[!is.na(col)], alpha[!is.na(col)], sep = "")
  
  ## return original colors whenever amount == 0
  col[amount == 0] <- col_orig[amount == 0]
  
  return(col)
}


#' @rdname lighten
#' @param ... Other parameters handed to the function \code{lighten()}.
#' @export
darken <- function(col, amount = 0.1, space = "combined", ...)
{
  lighten(col, amount = -1*amount, space = space, ...)
}
