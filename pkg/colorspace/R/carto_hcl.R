#' CARTO Sequential and Diverging Palettes Emulated by HCL Palettes
#' 
#' Sequential and diverging CARTO color palettes, emulated by sequential
#' HCL palettes (combining two sequential palettes for diverging palettes).
#' 
#' The \code{carto_hcl} function simply calls \code{\link{sequential_hcl}}
#' with a prespecified set of hue/chorma/luminance parameters that have
#' been chosen such that the CARTO color palettes (\url{https://carto.com/carto-colors/})
#' are approximately emulated. In case of diverging CARTO palettes two
#' sequential HCL palettes are combined, possibly with rather unbalanced
#' chroma/luminance specifications.
#'
#' Available sequential palettes: RedOr, OrYel, Peach, PinkYl, Mint, BluGrn,
#' DarkMint, Emrld, ag_GrnYl, BluYl, Teal, TealGrn, PurpOr, Sunset, Magenta.
#'
#' Available diverging palettes: ArmyRose, Fall, Geyser, Temps, Tropic, Earth.
#'
#' Furthermore some CARTO palettes cannot be emulated well with sequential
#' HCL palettes, typically due to requiring non-monotonic chroma sequences
#' or sometimes nonlinear hue sequences. The CARTO palettes not available
#' in \code{carto_hcl} are: Burg, BurgYl, Purp, SunsetDark, ag_Sunset, BrwnYl, TealRose.
#' 
#' @param n the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.
#' @param palette character with the name (see details).
#' @param \dots arguments passed to \code{\link{sequential_hcl}}.
#' @param rev logical. Should the palette be reversed?
#' @param h1 numeric. Starting hue coordinate.
#' @param h2 numeric. Either ending hue coordinate (for sequential palettes)
#' or center hue coordinate (for diverging palettes).
#' @param h3 numeric. Either \code{NA} (for sequential palettes) or ending hue
#' coordinate (for diverging palettes).
#' @param c1 numeric. Chroma coordinate corresponding to \code{h1}.
#' @param c2 numeric. Chroma coordinate corresponding to \code{h2}.
#' @param c3 numeric. Chroma coordinate corresponding to \code{h3} (if any).
#' @param l1 numeric. Luminance coordinate corresponding to \code{h1}.
#' @param l2 numeric. Luminance coordinate corresponding to \code{h2}.
#' @param l3 numeric. Luminance coordinate corresponding to \code{h3} (if any).
#' @param p1 numeric. Power parameter for chroma coordinates in (first) sequential palette.
#' @param p2 numeric. Power parameter for luminance coordinates in (first) sequential palette.
#' @param p3 numeric. Power parameter for chroma coordinates in second sequential palette for diverging palettes.
#' @param p4 numeric. Power parameter for luminance coordinates in second sequential palette for diverging palettes.
#' @return A character vector with (s)RGB codings of the colors in the palette.
#' @seealso \code{\link[colorspace]{sequential_hcl}}
#' @keywords color
#' @examples
#' ## show emulated CARTO palettes
#' carto_pal <- function(palette, border = "light gray")
#' {
#'   n <- 7
#'   col <- carto_hcl(n, palette)
#'   plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", axes = FALSE, xlab = "", ylab = "")
#'   rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
#'   axis(2, at = 0.5, tick = FALSE, labels = palette)
#' }
#' carto_seq <- c("RedOr", "OrYel", "Peach", "PinkYl", "Mint", "BluGrn", "DarkMint",
#'   "Emrld", "ag_GrnYl", "BluYl", "Teal", "TealGrn", "PurpOr", "Sunset", "Magenta")
#' carto_div <- c("ArmyRose", "Earth", "Fall", "Geyser", "Temps", "Tropic")
#' 
#' ## sequential palettes
#' par(mar = c(0, 5.5, 0, 0.5), mfrow = c(8, 2), las = 1)
#' for(n in carto_seq) carto_pal(n)
#'
#' ## diverging palettes
#' par(mar = c(0, 5.5, 0, 0.5), mfrow = c(6, 1), las = 1)
#' for(n in carto_div) carto_pal(n)
#'
#' ## compared to diverge_hcl() the diverging CARTO palettes are typically warmer
#' ## but also less balanced with respect to chroma/luminance, see e.g.,
#' specplot(carto_hcl(7, "Fall"), rgb = FALSE)
#' @rdname carto_hcl

#' @export
carto_hcl <- function(n, palette = "Geyser", ..., rev = FALSE,
  h1, h2, h3, c1, c2, c3, l1, l2, l3, p1, p2, p3, p4)
{
    ## empty palette
    if(n < 1L) return(character(0L))

    ## obtained stored coordinates    
    palette <- match.arg(palette, names(carto.pals))
    pals <- carto.pals[[palette]]
    names(pals) <- c("h1", "h2", "h3", "c1", "c2", "c3", "l1", "l2", "l3", "p1", "p2", "p3", "p4")

    ## stop if CARTO palette cannot be matched well enough
    if(is.na(pals["h1"])) stop(sprintf("palette '%s' cannot be emulated well enough by sequential_hcl()", palette))

    ## replace coordinates (if specified)
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(h3)) pals["h3"] <- h3
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(c2)) pals["c2"] <- c2
    if(!missing(c3)) pals["c3"] <- c3
    if(!missing(l1)) pals["l1"] <- l1
    if(!missing(l2)) pals["l2"] <- l2
    if(!missing(l3)) pals["l3"] <- l3
    if(!missing(p1)) pals["p1"] <- p1
    if(!missing(p2)) pals["p2"] <- p2
    if(!missing(p3)) pals["p3"] <- p3
    if(!missing(p4)) pals["p4"] <- p4

    ## resolve NAs
    ## first coordinate
    if(any(is.na(pals[c("h1", "c1", "l1")]))) stop("first hue/chroma/luminance coordinate must be specified")
    if(is.na(pals["p1"])) pals["p1"] <- 1
    ## second coordinate
    if(is.na(pals["c2"])) pals["c2"] <- 0
    if(is.na(pals["l2"])) pals["l2"] <- pals["l1"]
    if(is.na(pals["p2"])) pals["p2"] <- pals["p1"]
    ## third coordinate
    if(is.na(pals["h3"])) {
        diverge <- FALSE
    } else {
        diverge <- TRUE
	if(is.na(pals["c3"])) pals["c3"] <- pals["c1"]
	if(is.na(pals["l3"])) pals["l3"] <- pals["l1"]
	if(is.na(pals["p3"])) pals["p3"] <- pals["p1"]
	if(is.na(pals["p4"])) pals["p4"] <- pals["p3"]
    }

    ## call sequential_hcl() once or twice
    if(diverge) {
        n2 <- ceiling(n/2)    
        rval <- sequential_hcl(n2, rev = TRUE,
            h1 = pals["h1"], h2 = pals["h2"], c1 = pals["c1"], c2 = pals["c2"],
	    l1 = pals["l1"], l2 = pals["l2"], p1 = pals["p1"], p2 = pals["p2"], ...)
	if(floor(n/2) < n2) rval <- rval[-1L]
        rval <- c(rev(rval), sequential_hcl(n2, rev = TRUE,
            h1 = pals["h3"], h2 = pals["h2"], c1 = pals["c3"], c2 = pals["c2"],
	    l1 = pals["l3"], l2 = pals["l2"], p1 = pals["p3"], p2 = pals["p4"], ...))
	if(rev) rval <- rev(rval)
    } else {
        rval <- sequential_hcl(n, rev = rev,
            h1 = pals["h1"], h2 = pals["h2"], c1 = pals["c1"], c2 = pals["c2"],
	    l1 = pals["l1"], l2 = pals["l2"], p1 = pals["p1"], p2 = pals["p2"], ...)
    }

    return(rval)   
}

carto.pals <- list()
carto.pals[["Burg"]]       <- c( NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["BurgYl"]]     <- c( NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["RedOr"]]      <- c( -3,  53,  NA,  90,  42,  NA,  44,  86,  NA, 0.7, 1.0,  NA,  NA)
carto.pals[["OrYel"]]      <- c(  1,  72,  NA, 125,  49,  NA,  56,  87,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["Peach"]]      <- c( 15,  50,  NA, 130,  30,  NA,  55,  90,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["PinkYl"]]     <- c( -4,  80,  NA, 100,  47,  NA,  55,  96,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["Mint"]]       <- c(205, 140,  NA,  40,  12,  NA,  34,  94,  NA, 0.5, 1.0,  NA,  NA)
carto.pals[["BluGrn"]]     <- c(224, 127,  NA,  30,  30,  NA,  31,  88,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["DarkMint"]]   <- c(240, 130,  NA,  30,  30,  NA,  25,  95,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["Emrld"]]      <- c(224, 105,  NA,  23,  55,  NA,  25,  92,  NA, 1.5, 1.0,  NA,  NA)
carto.pals[["ag_GrnYl"]]   <- c(225,  87,  NA,  27,  86,  NA,  34,  92,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["BluYl"]]      <- c(250,  90,  NA,  40,  55,  NA,  33,  98,  NA, 0.5, 1.0,  NA,  NA)
carto.pals[["Teal"]]       <- c(240, 180,  NA,  35,  15,  NA,  35,  92,  NA, 0.5, 1.0,  NA,  NA)
carto.pals[["TealGrn"]]    <- c(224, 134,  NA,  44,  55,  NA,  49,  90,  NA, 1.2, 1.2,  NA,  NA)
carto.pals[["Purp"]]       <- c( NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["PurpOr"]]     <- c(-83,  20,  NA,  65,  18,  NA,  32,  90,  NA, 0.5, 1.0,  NA,  NA)
carto.pals[["Sunset"]]     <- c(-80,  78,  NA,  80,  45,  NA,  40,  91,  NA, 0.4, 1.0,  NA,  NA)
carto.pals[["Magenta"]]    <- c(312, 358,  NA,  70,  24,  NA,  27,  85,  NA, 0.5, 1.0,  NA,  NA)
carto.pals[["SunsetDark"]] <- c( NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["ag_Sunset"]]  <- c( NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["BrwnYl"]]     <- c( NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["ArmyRose"]]   <- c(  0,  NA,  93,  73,  18,  47,  60,  98,  52, 1.5, 0.8, 0.8, 1.0)
carto.pals[["Fall"]]       <- c(133,  77,  21,  20,  35, 100,  35,  95,  50, 1.0,  NA, 1.5,  NA)
carto.pals[["Geyser"]]     <- c(192,  77,  21,  40,  35, 100,  50,  95,  50, 1.0,  NA, 1.5,  NA)
carto.pals[["Temps"]]      <- c(191,  80,  -4,  43,  50,  78,  55,  89,  54, 1.0,  NA,  NA,  NA)
carto.pals[["TealRose"]]   <- c( NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["Tropic"]]     <- c(195,  NA, 325,  70,  NA,  NA,  55,  95,  NA, 1.0,  NA,  NA,  NA)
carto.pals[["Earth"]]      <- c( 43,  82, 221,  61,  30,  45,  50,  92,  52, 1.0, 1.0, 3.0, 1.0)
