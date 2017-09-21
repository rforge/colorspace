#' Simulate Color Vision Deficiency
#'
#' Transformation of R colors by simulating color vision deficiencies,
#' based on a CVD transform matrix.
#'
#' Using the physiologically-based model for simulating color vision deficiency (CVD)
#' of Machado et al. (2009), deutanope, protanope, and tritanope vision can be emulated.
#' The workhorse function to do so is \code{simulate_cvd} which can take any vector
#' of valid R colors and transform them according to a certain CVD transformation
#' matrix (see \code{\link{cvd}}) and transformation equation.
#'
#' The functions \code{deutan}, \code{protan}, and \code{tritan} are the high-level functions for
#' simulating the corresponding kind of colorblindness with a given severity.
#' Internally, they all call \code{simulate_cvd} along with a (possibly interpolated)
#' version of the matrices from \code{\link{cvd}}. Matrix interpolation can be carried out with
#' the function \code{interpolate_cvd_transform} (see Examples).

#' @param col character. A color or vector of colors, e.g., \code{"#FFA801"} or \code{"blue"}.
#' @param severity numeric. Severity of the color vision defect, a number between 0 and 1.
#' @param cvd_transform numeric 3x3 matrix, specifying the color vision deficiency transform matrix.
#' @param cvd list of cvd transformation matrices. See \code{\link{cvd}} for available options.

#' @references Machado GM, Oliveira MM, Fernandes LAF (2009).
#'   A Physiologically-Based Model for Simulation of Color Vision Deficiency.
#'   \emph{IEEE Transactions on Visualization and Computer Graphics}. \bold{15}(6), 1291--1298.
#'   \doi{10.1109/TVCG.2009.113}
#'   Online version with supplements at
#'   \url{http://www.inf.ufrgs.br/~oliveira/pubs_files/CVD_Simulation/CVD_Simulation.html}.
#' @keywords colors cvd colorblind
#' @seealso \code{\link{cvd}}
#' @export
#' @examples
#' # simulate color-vision deficiency by calling `simulate_cvd` with specified matrix
#' simulate_cvd(c("#005000", "blue", "#00BB00"), tritanomaly_cvd["6"][[1]])
#' 
#' # simulate color-vision deficiency by calling the shortcut high-level function
#' tritan(c("#005000", "blue", "#00BB00"), severity = 0.6)
#' 
#' # simulate color-vision deficiency by calling `simulate_cvd` with interpolated cvd matrix
#' simulate_cvd(c("#005000", "blue", "#00BB00"),
#'              interpolate_cvd_transform(tritanomaly_cvd, severity = 0.6))
#' 
#' @importFrom grDevices col2rgb
simulate_cvd <- function(col, cvd_transform) {
  # Adapted from desaturate

  # If all hex
  if (is.character(col) && (all(substr(col, 1L, 1L) == "#") &
                            all(nchar(col) %in% c(7L, 9L)))) {
    #Save transparency value for later
    alpha <- substr(col, 8L, 9L)
    col <- substr(col, 1L, 7L)
    col <- grDevices::col2rgb(col)
  }
  # If contains built in color..,
  else {
    col <- grDevices::col2rgb(col, alpha = TRUE)
    ## extract alpha values (if non-FF)
    alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
    alpha[alpha == "FF"] <- ""
    ## retain only RGB
    col <- col[1L:3L,]
  }

  # Transform color
  RGB <- cvd_transform %*% col

  # Bound RGB values
  RGB[RGB<0] <- 0
  RGB[RGB>255] <- 255

  # Convert back to hex
  rgb2hex <- function(RGB) grDevices::rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)

  final_hex <- paste(rgb2hex(RGB), alpha, sep="")
  return(final_hex)
}

#' @rdname simulate_cvd
#' @export
deutan <- function(col, severity = 1) {
  simulate_cvd(col, cvd_transform = interpolate_cvd_transform(deutanomaly_cvd, severity))
}

#' @rdname simulate_cvd
#' @export
protan <- function(col, severity = 1) {
  simulate_cvd(col, cvd_transform = interpolate_cvd_transform(protanomaly_cvd, severity))
}

#' @rdname simulate_cvd
#' @export
tritan <- function(col, severity = 1){
  simulate_cvd(col, cvd_transform = interpolate_cvd_transform(tritanomaly_cvd, severity))
}

#' @rdname simulate_cvd
#' @export
interpolate_cvd_transform <- function(cvd, severity = 1) {
  if (severity <= 0) {
    cvd[[1]]
  } else if (severity >= 1) {
    cvd[[11]]
  } else {
    s <- 10*severity
    i1 <- floor(s)
    i2 <- ceiling(s)
    if (i1 == i2) {
      cvd[[i1+1]]
    }
    else {
      (i2-s)*cvd[[i1+1]] + (s-i1)*cvd[[i2+1]]
    }
  }
}
