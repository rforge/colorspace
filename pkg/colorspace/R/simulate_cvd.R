#' Simulate color vision deficiency given a cvd transform matrix
#'
#' This function takes valid R colors and transforms them according to a cvd transform matrix.
#' @param col A color or vector of colors e.g., "#FFA801" or "blue"
#' @param cvd_transform A 3x3 matrix specifying the color vision deficiency transform matrix
#' @keywords colors, cvd, colorblind
#' @export
#' @examples
#'
#'  simulate_cvd(c("#005000","blue","#00BB00"),
#'  tritanomaly_cvd['6'][[1]])
#'
#' @importFrom grDevices col2rgb
#' @author Claire D. McWhite claire.d.mcwhite@gmail.com
simulate_cvd <- function(col, cvd_transform) {
  #Adapted from desaturate

  #If all hex
  if (is.character(col) && (all(substr(col, 1L, 1L) == "#") &
                            all(nchar(col) %in% c(7L, 9L)))) {
    #Save transparency value for later
    alpha <- substr(col, 8L, 9L)
    col <- substr(col, 1L, 7L)
    col <- grDevices::col2rgb(col)
  }
  #If contains built in color..,
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
  RGB[RGB<0]=0
  RGB[RGB>255]=255

  # Convert back to hex
  rgb2hex <- function(RGB) grDevices::rgb(RGB[1,], RGB[2,], RGB[3,], maxColorValue = 255)

  final_hex <- paste(rgb2hex(RGB), alpha, sep="")
  return(final_hex)
}

#' Generate color transformation matrices using the deutan model of colorblindness
#'
#' @param sev Severity of the color vision defect, a number between 0 and 1
#' @export
#' @author Claire D. McWhite claire.d.mcwhite@gmail.com
#' @author Claus O. Wilke wilke@austin.utexas.edu
deutan_transform <- function(sev = 1) {
  if (sev <= 0) {
    deutanomaly_cvd[[1]]
  } else if (sev >= 1) {
    deutanomaly_cvd[[11]]
  } else {
    s <- 10*sev
    i1 <- floor(s)
    i2 <- ceiling(s)
    if (i1 == i2) {
      deutanomaly_cvd[[i1]]
    }
    else {
      (i2-s)*deutanomaly_cvd[[i1+1]] + (s-i1)*deutanomaly_cvd[[i2+1]]
    }
  }
}

#' Convert colors using the deutan model of colorblindness
#'
#' @param colors Vector of colors to convert
#' @param sev Severity of the color vision defect, a number between 0 and 1
#' @export
#' @author Claire D. McWhite claire.d.mcwhite@gmail.com
#' @author Claus O. Wilke wilke@austin.utexas.edu
deutan <- function(colors, sev = 1) {
  simulate_cvd(colors, cvd_transform=deutan_transform(sev))
}

#' Generate color transformation matrices using the protan model of colorblindness
#'
#' @param sev Severity of the color vision defect, a number between 0 and 1
#' @export
#' @author Claire D. McWhite claire.d.mcwhite@gmail.com
#' @author Claus O. Wilke wilke@austin.utexas.edu
protan_transform <- function(sev = 1) {
  if (sev <= 0) {
    protanomaly_cvd[[1]]
  } else if (sev >= 1) {
    protanomaly_cvd[[11]]
  } else {
    s <- 10*sev
    i1 <- floor(s)
    i2 <- ceiling(s)
    if (i1 == i2) {
      protanomaly_cvd[[i1]]
    }
    else {
      (i2-s)*protanomaly_cvd[[i1+1]] + (s-i1)*protanomaly_cvd[[i2+1]]
    }
  }
}

#' Convert colors using the protan model of colorblindness
#'
#' @param colors Vector of colors to convert
#' @param sev Severity of the color vision defect, a number between 0 and 1
#' @export
#' @author Claire D. McWhite claire.d.mcwhite@gmail.com
#' @author Claus O. Wilke wilke@austin.utexas.edu
protan <- function(colors, sev = 1) {
  simulate_cvd(colors, cvd_transform = protan_transform(sev))
}

#' Generate color transformation matrices using the tritan model of colorblindness
#'
#' @param sev Severity of the color vision defect, a number between 0 and 1
#' @export
#' @author Claire D. McWhite claire.d.mcwhite@gmail.com
#' @author Claus O. Wilke wilke@austin.utexas.edu
tritan_transform <- function(sev = 1) {
  if (sev <= 0) {
    tritanomaly_cvd[[1]]
  } else if (sev >= 1) {
    tritanomaly_cvd[[11]]
  } else {
    s <- 10*sev
    i1 <- floor(s)
    i2 <- ceiling(s)
    if (i1 == i2) {
      tritanomaly_cvd[[i1]]
    }
    else {
      (i2-s)*tritanomaly_cvd[[i1+1]] + (s-i1)*tritanomaly_cvd[[i2+1]]
    }
  }
}

#' Convert colors using the tritan model of colorblindness
#'
#' @param colors Vector of colors to convert
#' @param sev Severity of the color vision defect, a number between 0 and 1
#' @export
#' @author Claire D. McWhite claire.d.mcwhite@gmail.com
#' @author Claus O. Wilke wilke@austin.utexas.edu
tritan <- function(colors, sev = 1){
  simulate_cvd(colors, cvd_transform=tritan_transform(sev))
}


