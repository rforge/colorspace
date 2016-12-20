## desaturate colors (remove chroma in HCL space)
desaturate <- function(col) {
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
  
  ## convert to HCL and fix-up extreme luminance cases
  col <- as(col, "polarLUV")
  col@coords[col@coords[, 1L] <= 0 | col@coords[, 1L] >= 100, 2L:3L] <- 0
  
  ## convert back to hex and add alpha again (if any)
  col <- hex(col)
  col <- paste(col, alpha, sep = "")
  return(col)
}

## turn colors lighter or darker in either HCL or HLS space
lightdark <- function(col, amount = 0.1,
  method = "relative", space = c("HLS", "HCL"), fixup = TRUE)
{
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

    ## transform chroma correspondingly
    col@coords[, "C"] <- ifelse(col@coords[, "L"] > Lold,
      (col@coords[, "L"] - 100)/(Lold - 100) * col@coords[, "C"],
      col@coords[, "L"]/Lold * col@coords[, "C"]
    )
    col@coords[, "C"] <- pmin(100, pmax(0, col@coords[, "C"]))      
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

if(FALSE) {
## code
library("colorspace")
source("desaturate.R")
pal <- function(col, border = "light gray") {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

## fairly dark/colorful colors --> turn lighter
cl <- rainbow_hcl(5)

## HLS vs. HCL --> HCL is much more balanced
par(mfrow = c(3, 2), mar = rep(0, 4))
pal(cl)
pal(cl)
pal(lightdark(cl, 0.15))
pal(lightdark(cl, 0.15, space = "HCL"))
pal(lightdark(cl, 0.30))
pal(lightdark(cl, 0.30, space = "HCL"))

## fairly light colors --> turn darker
## http://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values
cl <- c("#CDE4F3","#E7F3D3","#F7F0C7","#EFCFE5","#D0D1E7")

## HLS vs. HCL --> HCL becomes darker but not more colorful...
par(mfrow = c(3, 2), mar = rep(0, 4))
pal(cl)
pal(cl)
pal(lightdark(cl, -0.15))
pal(lightdark(cl, -0.15, space = "HCL"))
pal(lightdark(cl, -0.30))
pal(lightdark(cl, -0.30, space = "HCL"))

}
