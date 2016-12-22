adjust_hcl <- function(col, h = 0, c = 0, l = 0, method = "relative", fixup = TRUE, maxchroma = TRUE)
{
  ## method
  method <- match.arg(method, c("relative", "absolute"))

  ## number of colors
  n <- max(c(length(col), length(h), length(c), length(l)))
  col <- rep_len(col, length.out = n)
  h <- rep_len(h, length.out = n)
  c <- rep_len(c, length.out = n)
  l <- rep_len(l, length.out = n)

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
  
  ## convert to HCL and remove chroma
  col <- as(col, "polarLUV")
  ## fix-up extreme luminance cases
  col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))

  ## transform coordinates
  if(method == "relative") {
    col@coords[, "H"] <- col@coords[, "H"] *  (1 + h)
    col@coords[, "C"] <- col@coords[, "C"] *  (1 + c)
    col@coords[, "L"] <- col@coords[, "L"] *  (1 + l)
  } else {
    col@coords[, "H"] <- col@coords[, "H"] + h * 360
    col@coords[, "C"] <- col@coords[, "C"] + c * 100
    col@coords[, "L"] <- col@coords[, "L"] + l * 100
  }
  col@coords[, "C"] <- pmin(100, pmax(0, col@coords[, "C"]))
  col@coords[, "L"] <- pmin(100, pmax(0, col@coords[, "L"]))
  
  if(maxchroma) {
    col@coords[, "C"] <- pmin(col@coords[, "C"], max_chroma(col@coords[, "H"], col@coords[, "L"]))
  }
  
  ## convert back to hex and add alpha again (if any)
  col <- hex(col, fixup = fixup)
  col[!is.na(col)] <- paste(col[!is.na(col)], alpha[!is.na(col)], sep = "")
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
    ## transform chroma correspondingly (relative to maximum chroma possible)
    col@coords[, "C"] <- col@coords[, "C"]/ceiling(max_chroma(col@coords[, "H"], Lold) + 1e-8) *
      max_chroma(col@coords[, "H"], col@coords[, "L"], floor = TRUE)
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
pal <- function(col, border = "light gray") {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

## fairly dark/colorful colors --> turn lighter
cl <- rainbow_hcl(5)

## HLS vs. HCL --> HCL is much more balanced
par(mfrow = c(3, 2), mar = rep(0, 4), oma = c(0, 0, 2, 0))
pal(cl); mtext("HLS")
pal(cl); mtext("HCL")
pal(lightdark(cl, 0.15))
pal(lightdark(cl, 0.15, space = "HCL"))
pal(lightdark(cl, 0.30))
pal(lightdark(cl, 0.30, space = "HCL"))


## fairly light colors --> turn darker
cl <- rainbow_hcl(5, l = 90, c = 35)

## HLS vs. HCL --> HCL is much more balanced
par(mfrow = c(3, 2), mar = rep(0, 4), oma = c(0, 0, 2, 0))
pal(cl); mtext("HLS")
pal(cl); mtext("HCL")
pal(lightdark(cl, -0.15))
pal(lightdark(cl, -0.15, space = "HCL"))
pal(lightdark(cl, -0.30))
pal(lightdark(cl, -0.30, space = "HCL"))


## fairly light colors --> turn darker
## http://stackoverflow.com/questions/30219738/is-there-a-way-to-programmatically-darken-the-color-given-rgb-values
cl <- c("#CDE4F3","#E7F3D3","#F7F0C7","#EFCFE5","#D0D1E7")

## HLS vs. HCL --> HCL becomes darker but not more colorful...
par(mfrow = c(3, 2), mar = rep(0, 4), oma = c(0, 0, 2, 0))
pal(cl); mtext("HLS")
pal(cl); mtext("HCL")
pal(lightdark(cl, -0.15))
pal(lightdark(cl, -0.15, space = "HCL"))
pal(lightdark(cl, -0.30))
pal(lightdark(cl, -0.30, space = "HCL"))

}
