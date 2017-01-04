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

if ( FALSE ) {

   # ----------------------------------------------------------------
   # Development function to get multi-hue diverging color maps
   # ----------------------------------------------------------------
   multiverge_hcl <- function(n,h=c(10,100,230,290),l=60,lmax=90,fixup=FALSE) {

      C <- seq(0,min(max_chroma(h,l)),length=n)
      L <- seq(lmax,l,length=n)
      cols <- matrix("NA",ncol=length(h),nrow=n)
      for ( i in 1:ncol(cols) ) {
         cols[,i] <- hex(polarLUV(H=h[i],C=C,L=L),fixup)
      }
      attr(cols,"H") <- h
      attr(cols,"C") <- C
      attr(cols,"L") <- L

      cols
   }

   # ----------------------------------------------------------------
   # Development function to show the color map properties
   # ----------------------------------------------------------------
   showcols <- function(cols) {
      opar <- par(no.readonly=TRUE); on.exit(par(opar))
      par(ask=TRUE)
      par(mfrow=c(ncol(cols),1),mar=c(0,3,0,0),oma=rep(0,4),xaxt='n',yaxt='n')
      for ( i in 1:ncol(cols) ) {
         image(matrix(1:nrow(cols)),col=cols[,i])
         mtext(side=2,attr(cols,"H")[i])
      }

      # Show space
      h <- attr(cols,"H")
      res <- matrix(NA,ncol=length(h),nrow=101)
      for ( i in seq_along(h) ) {
         names   <- sprintf("%d-%d",round(h[i]),0:100)
         res[,i] <- as.numeric(tab[names(tab) %in% names])
      }
      par(mar=c(4,4,2,1),xaxt='s',yaxt='s',mfrow=c(1,1),xaxs='i',yaxs='i')
      plot(0,xlim=c(0,max(res)),ylim=c(0,100),type="n",main="Space Check",
            xlab="Chroma", ylab="Luminance")
      for ( i in 1:length(h) ) {
         lines(res[,i],0:100,col=cols[nrow(cols),i])
      }
      points(attr(cols,'C'),attr(cols,'L'))
   }

   # ----------------------------------------------------------------
   # A working set
   # ----------------------------------------------------------------
   h <- c(10,100,230,290)
   l <- 50
   lmax <- 90
   cols <- multiverge_hcl(10,h=h,l=l,lmax=lmax)
   showcols(cols)

   # ----------------------------------------------------------------
   # Fixup=FALSE, wherefore we have missing colors here (non-working
   # example, even if a fixup would lead to quite good results) 
   # ----------------------------------------------------------------
   h <- c(10,290)
   l <- 70
   lmax <- 99
   cols <- multiverge_hcl(10,h=h,l=l,lmax=lmax)
   showcols(cols)

}
