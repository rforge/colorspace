#' Palette Plot in HCL Space
#' 
#' Visualization of color palettes in HCL space projections.
#' 
#' The function \code{swatchplot} is a convenience function for displaying
#' collections of palettes that can be speciefied as lists or matrices of
#' character color specifications. Essentially, the is just a call to
#' \code{\link[graphics]{rect}}. The value-added are the heuristics used
#' for choosing default labels, margins, spacings, borders. These are selected
#' to work well for \code{\link{hcl_palettes}} and might need further tweaking
#' in future versions.
#' 
#' @param x character vector/matrix (or list of character vectors/matrices)
#' containing color hex codes.
#' @param \dots further (possibly named) character vectors/matrices with color
#' hex codes.
#' @param nrow integer specifying the maximal number of rows of swatches.
#' (The actual number might be lower in order to balance the rows used in each column.)
#' @param border color for border of individual color rectangles. By default
#' \code{"lightgray"} for up to 9 colors, \code{"transparent"} otherwise.
#' @param sborder color for border of the entire palette swatch. By default
#' \code{"lightgray"} if \code{border} is \code{"transparent"} and \code{"lightgray"}
#' otherwise (if \code{off = 0}).
#' @param off numeric vector of length 2. Offset in horizontal and vertical direction
#' (specified as a fraction of the rectangle for one color). By default, the
#' horizontal offset is \code{0.3} for up to 5 colors and \code{0} otherwise,
#' and the vertical offset is \code{0.1}.
#' @param mar numeric vector of length 4, specifying the margins of column
#' of color swatches.
#' @param line numeric. Line in which the palette names (if any) are printed
#' in the margin.
#' @param cex,font numeric vectors of length 1 or 2. Specifications for the
#' annotation text for the individual palettes and lists of palettes, respectively.
#' @return \code{swatchplot} invisibly returns a matrix with colors and annotations.
#' @keywords misc
#' @examples
#' ## for qualitative palettes luminance and chroma are fixed, varying only hue
#' hclplot(qualitative_hcl(9, c = 50, l = 70))
#' 
#' ## simple single-hue sequential palette (h = 260) with linear and power-transformed trajectory
#' hclplot(sequential_hcl(7, h = 260, c = 80, l = c(35, 95), power = 1))
#' hclplot(sequential_hcl(7, h = 260, c = 80, l = c(35, 95), power = 1.5))
#' 
#' ## single-hue sequential palette with triangular chroma trajectory (piecewise linear and power-transformed)
#' hclplot(sequential_hcl(7, h = 245, c = c(40, 75, 0), l = c(30, 95), power = 1))
#' hclplot(sequential_hcl(7, h = 245, c = c(40, 75, 0), l = c(30, 95), power = c(0.8, 1.4)))
#' 
#' ## multi-hue sequential palette with small hue range and triangular chroma vs.
#' ## large hue range and linear chroma trajectory
#' hclplot(sequential_hcl(7, h = c(260, 220), c = c(50, 75, 0), l = c(30, 95), power = 1))
#' hclplot(sequential_hcl(7, h = c(260, 60), c = 60, l = c(40, 95), power = 1))
#' 
#' ## balanced diverging palette constructed from two simple single-hue sequential
#' ## palettes (for hues 260/blue and 0/red)
#' hclplot(diverge_hcl(7, h = c(260, 0), c = 80, l = c(35, 95), power = 1))
#' @export hclplot
#' @importFrom graphics box lines mtext par plot points rect text

hclplot <- function(x, type = NULL, h = NULL, c = NULL, l = NULL,
    xlab = NULL, ylab = NULL, main = NULL, cex = 1.0, axes = TRUE, ...)
{  
    ## convert to HCL coordinates
    if(is.character(x)) {
      HCL <- hex2RGB(x)
    } else {
      HCL <- x
      x <- hex(x)
    }
    HCL <- coords(as(HCL, "polarLUV"))[, c("H", "C", "L")]
    n <- nrow(HCL)

    ## determine type of palette based on luminance trajectory
    lran <- diff(range(HCL[, "L"], na.rm = TRUE))
    llin <- cor(HCL[, "L"], 1L:n, use = "pairwise.complete.obs")^2
    ltri <- cor(HCL[, "L"], abs(1L:n - (n + 1)/2), use = "pairwise.complete.obs")^2
    if(is.null(type)) {
      type <- if(ltri > 0.75 & lran > 10) {
        "diverging"
      } else if(llin > 0.75 & lran > 10) {
        "sequential"
      } else {
        "qualitative"
      }
    }

    ## FIXME: put into separate function
    if(n > 1L) {
        for(i in 2L:n) {
            if ( any(is.na(HCL[(i-1L):i,])) ) next
            d <- HCL[i, "H"] - HCL[i - 1L, "H"]
            if (abs(d) > 320) HCL[i, "H"] <- HCL[i, "H"] - sign(d) * 360
            if (abs(HCL[i, "H"]) >  360) HCL[1L:i, "H"] <- HCL[1L:i, "H"] - sign(HCL[i, "H"]) * 360
        }

        # (2) Smoothing hue values in batches where chroma is very low
        idx <- which(HCL[, "C"] < 8)
        if (length(idx) == n) {
            HCL[,"H"] <- mean(HCL[,"H"])
        } else if (length(idx) > 0L) {
            ## pre-smooth hue
            if(n >= 49L) {
                HCL[, "H"] <- 1/3 * (
                HCL[c(rep.int(1L, 2L), 1L:(n - 2L)), "H"] +
                HCL[c(rep.int(1L, 1L), 1L:(n - 1L)), "H"] +
                HCL[                   1L:n,         "H"])
            }
            idxs <- split(idx, cumsum(c(1, diff(idx)) > 1))
            s <- 1L
            while(length(idxs) > 0L) {
                e <- if(s %in% idxs[[1L]]) {
                    if(length(idxs) > 1L) idxs[[2L]] - 1L else n
                } else {
                    if(n %in% idxs[[1L]]) n else round(mean(range(idxs[[1L]])))
                }
                io <- split(s:e, s:e %in% idx)
                if (length(io) == 2L & sum(!is.na(HCL[io[["FALSE"]],"H"])) > 0) {
                    HCL[io[["TRUE"]], "H"] <- stats::spline(io[["FALSE"]], HCL[io[["FALSE"]], "H"],
                    xout = io[["TRUE"]], method = "natural")$y
                }
                idxs[[1L]] <- NULL
                s <- e + 1L
            }
        }
    }

    maxchroma <- if(!is.null(c)) ceiling(c) else pmax(100, pmin(180, ceiling(max(HCL[, "C"], na.rm = TRUE)/20) * 20))

    # Parameters
    opar <- par(cex = cex, no.readonly = TRUE)
    on.exit(par(opar))

    switch(type,
        "sequential" = {
            par(mar = c(3, 3, 2, 1) * cex)
            nd <- expand.grid(C = 0:maxchroma, L = 0:100)
	    if(!is.null(h)) {
	        nd$H <- h
	    } else if(diff(range(HCL[, "H"], na.rm = TRUE)) < 5) {
                nd$H <- mean(HCL[, "H"], na.rm = TRUE)
            } else {
                m <- lm(H ~ C + L, data = as.data.frame(HCL))
                if(summary(m)$sigma > 7.5) warning("cannot approximate H well as a linear function of C and L")
                nd$H <- predict(m, nd)
            }
            if(is.null(main)) {
	        main <- if(length(unique(nd$H)) <= 1L) {
		    round(nd$H[1L])
		} else {
		    paste("[", round(min(nd$H, na.rm = TRUE)), ", ", round(max(nd$H, na.rm = TRUE)), "]", sep = "")
		}
	        main <- paste("Hue =", main)
	    }
            HCL2 <- hex(polarLUV(H = nd$H, C = nd$C, L = nd$L), fixup = FALSE)
            HCL2[nd$L < 1 & nd$C > 0] <- NA
            plot(0, 0, type = "n", xlim = c(0, maxchroma), ylim = c(0, 100), xaxs = "i", yaxs = "i",
                 xlab = NA, ylab = NA, main = main)
            # Adding axis labels
            if ( is.null(xlab) ) xlab <- "Chroma"
            if ( is.null(ylab) ) ylab <- "Luminance"
            mtext(side = 1, line = 2 * cex, xlab, cex = cex)
            mtext(side = 2, line = 2 * cex, ylab, cex = cex)
            # Adding colors
            points(nd$C, nd$L, col = HCL2, pch = 19, cex = 3)
            points(HCL[, 2L:3L], pch = 19, cex = 2.2,  type = "p", lwd = 5, col = "white")
            points(HCL[, 2L:3L], pch = 21, bg = x, cex = 2, type = "o", lwd = 2)
            box()
        },
        "diverging" = {
            par(mar = c(3, 3, 2, 1) * cex)
            nd <- expand.grid(C = -maxchroma:maxchroma, L = 0:100)
	    nd$H <- NA
            nd$left <- nd$C < 0
            left  <- 1L:floor(n/2)
	    left  <- left[HCL[left, "C"] > 10]
            right <- ceiling(n/2):n
	    right <- right[HCL[right, "C"] > 10]
	    
	    if(!is.null(h)) {
	        if(length(h) == 2L) {
                    nd$H[nd$left]  <- h[1L]
                    nd$H[!nd$left] <- h[2L]
		} else {
		  nd$H <- h
		}
            } else if(diff(range(HCL[left, "H"]  - min(HCL[ left, "H"], na.rm = TRUE), na.rm = TRUE)) < 5 &
	       diff(range(HCL[right, "H"] - min(HCL[right, "H"], na.rm = TRUE), na.rm = TRUE)) < 5)
	    {
               nd$H[nd$left]  <- mean(HCL[ left, "H"] - min(HCL[ left, "H"], na.rm = TRUE), na.rm = TRUE) + min(HCL[ left, "H"], na.rm = TRUE)
               nd$H[!nd$left] <- mean(HCL[right, "H"] - min(HCL[right, "H"], na.rm = TRUE), na.rm = TRUE) + min(HCL[right, "H"], na.rm = TRUE)
            } else {
	       HCLdata <- as.data.frame(HCL)
	       HCLdata$left <- factor(rep(c(TRUE, FALSE), c(floor(n/2), ceiling(n/2))))
	       nd$left <- factor(nd$left)
               m <- lm(H ~ left * (C + L), data = HCLdata)
               if(summary(m)$sigma > 7.5) warning("cannot approximate H well as a linear function of C and L")
               nd$H <- predict(m, nd)
	       nd$left <- nd$left == "TRUE"
            }
            if(is.null(main)) {
	        main <- if(length(unique(nd$H)) <= 2L) {
		    paste(round(nd$H[nd$left][1L]), "/", round(nd$H[!nd$left][1L]))
		} else {
		    paste("[",
		        round(min(nd$H[nd$left], na.rm = TRUE)), ", ", round(max(nd$H[nd$left], na.rm = TRUE)), "] / [",
	                round(min(nd$H[!nd$left], na.rm = TRUE)), ", ", round(max(nd$H[!nd$left], na.rm = TRUE)), "]", sep = "")
		}
	        main <- paste("Hue =", main)
	    }
            HCL2 <- hex(polarLUV(H = nd$H, C = abs(nd$C), L = nd$L), fixup = FALSE)
            HCL2[nd$L < 1 & nd$C > 0] <- NA
            plot(0, 0, type = "n", xlim = c(-1,1)*maxchroma, ylim = c(0, 100), xaxs = "i", yaxs = "i",
                 xlab = NA, ylab = NA, main = main)
            # Axis labels
            if ( is.null(xlab) ) xlab <- "Chroma"
            if ( is.null(ylab) ) ylab <- "Luminance"
            mtext(side = 1, line = 2 * cex, xlab, cex = cex)
            mtext(side = 2, line = 2 * cex, ylab, cex = cex)
            # Plotting colors
            points(nd$C, nd$L, col = HCL2, pch = 19, cex = 3)
            points( HCL[, "C"] * ifelse(1L:n <= floor(mean(n/2)),-1,1),
                    HCL[, "L"], pch = 19, cex = 2.2,  type = "p", lwd = 5, col = "white")
            points( HCL[, "C"] * ifelse(1L:n <= floor(mean(n/2)),-1,1),
                    HCL[, "L"], pch = 21, bg = x, cex = 2, type = "o")
            box()
        },
        "qualitative" = {
            par(mar = c(1, 1, 2, 1) * cex, bty = "n")
            nd <- expand.grid(H = 0:180 * 2, C = 0:maxchroma)


	    if(!is.null(l)) {
                nd$L <- l
            } else if(diff(range(HCL[, "L"], na.rm = TRUE)) < 5) {
                nd$L <- mean(HCL[, "L"], na.rm = TRUE)
            } else {
                m <- lm(L ~ C + H, data = as.data.frame(HCL))
                if(summary(m)$sigma > 7.5) warning("cannot approximate L well as a linear function of H and C")
                nd$L <- predict(m, nd)
                nd$L <- pmin(100, pmax(0, nd$L))
            }
            if(is.null(main)) {
	        main <- if(length(unique(nd$L)) <= 1L) {
		    round(nd$L[1L])
		} else {
		    paste("[", round(min(nd$L, na.rm = TRUE)), ", ", round(max(nd$L, na.rm = TRUE)), "]", sep = "")
		}
	        main <- paste("Luminance =", main)
	    }
            HCL2 <- hex(polarLUV(H = nd$H, C = nd$C, L = nd$L), fixup = FALSE)
            HCL2[nd$L < 1 & nd$C > 0] <- NA

            # fact: used for scaling
            fact <- 1.1 + (cex - 1) / 10
            plot(0, 0, type = "n", axes = FALSE, xlab = NA, ylab = NA, main = main,
                 xlim = c(-maxchroma, maxchroma) * fact, ylim = c(-maxchroma, maxchroma) * fact, asp = 1)
            xpos <- function(h, c) cos(h * pi/180) * c
            ypos <- function(h, c) sin(h * pi/180) * c
            points(xpos(nd$H, nd$C), ypos(nd$H, nd$C), col = HCL2, pch = 19, cex = 3)
            lines(xpos(0:360, maxchroma), ypos(0:360, maxchroma))
            
            if(axes) {
                if(is.null(xlab)) xlab <- "Chroma"
                if(is.null(ylab)) ylab <- "Hue"
                at.c <- if(maxchroma >= 150) 0:3 * 50 else 0:3 * 25
                at.h <- 0:6 * 60
                lines(c(0, maxchroma), c(0, 0))
                text(at.c, rep(-7, length(at.c)), at.c)
                text(50, -14, xlab)
                rect(at.c, 0, at.c, -3)
                if(0 %in% at.h | 360 %in% at.h) {
                  lines(xpos(0, maxchroma + c(0, 3)), ypos(0, maxchroma + c(0, 3)))
                  text(xpos(0, maxchroma + 7), ypos(0, maxchroma + 7), 0, pos = 3)
                  text(xpos(0, maxchroma + 7), ypos(0, maxchroma + 7), 360, pos = 1)
                  text(xpos(0, maxchroma + 16), ypos(0, maxchroma + 16), ylab)
                }
                at.h <- at.h[at.h > 0 & at.h < 360]
                for(hue in at.h) {
                  text(xpos(hue, maxchroma + 7), ypos(hue, maxchroma + 7), hue)
                  lines(xpos(hue, maxchroma + c(0, 3)), ypos(hue, maxchroma + c(0, 3)))
                }
            }
            points(xpos(HCL[, "H"], HCL[, "C"]), ypos(HCL[, "H"], HCL[, "C"]),
                   pch = 19, cex = 2.2,  type = "p", lwd = 5, col = "white")
            points(xpos(HCL[, "H"], HCL[, "C"]), ypos(HCL[, "H"], HCL[, "C"]),
                   pch = 21, bg = x, cex = 2, type = "o")
            box()
        }
    )

    invisible(HCL)
}
