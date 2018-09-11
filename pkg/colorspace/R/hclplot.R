


hclplot <- function(pal, collapse = NULL, maxchroma = NULL, xlab = NULL, ylab = NULL, main = NULL, cex = 1.0, ... ) {
  
    HCL <- coords(as(hex2RGB(pal), "polarLUV"))[, c("H", "C", "L")]

    ## FIXME: put into separate function
    if(nrow(HCL) > 1L) {
        for(i in 2L:nrow(HCL)) {
            if ( any(is.na(HCL[(i-1L):i,])) ) next
            d <- HCL[i, "H"] - HCL[i - 1L, "H"]
            if (abs(d) > 320) HCL[i, "H"] <- HCL[i, "H"] - sign(d) * 360
            if (abs(HCL[i, "H"]) >  360) HCL[1L:i, "H"] <- HCL[1L:i, "H"] - sign(HCL[i, "H"]) * 360
        }

        # (2) Smoothing hue values in batches where chroma is very low
        idx <- which(HCL[, "C"] < 8)
        if (length(idx) == nrow(HCL)) {
            HCL[,"H"] <- mean(HCL[,"H"])
        } else if (length(idx) > 0L) {
            ## pre-smooth hue
            n <- nrow(HCL)
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


    # Diverge color palette check
    X <- HCL[HCL[,"C"] > 15,]
    if ( nrow(X) == 0 ) {
        X <- 0
    } else {
        X <- c( sd(X[1:floor(nrow(X)/2),c("H")]) +
                sd(X[nrow(X):(1+ceiling(nrow(X)/2)),c("H")]),
                apply( X[1:floor(nrow(X)/2),c("C","L")] -
                       X[nrow(X):(1+ceiling(nrow(X)/2)),c("C","L")],2,mean) )
    }

    # Qualitative scheme -> luminance plane
    # Single sequential schemes -> hue plane
    # Diverging schemes -> diverging plane
    if(is.null(collapse)) {
        collapse <- if ( sd(HCL[,1L],na.rm=TRUE) < 5 ) { "luminance" }
                    else if ( mean(X) < 10 ) { "diverging" } else { "hue" }
    }
    collapse <- match.arg(collapse, c("hue", "luminance", "diverging" ))
    if(is.null(maxchroma)) maxchroma <- pmax(100, pmin(180, ceiling(max(HCL[, "C"], na.rm = TRUE)/20) * 20))

    # Parameters
    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    ## FIXME: strange palettes need other interpolation/smoothing or better warning
    #cat(sprintf(" * Collapsing on \"%s\" plane\n",collapse))
    switch(collapse,
        "hue" = {
            par(cex = cex, mar = c(3,3,2,1) * cex)
            nd <- expand.grid(C = 0:maxchroma, L = 0:100)
            if(diff(range(HCL[, "H"], na.rm = TRUE)) < 5) {
               nd$H <- mean(HCL[, "H"], na.rm = TRUE)
               if(is.null(main)) main <- paste("Hue =", round(nd$H[1L]))
            } else {
              m <- lm(H ~ C + L, data = as.data.frame(HCL))
                if(summary(m)$sigma > 7.5) warning("cannot approximate H well as a linear function of C and L")
               nd$H <- predict(m, nd)
                if(is.null(main)) main <- paste("Hue = [", round(min(nd$H, na.rm = TRUE)), ", ", round(max(nd$H, na.rm = TRUE)), "]", sep = "")
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
            points(HCL[, 2:3], pch = 19, cex = 2.2,  type = "p", lwd = 5, col = "white")
            points(HCL[, 2:3], pch = 21, bg = pal, cex = 2, type = "o", lwd = 2)
            box()
        }, # end switch "hue"
        "diverging" = {
            par(cex = cex, mar = c(3,3,2,1) * cex)
            nd <- expand.grid(C = -maxchroma:maxchroma, L = 0:100)

            left  <- which( HCL[1:floor(nrow(HCL)/2),"C"]               > 15 )
            right <- nrow(HCL) - left + 1

            H1 <- mean(HCL[left,"H"])
            H2 <- mean(HCL[right,"H"])
            #if ( diff(range(HCL[left,"H"], na.rm = TRUE)) < 5 ) {
            #   H1 <- mean(HCL[left,"H"], na.rm = TRUE)
            #} else { print(HCL); stop('H1 cannot be estimated') }
            #if ( diff(range(HCL[right,"H"], na.rm = TRUE)) < 5 ) {
            #   H2 <- mean(HCL[right,"H"], na.rm = TRUE)
            #} else { print(HCL); stop('H2 cannot be estimated') }

            nd$H <- ifelse( nd$C < 0, H1, H2 )
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
            points( HCL[,"C"] * ifelse(1:nrow(HCL) <= floor(mean(nrow(HCL)/2)),-1,1),
                    HCL[, "L"], pch = 19, cex = 2.2,  type = "p", lwd = 5, col = "white")
            points( HCL[,"C"] * ifelse(1:nrow(HCL) <= floor(mean(nrow(HCL)/2)),-1,1),
                    HCL[,"L"], pch = 21, bg = pal, cex = 2, type = "o")
            box()
        }, # end switch "luminance"
        "luminance" = {
            par(cex = cex, mar = c(1,1,2,1) * cex, bty = "n")
            nd <- expand.grid(H = 0:180 * 2, C = 0:maxchroma)
            if(diff(range(HCL[, "L"], na.rm = TRUE)) < 5) {
                nd$L <- mean(HCL[, "L"], na.rm = TRUE)
                if(is.null(main)) main <- paste("Luminance =", round(nd$L[1L]))
            } else {
                m <- lm(L ~ C + H, data = as.data.frame(HCL))
                if(summary(m)$sigma > 7.5) warning("cannot approximate L well as a linear function of H and C")
                    nd$L <- predict(m, nd)
                    nd$L <- pmin(100, pmax(0, nd$L))
                if(is.null(main)) main <- paste("Luminance = [", round(min(nd$L, na.rm = TRUE)),
                                                ", ", round(max(nd$L, na.rm = TRUE)), "]", sep = "")
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
            
            axes <- TRUE ## FIXME: export as argument?
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
                   pch = 21, bg = pal, cex = 2, type = "o")
            box()
        } # end switch "diverging"
    )

    invisible(HCL)
}
