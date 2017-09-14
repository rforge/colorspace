hclplot <- function(pal, collapse = NULL, ...) {
  
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

  if(is.null(collapse)) collapse <- if(abs(diff(range(HCL[, 3L], na.rm = TRUE))) < 5) "luminance" else "hue"
  collapse <- match.arg(collapse, c("hue", "luminance"))

  switch(collapse,
    "hue" = {
      m <- lm(H ~ C + L, data = as.data.frame(HCL))
      nd <- expand.grid(C = 0:180, L = 0:100)
      nd$H <- predict(m, nd)
      plot(0, 0, type = "n", xlim = c(0, 180), ylim = c(0, 100), xaxs = "i", yaxs = "i", xlab = "Chroma", ylab = "Luminance")
      points(nd$C, nd$L, col = hcl(nd$H, nd$C, nd$L, fixup = FALSE), pch = 19, cex = 3)
      points(HCL[, 2:3], pch = 21, bg = pal, cex = 2, type = "o")
    },
    "luminance" = {
      ## FIXME: back from polar coord
      m <- lm(L ~ C + H, data = as.data.frame(HCL))
      nd <- expand.grid(H = -360:360, C = 0:180)
      nd$L <- predict(m, nd)
      nd$L <- pmin(100, pmax(0, nd$L))
      plot(0, 0, type = "n", xlim = c(0, 180), ylim = c(-360, 360), xaxs = "i", yaxs = "i", xlab = "Chroma", ylab = "Hue")
      points(nd$C, nd$H, col = hcl(nd$H, nd$C, nd$L, fixup = FALSE), pch = 19, cex = 3)
      points(HCL[, 2:1], pch = 21, bg = pal, cex = 2, type = "o")
    }
  )
  ## FIXME: luminance = 0
}
