#' HCL Color Palettes
#' 
#' Qualitative, sequential (single-hue and multi-hue), and diverging
#' color palettes based on the HCL (hue-chroma-luminance) color model.
#' 
#' Details are still tbd...
#' 
#' @param n the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.
#' @param h,h1,h2 hue value in the HCL color description, has to be in [0, 360].
#' @param c,c.,c1,c2 chroma value in the HCL color description.
#' @param l,l1,l2 luminance value in the HCL color description.
#' @param power,p1,p2 control parameter determining how chroma and luminance should
#' be increased (1 = linear, 2 = quadratic, etc.).
#' @param cmax Maximum chroma value in the HCL color description.
#' @param gamma Deprecated.
#' @param fixup logical. Should the color be corrected to a valid RGB value?
#' @param off numeric. Vector of length 2 indicating horizontal and vertical
#' offsets between the color rectangles displayed.
#' @param border character. Color of rectangle borders.
#' @param alpha numeric vector of values in the range \code{[0, 1]} for alpha
#' transparency channel (0 means transparent and 1 means opaque).
#' @param name,palette character. Name of HCL color palette.
#' @param rev logical. Should the color palette vector be returned in reverse order?
#' @param \dots Other arguments passed to \code{\link{hex}}.
#' @param type character indicating type of HCL palette.
#' @param plot logical. Should the selected HCL color palettes be visualized?
#' @param x,object A \code{hcl_palettes} object.
#'
#' @keywords color
#' @examples
#' ## overview of all _named_ HCL palettes
#' hcl_palettes()
#'
#' ## visualize
#' hcl_palettes("qualitative", plot = TRUE)
#' hcl_palettes("sequential (single-hue)", n = 7, plot = TRUE)
#' hcl_palettes("sequential (multi-hue)", n = 7, plot = TRUE)
#' hcl_palettes("diverging", n = 7, plot = TRUE)
#'
#' ## inspect a specific palette
#' ## (upper-case, spaces, etc. are ignored for matching)
#' hcl_palettes(name = "Dark 2")
#' hcl_palettes(name = "dark2")
#' 
#' ## set up actual colors
#' qualitative_hcl(4, h = c(0, 288), c = 50, l = 60) ## by hand
#' qualitative_hcl(4, palette = "dark2")             ## by name
#' qualitative_hcl(4, palette = "dark2", c = 80)     ## by name plus modification
#' @export
hcl_palettes <- function(type = NULL, name = NULL, plot = FALSE, n = 5L, ...)
{
  ## subset by type and name (by flexible matching)
  fx <- function(n) tolower(gsub("[-, _, \\,, (, ), \\ , \\.]", "", n))
  if(!is.null(type)) {
    tytab <- c("sequential", fx(levels(hcl_pals$type)))
    type <- lapply(type, function(ty) {
      ty <- startsWith(tytab, fx(ty))
      if(all(!ty)) stop("Palette 'type' should be one of: ", paste(levels(hcl_pals$type), collapse = ", "))
      ty <- tytab[which(ty)[1L]]
      if(ty == "sequential") ty <- c("sequentialsinglehue", "sequentialmultihue")
      return(ty)
    })
    type <- unlist(type)
    type <- levels(hcl_pals$type)[tytab[-1L] %in% type]
    pals <- hcl_pals[as.character(hcl_pals$type) %in% type, , drop = FALSE]
  } else {
    pals <- hcl_pals
  }
  if(!is.null(name)) {
    namtab <- fx(rownames(pals))
    name <- sapply(fx(name), function(n) {
      if(n %in% namtab) return(n)
      n <- startsWith(namtab, n)
      if(all(!n)) stop("Palette 'name' should be one of: ", paste(rownames(pals), collapse = ", "))
      namtab[which(n)[1L]]
    })
    pals <- pals[fx(rownames(pals)) %in% name, , drop = FALSE]
  }

  ## add class and show selection
  class(pals) <- c("hcl_palettes", "data.frame")
  if(plot) {
    plot(pals, n = n, ...)
    invisible(pals)
  } else {
    return(pals)
  }
}


#' @rdname hcl_palettes
#' @export
print.hcl_palettes <- function(x, ...) {
  if(nrow(x) > 1L) {
    type <- unique(as.character(x$type))
    cat("HCL palettes\n")
    for(ty in type) {
      cat("\nType: ", ty, "\n")
      cat("Names: ")
      writeLines(strwrap(paste(rownames(x)[as.character(x$type) %in% ty], collapse = ", "), exdent = 7), ...)
    }
  } else {
    cat("HCL palette")
    cat("\nName:", rownames(x))
    cat("\nType:", as.character(x$type))
    cat("\nParameter ranges:\n")
    print.data.frame(x[ , 2L:11L, drop = FALSE], row.names = FALSE, ...)
  }
  invisible(x)
}

#' @rdname hcl_palettes
#' @export
summary.hcl_palettes <- function(object, ...) {
  type <- unique(as.character(object$type))
  cat("HCL palettes\n")
  for(ty in type) {
    cat("\nType:", ty, "\n")
    cat("Parameter ranges:\n")
    print.data.frame(object[as.character(object$type) %in% ty, 2L:11L, drop = FALSE], ...)
  }
  invisible(object)
}

#' @rdname hcl_palettes
#' @method plot hcl_palettes
#' @export
plot.hcl_palettes <- function(x, n = 5L, fixup = TRUE, off = NULL, border = NULL, ...)
{
  typ <- levels(x$type)
  x$type <- as.character(x$type)
  xx <- as.matrix(x[, 2L:11L])

  qcol <- sapply(which(x$type == typ[1L]), function(i) {
    qualitative_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], c1 = xx[i, "c1"], l1 = xx[i, "l1"], fixup = fixup)
  })
  qcol <- if(length(qcol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(qcol))), ncol = n,
    dimnames = list(c(typ[1L], rownames(x)[x$type == typ[1L]]), paste("Color", 1L:n)))

  scol <- sapply(which(x$type == typ[2L]), function(i) {
    sequential_hcl(n = n, h1 = xx[i, "h1"], c1 = xx[i, "c1"], c2 = xx[i, "c2"], l1 = xx[i, "l1"], l2 = xx[i, "l2"],
      p1 = xx[i, "p1"], p2 = xx[i, "p2"], cmax = xx[i, "cmax"], fixup = fixup)
  })
  scol <- if(length(scol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(scol))), ncol = n,
    dimnames = list(c(typ[2L], rownames(x)[x$type == typ[2L]]), paste("Color", 1L:n)))

  mcol <- sapply(which(x$type == typ[3L]), function(i) {
    sequential_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], c1 = xx[i, "c1"], c2 = xx[i, "c2"], l1 = xx[i, "l1"], l2 = xx[i, "l2"],
      p1 = xx[i, "p1"], p2 = xx[i, "p2"], cmax = xx[i, "cmax"], fixup = fixup)
  })
  mcol <- if(length(mcol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(mcol))), ncol = n,
    dimnames = list(c(typ[3L], rownames(x)[x$type == typ[3L]]), paste("Color", 1L:n)))

  dcol <- sapply(which(x$type == typ[4L]), function(i) {
    diverging_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], c1 = xx[i, "c1"], l1 = xx[i, "l1"], l2 = xx[i, "l2"],
      p1 = xx[i, "p1"], p2 = xx[i, "p2"], fixup = fixup)
  })
  dcol <- if(length(dcol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(dcol))), ncol = n,
    dimnames = list(c(typ[4L], rownames(x)[x$type == typ[4L]]), paste("Color", 1L:n)))
  
  ## collect colors
  col <- rbind(qcol, scol, mcol, dcol)
  col[is.na(col)] <- "white"
  m <- nrow(col)
  
  ## border color: light gray for sufficiently "discrete" case
  if(is.null(border)) {
    border <- if(n > 15L) "transparent" else "lightgray"
  }
  border <- rep.int(border, n * m)
  border[col == "transparent"] <- "transparent"
  
  ## graphical parameters
  opar <- par(mar = c(0.1, if(m > 15L) 5.5 else 6.5, 0, 0))
  on.exit(par(opar))
  if(is.null(off)) {
    off <- if(n > 15L) c(1, 0.7) else c(0.9, 0.7)
  } else {
    off <- c(1, 1) - off
  }
  lin <- if(m > 15L) 3.5 else 4.5
  cex <- if(m > 15L) 0.7 else 1
  cex <- rep.int(cex, m)
  cex[col[, 1L] == "transparent"] <- 1
  font <- rep.int(1, m)
  font[col[, 1L] == "transparent"] <- 2
  
  ## visualization
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  x1 <- rep(0:(n-1)/n, each = m)
  y1 <- rep.int((m-1):0/m, n)
  rect(x1, y1, x1 + off[1L]/n, y1 + off[2L]/m, col = col, border = border)
  mtext(rownames(col), side = 2, at = y1[1L:m] + 0.5 * off[2L]/m,
    las = 1, line = lin, adj = 0, cex = cex, font = font)
}


#' @rdname hcl_palettes
#' @export
qualitative_hcl <- function(n, h = c(0, 360 * (n - 1)/n), c = 80, l = 60,
  fixup = TRUE, alpha = 1, palette = NULL, rev = FALSE, ..., h1, h2, c1, l1)
{
    ## edge cases
    if(n < 1L) return(character(0L))
    
    ## process HCL coordinates: (1) palette, (2) h/c/l, (3) h1/h2/...
    ## (1) palette
    if(is.character(h)) palette <- h
    pals <- if(!is.null(palette)) {
        as.matrix(hcl_palettes(type = "Qualitative", name = palette)[, 2L:11L])[1L, ]
    } else {
        structure(c(if(length(h) < 2L) c(h, NA) else rep_len(h, 2L), c[1L], NA, l[1L], NA, NA, NA, NA, 1), .Names = vars.pal[1L:10L])
    }
    ## (2) h/c/l
    if(!missing(h) && !is.character(h)) {
        h <- if(length(h) < 2L) c(h, NA) else rep_len(h, 2L)
        pals["h1"] <- h[1L]
        pals["h2"] <- h[2L]
    }
    if(!missing(c)) pals["c1"] <- c
    if(!missing(l))  pals["l1"] <- l
    if(!missing(fixup)) pals["fixup"] <- as.numeric(fixup)
    ## (3) h1/h2/...
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(l1)) pals["l1"] <- l1
    if(is.na(pals["h2"])) pals["h2"] <- pals["h1"] + 360 * (n - 1)/n
    
    ## HCL trajectory
    rval <- hex(polarLUV(
        L = pals["l1"],
        C = pals["c1"],
        H = seq(pals["h1"], pals["h2"], length = n)),
        fixup = as.logical(pals["fixup"]), ...)

    ## alpha transparency
    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
	alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }

    ## return value
    if(rev) rval <- rev(rval)
    return(rval)
}

#' @rdname hcl_palettes
#' @export
sequential_hcl <- function(n, h = 260, c = 80, l = c(30, 90), power = 1.5,
  gamma = NULL, fixup = TRUE, alpha = 1, palette = NULL, rev = FALSE, ...,
  h1, h2, c1, c2, l1, l2, p1, p2, cmax, c.)
{
    ## edge cases
    if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))
    if(!missing(c.)) c <- c.
    if(length(c) == 3L) c <- c[c(1L, 3L, 2L)]

    ## process HCL coordinates: (1) palette, (2) h/c/l, (3) h1/h2/...
    ## (1) palette
    if(is.character(h)) palette <- h
    pals <- if(!is.null(palette)) {
        as.matrix(hcl_palettes(type = "Sequential", name = palette)[, 2L:11L])[1L, ]
    } else {
        structure(c(
	    if(length(h) < 2L) c(h, NA) else rep_len(h, 2L),
	    if(length(c) < 2L) c(c, 0) else rep_len(c, 2L),
	    rep_len(l, 2L),
	    if(length(power) < 2L) c(power, NA) else rep_len(power, 2L),
	    if(length(c) < 3L) NA else c[3L],
	    1), .Names = vars.pal)
    }
    ## (2) h/c/l
    if(!missing(h) && !is.character(h)) {
        h <- if(length(h) < 2L) c(h, NA) else rep_len(h, 2L)
        pals["h1"] <- h[1L]
        pals["h2"] <- h[2L]
    }
    if(!missing(c) || !missing(c.)) {
        if(length(c) < 2L) c <- c(c, 0)
        pals["c1"] <- c[1L]
        pals["c2"] <- c[2L]
	if(length(c) == 3L) pals["cmax"] <- c[3L]
    }
    if(!missing(l)) {
        l <- rep_len(l, 2L)
        pals["l1"] <- l[1L]
        pals["l2"] <- l[2L]
    }
    if(!missing(power)) {
        power <- if(length(power) < 2L) c(power, NA) else rep_len(power, 2L)
        pals["p1"] <- power[1L]
        pals["p2"] <- power[2L]
    }
    if(!missing(fixup)) pals["fixup"] <- as.numeric(fixup)
    ## (3) h1/h2/...
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(c2)) pals["c2"] <- c2
    if(!missing(l1)) pals["l1"] <- l1
    if(!missing(l2)) pals["l2"] <- l2
    if(!missing(p1)) pals["p1"] <- p1
    if(!missing(p2)) pals["p2"] <- p2
    if(!missing(cmax)) pals["cmax"] <- cmax
    if(is.na(pals["h2"])) pals["h2"] <- pals["h1"]
    if(is.na(pals["c2"])) pals["c2"] <- 0
    if(is.na(pals["p2"])) pals["p2"] <- pals["p1"]

    ## HCL trajectory
    cmaxat <- 1/(1 + abs(pals["cmax"] - pals["c1"]) / abs(pals["cmax"] - pals["c2"]))
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(
        L = pals["l2"] - (pals["l2"] - pals["l1"]) * rval^pals["p2"],
        C = if(is.na(cmaxat)) {
	  pals["c2"] - (pals["c2"] - pals["c1"]) * rval^pals["p1"]
	} else {
	  ifelse(rval^pals["p1"] < cmaxat,
	    pals["c2"] - (pals["c2"] - pals["cmax"]) * (rval^pals["p1"])/cmaxat,
            pals["cmax"] - (pals["cmax"] - pals["c1"]) * ((rval^pals["p1"] - cmaxat)/(1 - cmaxat))
	  )
	},
        H = pals["h2"] - (pals["h2"] - pals["h1"]) * rval),
        fixup = as.logical(pals["fixup"]), ...)

    ## alpha transparency
    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
	alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }

    ## return value
    if(rev) rval <- rev(rval)
    return(rval)
}

#' @rdname hcl_palettes
#' @export
diverging_hcl <- function(n, h = c(260, 0), c = 80, l = c(30, 90), power = 1.5,
  gamma = NULL, fixup = TRUE, alpha = 1, palette = NULL, rev = FALSE, ...,
  h1, h2, c1, l1, l2, p1, p2, cmax)
{
    ## edge cases
    if (!is.null(gamma)) warning("'gamma' is deprecated and has no effect")
    if(n < 1L) return(character(0L))
    
    ## process HCL coordinates: (1) palette, (2) h/c/l, (3) h1/h2/...
    ## (1) palette
    if(is.character(h)) palette <- h
    pals <- if(!is.null(palette)) {
        as.matrix(hcl_palettes(type = "Diverging", name = palette)[, 2L:10L])[1L, ]
    } else {
        structure(c(
	  rep_len(h, 2L),
	  c(c[1L], NA),
	  rep_len(l, 2L),
	  if(length(power) < 2L) c(power, NA) else rep_len(power, 2L),
	  if(length(c) > 1L) c[2L] else NA,
	  1), .Names = vars.pal)
    }
    ## (2) h/c/l
    if(!missing(h) && !is.character(h)) {
        h <- rep_len(h, 2L)
        pals["h1"] <- h[1L]
        pals["h2"] <- h[2L]
    }
    if(!missing(c)) {
      pals["c1"] <- c[1L]
      if(length(c) > 1L) pals["cmax"] <- c[2L]
    }
    if(!missing(l)) {
        l <- rep_len(l, 2L)
        pals["l1"] <- l[1L]
        pals["l2"] <- l[2L]
    }
    if(!missing(power)) {
        power <- if(length(power) < 2L) c(power, NA) else rep_len(power, 2L)
        pals["p1"] <- power[1L]
        pals["p2"] <- power[2L]
    }
    if(!missing(fixup)) pals["fixup"] <- as.numeric(fixup)
    ## (3) h1/h2/...
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    if(!missing(c1)) pals["c1"] <- c1
    if(!missing(l1)) pals["l1"] <- l1
    if(!missing(l2)) pals["l2"] <- l2
    if(!missing(p1)) pals["p1"] <- p1
    if(!missing(p2)) pals["p2"] <- p2
    if(!missing(cmax)) pals["cmax"] <- cmax
    pals["c2"] <- NA
    if(is.na(pals["p2"])) pals["p2"] <- pals["p1"]

    ## HCL trajectory
    cmaxat <- 1/(1 + abs(pals["cmax"] - pals["c1"]) / pals["cmax"])
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(
        L = pals["l2"] - (pals["l2"] - pals["l1"]) * abs(rval)^pals["p2"],
        C = if(is.na(cmaxat)) {
              pals["c1"] * abs(rval)^pals["p1"]
	    } else {
	      ifelse(abs(rval)^pals["p1"] < cmaxat,
	        pals["cmax"] * (abs(rval)^pals["p1"])/cmaxat,
                pals["cmax"] - (pals["cmax"] - pals["c1"]) * ((abs(rval)^pals["p1"] - cmaxat)/(1 - cmaxat))
	      )
	    },
        H = ifelse(rval > 0, pals["h1"], pals["h2"])),
        fixup = as.logical(pals["fixup"]), ...)

    ## alpha transparency
    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
	alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }

    ## return value
    if(rev) rval <- rev(rval)
    return(rval)
}

#' @rdname hcl_palettes
#' @export
diverge_hcl <- diverging_hcl

# -------------------------------------------------------------------
# Palette specifications
# -------------------------------------------------------------------

vars.pal <- c("h1", "h2", "c1", "c2", "l1", "l2", "p1", "p2", "cmax", "fixup")
                                                                              # Inspired by:
qual.pals <- list()
qual.pals[["Pastel 1"]]    <- c(  0,   NA,  35, NA, 85, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Pastel1
qual.pals[["Dark 2"]]      <- c(  0,   NA,  50, NA, 60, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Dark2
qual.pals[["Dark 3"]]      <- c(  0,   NA,  80, NA, 60, NA,  NA,  NA,  NA, 1) # JCF/Z: ~Dark2 with more chroma
qual.pals[["Set 2"]]       <- c(  0,   NA,  60, NA, 70, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Set2
qual.pals[["Set 3"]]       <- c( 10,   NA,  50, NA, 80, NA,  NA,  NA,  NA, 1) # ColorBrewer.org: Set3
qual.pals[["Warm"]]        <- c( 90,  -30,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Warm (based on Ihaka-03)
qual.pals[["Cold"]]        <- c(270,  150,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Cold (based on Ihaka-03)
qual.pals[["Harmonic"]]    <- c( 60,  240,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Harmonic (based on Ihaka-03)
qual.pals[["Dynamic"]]     <- c( 30,   NA,  50, NA, 70, NA,  NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Dynamic (based on Ihaka-03)

seqs.pals <- list()
seqs.pals[["Grays"]]       <- c(  0,   NA,   0, NA, 10, 98, 1.3,  NA,  NA, 1) # ColorBrewer.org: Greys
seqs.pals[["Light Grays"]] <- c(  0,   NA,   0, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.5: Light Grays
seqs.pals[["Blues 2"]]     <- c(260,   NA,  80, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.5: Blues
seqs.pals[["Blues 3"]]     <- c(245,   NA,  50, NA, 20, 98, 0.8, 1.4,  75, 1) # ColorBrewer.org: Blues
seqs.pals[["Purples 2"]]   <- c(270,   NA,  70, NA, 25, 95, 1.2,  NA,  NA, 1) # ColorBrewer.org: Purples
seqs.pals[["Purples 3"]]   <- c(270,   NA,  50, NA, 20, 98, 0.9, 1.4,  75, 1) # ColorBrewer.org: Purples
seqs.pals[["Reds 2"]]      <- c( 10,   NA,  85, NA, 25, 95, 1.3,  NA,  NA, 1) # ColorBrewer.org: Reds
seqs.pals[["Reds 3"]]      <- c( 10,   NA,  65, NA, 20, 97, 1.1, 1.3, 150, 1) # ColorBrewer.org: Reds
seqs.pals[["Greens 2"]]    <- c(135,   NA,  45, NA, 35, 95, 1.3,  NA,  NA, 1) # ColorBrewer.org: Greens
seqs.pals[["Greens 3"]]    <- c(135,   NA,  35, NA, 25, 98, 1.0, 1.5,  70, 1) # ColorBrewer.org: Greens

seqm.pals <- list()
seqm.pals[["Purple-Blue"]] <- c(300,  200,  60,  0, 25, 95, 0.7, 1.3,  NA, 1) # ColorBrewer.org: BuPu
seqm.pals[["Red-Purple"]]  <- c( 10,  -80,  80,  5, 25, 95, 0.7, 1.3,  NA, 1) # ColorBrewer.org: PuRd
seqm.pals[["Red-Blue"]]    <- c(  0, -100,  80, 40, 40, 75, 1.0, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: Red-Blue
seqm.pals[["Purple-Orange"]]<-c(-83,   20,  65, 18, 32, 90, 0.5, 1.0,  NA, 1) # CARTO: PurpOr
seqm.pals[["Blue-Yellow"]] <- c(265,   80,  60, 10, 25, 95, 0.7, 2.0,  NA, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Green-Yellow"]]<- c(140,   80,  50, 10, 40, 97, 0.7, 1.8,  NA, 1) # ColorBrewer.org: YlGn
seqm.pals[["Red-Yellow"]]  <- c( 10,   85,  80, 10, 25, 95, 0.4, 1.3,  NA, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["Heat"]]        <- c(  0,   90,  80, 30, 30, 90, 0.2, 2.0,  NA, 1) # JCF/Z: alternative to heat_hcl
seqm.pals[["Heat 2"]]      <- c(  0,   90, 100, 30, 50, 90, 0.2, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: heat_hcl
seqm.pals[["Terrain"]]     <- c(130,    0,  80,  0, 60, 95, 0.1, 1.0,  NA, 1) # Z+KH+PM-09, Fig.5: terrain_hcl
seqm.pals[["Terrain 2"]]   <- c(130,   30,  65,  0, 45, 90, 0.5, 1.5,  NA, 1) # JCF/Z: alternative to terrain_hcl

seqm.pals[["Viridis"]]     <- c(300,   75,  40, 95, 15, 90, 1.0, 1.1,  NA, 1) # viridis::viridis
seqm.pals[["Plasma"]]      <-c(-100,  100,  60,100, 15, 95, 2.0, 0.9,  NA, 1) # viridis::plasma
seqm.pals[["Inferno"]]     <-c(-100,   85,   0, 65,  1, 98, 1.1, 0.9, 120, 1) # viridis::inferno

seqm.pals[["Dark Mint"]]   <- c(240,  130,  30, 33, 25, 95, 1.0,  NA,  NA, 1) # CARTO: Dark Mint
seqm.pals[["Mint"]]        <- c(205,  140,  40, 12, 34, 94, 0.5, 1.0,  NA, 1) # CARTO: Mint
seqm.pals[["BluGrn"]]      <- c(215,  120,  25, 30, 31, 88, 0.7, 1.1,  45, 1) # CARTO: BluGrn
seqm.pals[["Teal"]]        <- c(240,  180,  35, 15, 35, 92, 0.6, 1.1,  40, 1) # CARTO: Teal
seqm.pals[["TealGrn"]]     <- c(220,  125,  44, 50, 49, 90, 0.8, 1.2,  60, 1) # CARTO: TealGrn
seqm.pals[["Emrld"]]       <- c(224,  105,  23, 55, 25, 92, 1.5, 1.0,  NA, 1) # CARTO: Emrld
seqm.pals[["BluYl"]]       <- c(250,   90,  40, 55, 33, 98, 0.5, 1.0,  NA, 1) # CARTO: BluYl
seqm.pals[["ag_GrnYl"]]    <- c(225,   87,  27, 86, 34, 92, 0.9,  NA,  NA, 1) # CARTO: ag_GrnYl
seqm.pals[["Peach"]]       <- c( 15,   50, 128, 30, 55, 90, 1.1,  NA,  NA, 1) # CARTO: Peach
seqm.pals[["PinkYl"]]      <- c( -4,   80, 100, 47, 55, 96, 1.0,  NA,  NA, 1) # CARTO: PinkYl
seqm.pals[["Burg"]]        <- c(-10,   10,  40, 40, 25, 85, 1.2, 1.0,  75, 1) # CARTO: Burg
seqm.pals[["BurgYl"]]      <- c(-10,   55,  45, 30, 30, 90, 0.7, 1.0,  80, 1) # CARTO: BurgYl
seqm.pals[["RedOr"]]       <- c( -3,   53,  75, 42, 44, 86, 0.8, 1.0,  90, 1) # CARTO: RedOr
seqm.pals[["OrYel"]]       <- c(  5,   72, 120, 49, 56, 87, 1.0,  NA, 125, 1) # CARTO: OrYel
seqm.pals[["Purp"]]        <- c(270,  300,  55, 20, 42, 92, 0.6, 1.0,  60, 1) # CARTO: Purp
seqm.pals[["PurpOr"]]      <- c(-83,   20,  55, 18, 32, 90, 0.6, 1.0,  65, 1) # CARTO: PurpOr
seqm.pals[["Sunset"]]      <- c(-80,   78,  60, 55, 40, 91, 0.8, 1.0,  75, 1) # CARTO: Sunset
seqm.pals[["Magenta"]]     <- c(312,  358,  50, 24, 27, 85, 0.6, 1.1,  65, 1) # CARTO: Magenta
seqm.pals[["SunsetDark"]]  <- c(-35,   50,  55, 60, 30, 90, 1.2, 1.0, 120, 1) # CARTO: SunsetDark
seqm.pals[["ag_Sunset"]]   <- c(-85,   70,  70, 45, 25, 85, 0.6, 1.0, 105, 1) # CARTO: ag_Sunset
seqm.pals[["BrwnYl"]]      <- c(-20,   70,  30, 20, 20, 90, 1.0, 1.1,  60, 1) # CARTO: BrwnYl

seqm.pals[["YlOrRd"]]      <- c(  5,   85,  75, 40, 25, 99, 1.6, 1.3, 180, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["YlOrBr"]]      <- c( 20,   85,  50, 20, 25, 99, 1.3, 1.5, 150, 1) # ColorBrewer.org: YlOrBr
seqm.pals[["OrRd"]]        <- c(  0,   60,  90, 10, 25, 97, 1.0, 1.5, 135, 1) # ColorBrewer.org: OrRd
seqm.pals[["Oranges"]]     <- c( 20,   55,  70, 10, 30, 97, 1.2, 1.3, 150, 1) # ColorBrewer.org: Oranges
seqm.pals[["YlGn"]]        <- c(160,   85,  25, 20, 25, 99, 1.2, 1.6,  70, 1) # ColorBrewer.org: YlGn
seqm.pals[["YlGnBu"]]      <- c(270,   90,  40, 25, 15, 99, 2.0, 1.5,  90, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Reds"]]        <- c(  0,   35,  65,  5, 20, 97, 1.1, 1.3, 150, 1) # ColorBrewer.org: Reds
seqm.pals[["RdPu"]]        <- c(-70,   40,  45,  5, 15, 97, 1.0, 1.3, 100, 1) # ColorBrewer.org: RdPu
seqm.pals[["PuRd"]]        <- c( 20,  -95,  60,  5, 20, 97, 1.6, 1.1, 140, 1) # ColorBrewer.org: PuRd
seqm.pals[["Purples"]]     <- c(275,  270,  55,  5, 20, 99, 1.3, 1.3,  70, 1) # ColorBrewer.org: Purples
seqm.pals[["PuBuGn"]]      <- c(160,  320,  25,  5, 25, 98, 1.4, 1.2,  70, 1) # ColorBrewer.org: PuBuGn
seqm.pals[["PuBu"]]        <- c(240,  260,  30,  5, 25, 98, 1.5, 1.2,  70, 1) # ColorBrewer.org: PuBu
seqm.pals[["Greens"]]      <- c(135,  115,  35,  5, 25, 98, 1.0, 1.5,  70, 1) # ColorBrewer.org: Greens
seqm.pals[["BuGn"]]        <- c(125,  200,  30,  5, 25, 98, 1.4, 1.6,  65, 1) # ColorBrewer.org: BuGn
seqm.pals[["GnBu"]]        <- c(265,   95,  55, 10, 25, 97, 1.3, 1.7,  80, 1) # ColorBrewer.org: GnBu
seqm.pals[["BuPu"]]        <- c(320,  200,  40,  5, 15, 98, 1.2, 1.3,  65, 1) # ColorBrewer.org: BuPu
seqm.pals[["Blues"]]       <- c(260,  220,  45,  5, 25, 98, 1.2, 1.3,  70, 1) # ColorBrewer.org: Blues

dive.pals <- list()
dive.pals[["Blue-Red"]]    <- c(260,    0,  80, NA, 30, 90, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (high luminance contrast)
dive.pals[["Blue-Red 2"]]  <- c(260,    0, 100, NA, 50, 90, 1.0,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (medium luminance contrast)
dive.pals[["Blue-Red 3"]]  <- c(255,   12,  50, NA, 20, 97, 1.0, 1.3,  80, 1) # ColorBrewer.org: RdBu
dive.pals[["Red-Green"]]   <- c(340,  128,  60, NA, 30, 97, 0.8, 1.5,  80, 1) # ColorBrewer.org: PiYG
dive.pals[["Purple-Green"]]<- c(300,  128,  30, NA, 20, 95, 1.0, 1.4,  65, 1) # ColorBrewer.org: PRGn
dive.pals[["Purple-Brown"]]<- c(270,   40,  30, NA, 20, 98, 0.8, 1.2,  70, 1) # ColorBrewer.org: PuOr
dive.pals[["Green-Brown"]] <- c(180,   55,  40, NA, 25, 97, 0.8, 1.4,  65, 1) # ColorBrewer.org: BrBG
dive.pals[["Blue-Yellow 2"]]<-c(265,   80,  80, NA, 40, 95, 1.2,  NA,  NA, 1) # Z+COW
dive.pals[["Blue-Yellow 3"]]<-c(265,   80,  80, NA, 70, 95, 0.5, 2.0,  NA, 1) # Z+COW
dive.pals[["Green-Orange"]]<- c(130,   43, 100, NA, 70, 90, 1.0,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Green-Orange (low luminance contrast)
dive.pals[["Cyan-Magenta"]]<- c(180,  330,  59, NA, 75, 95, 1.5,  NA,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (low luminance contrast)
dive.pals[["Tropic"]]      <- c(195,  325,  70, NA, 55, 95, 1.0,  NA,  NA, 1) # CARTO: Tropic



base.pals <- list()
base.pals[["rainbow"]]        <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default RGB rainbow
base.pals[["heat.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default heatmap
base.pals[["topo.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default topo colors
base.pals[["terrain.colors"]] <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default terrain colors
base.pals[["cm.colors"]]      <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Default cyan magenta colors
base.pals[["bpy"]]            <- c(NA, NA, NA, NA, NA, NA, NA, NA,  NA, 1)    # Analogous to sp::bpy.colors

## collect all hcl palettes
make_hcl_pals <- function() {
  ## collect all palettes by group
  qpals <- as.data.frame(do.call("rbind", qual.pals))
  rownames(qpals) <- names(qual.pals)
  qpals$type <- "Qualitative"

  spals <- as.data.frame(do.call("rbind", seqs.pals))
  rownames(spals) <- names(seqs.pals)
  spals$type <- "Sequential (single-hue)"

  mpals <- as.data.frame(do.call("rbind", seqm.pals))
  rownames(mpals) <- names(seqm.pals)
  mpals$type <- "Sequential (multi-hue)"

  dpals <- as.data.frame(do.call("rbind", dive.pals))
  rownames(dpals) <- names(dive.pals)
  dpals$type <- "Diverging"
  
  ## combine and rearrange
  pals <- rbind(qpals, spals, mpals, dpals)
  names(pals) <- c(vars.pal, "type")
  pals$type <- factor(pals$type, levels = c("Qualitative",
    "Sequential (single-hue)", "Sequential (multi-hue)", "Diverging"))
  pals$fixup <- as.logical(pals$fixup)
  pals <- pals[, c(11L, 1L:10L)]
  return(pals)
}
hcl_pals <- make_hcl_pals()


# -------------------------------------------------------------------
# Character vector specifying the example plots. For each element
# the function plot_<name> will be called.
# -------------------------------------------------------------------
example.plots <- c("Map", "Heatmap", "Scatter", "Spine", "Bar",
                   "Pie", "Perspective", "Mosaic", "Lines", "Spectrum")

# -------------------------------------------------------------------
# Helper function: returns a data.frame containing all
# palettes specified above. Used for hclwizard
# -------------------------------------------------------------------
GetPaletteConfig <- function() {
   res <- NULL
   palettes <- list(
     qual = qual.pals,
     seqs = seqs.pals,
     seqm = seqm.pals,
     dive = dive.pals,
     base = base.pals
   )
   for ( i in 1:length(palettes) ) {
      tmp <- data.frame(matrix(unlist(palettes[[i]]), ncol = 9, byrow = TRUE))
      names(tmp) <- toupper(vars.pal)
      tmp$name   <- names(palettes[[i]])
      tmp$typ    <- names(palettes)[i]
      res        <- rbind(tmp, res)
   }
   res
}
