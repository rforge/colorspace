#' HCL Color Palettes
#' 
#' Qualitative, sequential (single-hue and multi-hue), and diverged
#' color palettes based on the HCL (hue-chroma-luminance) color model.
#' 
#' Details are still tbd...
#' 
#' @keywords color
#' @examples
#' ## overview of all _named_ HCL palettes
#' hcl_palettes()
#'
#' ## visualize
#' hcl_palettes("qualitative", plot = TRUE)
#' hcl_palettes("sequential (single-hue)", plot = TRUE)
#' hcl_palettes("sequential (multi-hue)", plot = TRUE)
#' hcl_palettes("diverging", plot = TRUE)
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
  pals <- pals[, c(10L, 1L:9L)]

  ## subset by type and name (by flexible matching)
  fx <- function(n) {
    for(char in c(" ", "-", ".", "(", ")", "_")) n <- gsub(char, "", n, fixed = TRUE)
    tolower(n)
  }
  if(!is.null(type)) {
    tytab <- c("sequential", fx(levels(pals$type)))
    type <- lapply(type, function(ty) {
      ty <- startsWith(tytab, fx(ty))
      if(all(!ty)) stop("Palette 'type' should be one of: ", paste(levels(pals$type), collapse = ", "))
      ty <- tytab[which(ty)[1L]]
      if(ty == "sequential") ty <- c("sequentialsinglehue", "sequentialmultihue")
      return(ty)
    })
    type <- unlist(type)
    type <- levels(pals$type)[tytab[-1L] %in% type]
    pals <- pals[as.character(pals$type) %in% type, , drop = FALSE]
  }
  if(!is.null(name)) {
    namtab <- fx(rownames(pals))
    name <- sapply(name, function(n) {
      n <- startsWith(namtab, fx(n))
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
#' @method print hcl_palettes
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
    print.data.frame(x[ , 2L:9L, drop = FALSE], row.names = FALSE, ...)
  }
  invisible(x)
}

#' @rdname hcl_palettes
#' @method summary hcl_palettes
#' @export
summary.hcl_palettes <- function(object, ...) {
  type <- unique(as.character(object$type))
  cat("HCL palettes\n")
  for(ty in type) {
    cat("\nType:", ty, "\n")
    cat("Parameter ranges:\n")
    print.data.frame(object[as.character(object$type) %in% ty, 2L:9L, drop = FALSE], ...)
  }
  invisible(object)
}

#' @rdname hcl_palettes
#' @method plot hcl_palettes
#' @export
plot.hcl_palettes <- function(x, n = 5L, fixup = TRUE, ...)
{
  typ <- levels(x$type)
  x$type <- as.character(x$type)
  xx <- as.matrix(x[, 2L:9L])
  
  qcol <- sapply(which(x$type == typ[1L]), function(i) {
    qualitative_hcl(n = n, h1 = xx[i, "h1"], h2 = xx[i, "h2"], c = xx[i, "c1"], l = xx[i, "l1"], fixup = fixup)
  })
  qcol <- if(length(qcol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(qcol))), ncol = n,
    dimnames = list(c(typ[1L], rownames(x)[x$type == typ[1L]]), paste("Color", 1L:n)))

  scol <- sapply(which(x$type == typ[2L]), function(i) {
    sequential_hcl(n = n, h = xx[i, "h1"], c. = xx[i, c("c1", "c2")], l = xx[i, c("l1", "l2")],
      power = na.omit(xx[i, c("p1", "p2")]), fixup = fixup)
  })
  scol <- if(length(scol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(scol))), ncol = n,
    dimnames = list(c(typ[2L], rownames(x)[x$type == typ[2L]]), paste("Color", 1L:n)))

  mcol <- sapply(which(x$type == typ[3L]), function(i) {
    heat_hcl(n = n, h = xx[i, c("h1", "h2")], c. = xx[i, c("c1", "c2")], l = xx[i, c("l1", "l2")],
      power = na.omit(xx[i, c("p1", "p2")]), fixup = fixup)
  })
  mcol <- if(length(mcol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(mcol))), ncol = n,
    dimnames = list(c(typ[3L], rownames(x)[x$type == typ[3L]]), paste("Color", 1L:n)))

  dcol <- sapply(which(x$type == typ[4L]), function(i) {
    diverge_hcl(n = n, h = xx[i, c("h1", "h2")], c = xx[i, c("c1", "c2")], l = xx[i, c("l1", "l2")],
      power = na.omit(xx[i, c("p1", "p2")]), fixup = fixup)
  })
  dcol <- if(length(dcol) < 1L) NULL else matrix(as.vector(rbind("transparent", t(dcol))), ncol = n,
    dimnames = list(c(typ[4L], rownames(x)[x$type == typ[4L]]), paste("Color", 1L:n)))
  
  ## collect colors
  col <- rbind(qcol, scol, mcol, dcol)
  col[is.na(col)] <- "white"
  m <- nrow(col)
  
  ## border color: light gray for sufficiently "discrete" case
  border <- rep.int(if(n > 15L) "transparent" else "lightgray", n * m)
  border[col == "transparent"] <- "transparent"
  
  ## graphical parameters
  opar <- par(mar = c(0.1, if(m > 15L) 5.5 else 6.5, 0, 0))
  on.exit(par(opar))
  off <- if(n > 15L) c(1, 0.8) else c(0.6, 0.8)
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
qualitative_hcl <- function(n, h = c(0, 360 * (n - 1)/n), c. = 50, l = 70,
  fixup = TRUE, alpha = 1, palette = NULL, ..., h1, h2)
{
    ## if nothing is requested
    if(n < 1L) return(character(0L))
    
    ## process HCL coordinates: (1) palette, (2) h/c/l, (3) h1/h2
    if(is.character(h)) palette <- h
    pals <- if(!is.null(palette)) {
      as.matrix(hcl_palettes(name = palette)[, 2L:9L])[1L, ]
    } else {
      pals <- structure(c(h, c., NA, l, NA, NA, NA, NA), .Names = vars.pal)
    }
    
    if(!missing(h) && !is.character(h)) {
      h <- rep_len(h, 2L)
      pals["h1"] <- h[1L]
      pals["h2"] <- h[2L]
    }
    if(!missing(c.)) pals["c1"] <- c.
    if(!missing(l)) pals["l1"] <- l
    if(!missing(h1)) pals["h1"] <- h1
    if(!missing(h2)) pals["h2"] <- h2
    
    rval <- hex(polarLUV(L = pals["l1"], C = pals["c1"], H = seq(pals["h1"], pals["h2"], length = n)),
                fixup = fixup, ...)

    if(!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
	alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)),
	                width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }

    return(rval)
}


# -------------------------------------------------------------------
# Palette specifications
# -------------------------------------------------------------------

vars.pal <- c("h1", "h2", "c1", "c2", "l1", "l2", "p1", "p2", "fixup")
                                                                         # Inspired by:
qual.pals <- list()
qual.pals[["Pastel 1"]]    <- c(  0,  288,  35, NA, 85, NA,  NA,  NA, 1) # ColorBrewer.org: Pastel1
qual.pals[["Dark 2"]]      <- c(  0,  288,  50, NA, 60, NA,  NA,  NA, 1) # ColorBrewer.org: Dark2
qual.pals[["Dark 3"]]      <- c(  0,  300,  80, NA, 60, NA,  NA,  NA, 1) # JCF/Z: ~Dark2 with more chroma
qual.pals[["Set 2"]]       <- c(  0,  288,  60, NA, 70, NA,  NA,  NA, 1) # ColorBrewer.org: Set2
qual.pals[["Set 3"]]       <- c( 10,  320,  50, NA, 80, NA,  NA,  NA, 1) # ColorBrewer.org: Set3
qual.pals[["Warm"]]        <- c( 90,  -30,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Warm (based on Ihaka-03)
qual.pals[["Cold"]]        <- c(270,  150,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Cold (based on Ihaka-03)
qual.pals[["Harmonic"]]    <- c( 60,  240,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Harmonic (based on Ihaka-03)
qual.pals[["Dynamic"]]     <- c( 30,  300,  50, NA, 70, NA,  NA,  NA, 1) # Z+KH+PM-09, Fig.4: Dynamic (based on Ihaka-03)

seqs.pals <- list()
seqs.pals[["Grays"]]       <- c(  0,   NA,   0,  0, 15, 95, 1.3,  NA, 1) # ColorBrewer.org: Greys
seqs.pals[["Light Grays"]] <- c(260,   NA,   0,  0, 30, 90, 1.5,  NA, 1) # Z+KH+PM-09, Fig.5: Light Grays
seqs.pals[["Blues"]]       <- c(260,   NA,  80, 10, 30, 95, 0.7,  NA, 1) # ColorBrewer.org: Blues
seqs.pals[["Blues 2"]]     <- c(260,   NA,  80,  0, 30, 90, 1.5,  NA, 1) # Z+KH+PM-09, Fig.5: Blues
seqs.pals[["Purples"]]     <- c(280,   NA,  60,  5, 20, 95, 0.7,  NA, 1) # ColorBrewer.org: Purples
seqs.pals[["Reds"]]        <- c( 10,   NA,  80, 10, 30, 95, 0.7,  NA, 1) # JCF/Z: Reds
seqs.pals[["Oranges"]]     <- c( 20,   NA,  80,  5, 35, 95, 0.6,  NA, 1) # ColorBrewer.org: Oranges
seqs.pals[["Greens"]]      <- c(135,   NA,  50, 10, 40, 95, 0.4,  NA, 1) # ColorBrewer.org: Greens

seqm.pals <- list()
seqm.pals[["Purple-Blue"]] <- c(300,  200,  60,  0, 25, 95, 0.7, 1.3, 1) # ColorBrewer.org: BuPu
seqm.pals[["Pure Red"]]    <- c(370,  280,  80,  5, 25, 95, 0.7, 1.3, 1) # ColorBrewer.org: PuRd
seqm.pals[["Green-Yellow"]]<- c(140,   80,  40, 10, 35, 95, 0.7, 1.7, 1) # ColorBrewer.org: YlGn
seqm.pals[["Blue-Yellow"]] <- c(265,   80,  60, 10, 25, 95, 0.7, 2.0, 1) # ColorBrewer.org: YlGnBu
seqm.pals[["Red-Yellow"]]  <- c( 10,   85,  80, 10, 25, 95, 0.4, 1.3, 1) # ColorBrewer.org: YlOrRd
seqm.pals[["Heat"]]        <- c(  0,   90,  80, 30, 30, 90, 0.2, 2.0, 1) # JCF/Z: alternative to heat_hcl
seqm.pals[["Heat 2"]]      <- c(  0,   90, 100, 30, 50, 90, 0.2, 1.0, 1) # Z+KH+PM-09, Fig.5: heat_hcl
seqm.pals[["Terrain 2"]]   <- c(130,   30,  65,  0, 45, 90, 0.5, 1.5, 1) # JCF/Z: alternative to terrain_hcl
seqm.pals[["Terrain"]]     <- c(130,    0,  80,  0, 60, 95, 0.1, 1.0, 1) # Z+KH+PM-09, Fig.5: terrain_hcl
seqm.pals[["Red-Blue"]]    <- c(  0, -100,  80, 40, 40, 75, 1.0, 1.0, 1) # Z+KH+PM-09, Fig.5: Red-Blue
seqm.pals[["Viridis"]]     <- c(300,   75,  35, 95, 15, 90, 0.8, 1.2, 1) # viridis::viridis
seqm.pals[["Plasma"]]      <-c(-100,  100,  60,100, 15, 95, 2.0, 0.9, 1) # viridis::plasma

dive.pals <- list()
dive.pals[["Blue-Red"]]    <- c(260,    0,  80, NA, 30, 90, 1.5,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (high luminance contrast)
dive.pals[["Blue-Red 2"]]  <- c(260,    0, 100, NA, 50, 90, 1.0,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (medium luminance contrast)
dive.pals[["Blue-Red 3"]]  <- c(265,   12,  80, NA, 25, 95, 0.7,  NA, 1) # ColorBrewer.org: RdBu
dive.pals[["Red-Green"]]   <- c(340,  128,  45, NA, 35, 95, 0.7,  NA, 1) # ColorBrewer.org: PiYG
dive.pals[["Purple-Green"]]<- c(300,  128,  45, NA, 30, 95, 0.7,  NA, 1) # ColorBrewer.org: PRGn
dive.pals[["Purple-Brown"]]<- c(270,   40,  45, NA, 30, 95, 0.7,  NA, 1) # ColorBrewer.org: PuOr
dive.pals[["Green-Brown"]] <- c(160,   55,  30, NA, 35, 95, 0.7,  NA, 1) # ColorBrewer.org: BrBG
dive.pals[["Green-Orange"]]<- c(130,   43, 100, NA, 70, 90, 1.0,  NA, 1) # Z+KH+PM-09, Fig.6: Green-Orange (low luminance contrast)
dive.pals[["Cyan-Magenta"]]<- c(180,  330,  59, NA, 75, 95, 1.5,  NA, 1) # Z+KH+PM-09, Fig.6: Blue-Red (low luminance contrast)

base.pals <- list()
base.pals[["rainbow"]]        <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1)    # Default RGB rainbow
base.pals[["heat.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1)    # Default heatmap
base.pals[["topo.colors"]]    <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1)    # Default topo colors
base.pals[["terrain.colors"]] <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1)    # Default terrain colors
base.pals[["cm.colors"]]      <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1)    # Default cyan magenta colors
base.pals[["bpy"]]            <- c(NA, NA, NA, NA, NA, NA, NA, NA, 1)    # Analogous to sp::bpy.colors


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
   palettes <- list("qual"=qual.pals,"seqs"=seqs.pals,"seqm"=seqm.pals,"dive"=dive.pals,"base"=base.pals)
   for ( i in 1:length(palettes) ) {
      tmp <- data.frame(matrix(unlist(palettes[[i]]),ncol=9,byrow=T))
      names(tmp) <- toupper(vars.pal)
      tmp$name   <- names(palettes[[i]])
      tmp$typ    <- names(palettes)[i]
      res        <- rbind(tmp,res)
   }
   res
}
