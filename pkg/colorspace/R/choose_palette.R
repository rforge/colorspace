#' Graphical User Interface for Choosing HCL Color Palettes
#' 
#' A graphical user interface (GUI) for viewing, manipulating, and choosing HCL
#' color palettes.
#' 
#' Computes palettes based on the HCL (hue-chroma-luminance) color model (as
#' implemented by \code{\link{polarLUV}}). The GUIs interface the palette
#' functions \code{\link{qualitative_hcl}} for qualitative palettes,
#' \code{\link{sequential_hcl}} for sequential palettes with a single or
#' multiple hues, and \code{\link{diverging_hcl}} for diverging palettes (composed
#' from two single-hue sequential palettes).
#' 
#' Two different GUIs are implemented and can be selected using the function
#' input argument \code{gui} (\code{"tcltk"} or \code{"shiny"}). Both GUIs
#' allows for interactive modification of the arguments of the respective
#' palette-generating functions, i.e., starting/ending hue (wavelength, type of
#' color), minimal/maximal chroma (colorfulness), minimal maximal luminance
#' (brightness, amount of gray), and a power transformations that control how
#' quickly/slowly chroma and/or luminance are changed through the palette.
#' Subsets of the parameters may not be applicable depending on the type of
#' palette chosen. See \code{\link{qualitative_hcl}} and Zeileis et al. (2009) for
#' a more detailed explanation of the different arguments. Stauffer et al.
#' (2015) provide more examples and guidance.
#' 
#' Optionally, active palette can be illustrated by using a range of examples
#' such as a map, heatmap, scatter plot, perspective 3D surface etc.
#' 
#' To demonstrate different types of deficiencies, the active palette may be
#' desaturated (emulating printing on a grayscale printer) and collapsed to
#' emulate different types of color-blindness (without red-green or green-blue
#' contrasts) using the \code{\link{simulate_cvd}} functions.
#' 
#' \code{choose_palette} by default starts the Tcl/Tk version of the GUI while
#' \code{hclwizard} by default starts the shiny version. \code{hcl_wizard} is
#' an alias for \code{hclwizard}.
#' 
#' @param pal function; the initial palette, see \sQuote{Value} below.  Only
#' used if \code{gui = "tcltk"}.
#' @param n integer; the initial number of colors in the palette.
#' @param parent tkwin; the GUI parent window.  Only used if \code{gui =
#' "tcltk"}.
#' @param gui character; GUI to use. Available options are \code{tcltk} and
#' \code{shiny}, see \sQuote{Details} below.
#' @param shiny.trace boolean, default \code{FALSE}. Used for debugging if
#' \code{gui = "shiny"}.
#' @param ... Forwarded, used for development and demonstration purposes only.
#' Currently considered: \code{verbose} (logical, default \code{FALSE}) and
#' \code{autohclplot} (logical, default \code{FALSE}), and \code{shiny.trace}
#' (logical, default \code{FALSE}).
#' @return Returns a palette-generating function with the selected arguments.
#' Thus, the returned function takes an integer argument and returns the
#' corresponding number of HCL colors by traversing HCL space through
#' interpolation of the specified hue/chroma/luminance/power values.
#' @author Jason C. Fisher, Reto Stauffer, Achim Zeileis
#' @seealso \code{\link{simulate_cvd}}, \code{\link{desaturate}}, \code{\link{qualitative_hcl}}.
#' @references Zeileis A, Hornik K, Murrell P (2009).  Escaping RGBland:
#' Selecting Colors for Statistical Graphics.  \emph{Computational Statistics &
#' Data Analysis}, \bold{53}, 3259--3270.
#' \doi{10.1016/j.csda.2008.11.033}
#' Preprint available from
#' \url{https://eeecon.uibk.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf}.
#' 
#' Stauffer R, Mayr GJ, Dabernig M, Zeileis A (2015).  Somewhere over the
#' Rainbow: How to Make Effective Use of Colors in Meteorological
#' Visualizations.  \emph{Bulletin of the American Meteorological Society},
#' \bold{96}(2), 203--216.
#' \doi{10.1175/BAMS-D-13-00155.1}
#' @keywords misc
#' @examples
#' if(interactive()) {
#' ## Using tcltk GUI
#' pal <- choose_palette()
#' ## or equivalently: hclwizard(gui = "tcltk")
#' 
#' ## Using shiny GUI
#' pal <- hclwizard()
#' ## or equivalently: choose_palette(gui = "shiny")
#' 
#' ## use resulting palette function
#' filled.contour(volcano, color.palette = pal, asp = 1)
#' }
#' @importFrom grDevices dev.cur dev.list dev.new dev.off dev.set

#' @export
choose_palette <- function(pal = diverging_hcl, n = 7L, parent = NULL, gui = "tcltk", ...) {
   args <- list("pal" = pal, "n" = n, "parent" = parent, ...)
   gui <- match.arg(gui, c("tcltk", "shiny"))
   do.call(sprintf("choose_palette_%s", gui), args)
}

#' @rdname choose_palette
#' @export
hclwizard <- function(n = 7L, gui = "shiny", ...) {
   args <- list("n" = n, ...)
   gui <- match.arg(gui, c("tcltk", "shiny"))
   do.call(sprintf("choose_palette_%s", gui), args)
}

#' @rdname choose_palette
#' @usage NULL
#' @export
hcl_wizard <- function(n = 7L, gui = "shiny", ...)
   hclwizard(n = n, gui = gui, ...)

# hclwizard shiny GUI for selecting color palette
choose_palette_shiny <- function(pal, n = 7L, ...) {
   # Requirements for shiny application
   stopifnot(requireNamespace("shiny"), requireNamespace("shinyjs"))
   appDir <- system.file("hclwizard", package = "colorspace")
   if (appDir == "")
      stop("Could not find hclwizard app directory. Try re-installing `colorspace`.", call. = FALSE)
   # Start shiny
   Sys.setenv("hclwizard_Ninit" = n)
   dots         <- list(...)
   autohclplot <<- ifelse(is.null(dots$autohclplot), FALSE, as.logical(dots$autohclplot))
   Sys.setenv("hclwizard_autohclplot" = autohclplot)
   shiny.trace <- ifelse(is.null(list(...)$shiny.trace), FALSE, as.logical(list(...)$shiny.trace))
   options(shiny.trace = shiny.trace)
   pal <- shiny::runApp(appDir, display.mode = "normal", quiet = TRUE )
   Sys.unsetenv("hclwizard_Ninit")
   Sys.unsetenv("hclwizard_autohclplot")
   return(pal)
}

# tcltk GUI for selecting a color palette
choose_palette_tcltk <- function( pal = diverging_hcl, n=7L, parent = NULL, ... ) {

  # Evaluate dots args
  dots         <- list(...)
  verbose     <<- ifelse(is.null(dots$verbose),     FALSE, as.logical(dots$verbose))
  autohclplot <<- ifelse(is.null(dots$autohclplot), FALSE, as.logical(dots$autohclplot))

  # Choose a file interactively
  ChooseFile <- function(cmd, win.title, initialfile=NULL, 
                         defaultextension=NULL) {
    if ( verbose) cat(sprintf("Calling ChooseFile\n"))

    filetypes <- "{{R Source Files} {.R}} {{All files} {*}}"
    if (cmd == "Open") {
      args <- list("tk_getOpenFile")
    } else {
      args <- list("tk_getSaveFile")
      if (defaultextension == ".txt") 
        filetypes <- "{{Text Files} {.txt}} {{All files} {*}}"
    }
    args[["title"]] <- win.title
    args[["parent"]] <- tt
    args[["initialdir"]] <- initialdir
    args[["filetypes"]] <- filetypes
    
    if (!is.null(initialfile))
      args[["initialfile"]] <- initialfile
    if (!is.null(defaultextension))
      args[["defaultextension"]] <- defaultextension
    
    f <- tcltk::tclvalue(do.call(tcltk::tcl, args))
    if (!nzchar(f)) return()
    initialdir <<- dirname(f)
    f
  }

  # Open palette from file
  OpenPaletteFromFile <- function() {
    if ( verbose) cat(sprintf("Calling OpenPaletteFromFile\n"))

    f <- ChooseFile(cmd = "Open", win.title = "Open Palette File")
    if (is.null(f)) return()
    pal      <- dget(file = f)
    pal_args <- ConvertPaletteToAttributes(pal)
    UpdateDataType()
    AssignAttributesToWidgets(pal_args)
    DrawPalette()
  }

  # Save palette to file
  SavePaletteToFile <- function() {
    if ( verbose) cat(sprintf("Calling SavePaletteToFile\n"))

    f <- ChooseFile(cmd = "Save As", win.title = "Save Palette As",
                    initialfile = "color_palette", defaultextension = ".R")
    if (is.null(f)) return()
    args <- list("type" = as.character(tcltk::tclvalue(nature.var)))
    for ( arg in vars.pal[!vars.pal %in% "type"] )
        args[[arg]] <- eval(parse(text=arg))
    args$reverse <- reverse
    pal  <- do.call(GetPalette, args)
    dput(pal, file=f)
  }

  # Save colors to file
  SaveColorsToFile <- function(type) {
    if ( verbose) cat(sprintf("Calling SaveColorsToFile\n"))

    args <- list("type" = as.character(tcltk::tclvalue(nature.var)))
    for ( arg in vars.pal[!vars.pal %in% "type"] )
        eval(parse(text=arg))
    args$reverse <- reverse
    pal <- do.call(GetPalette, args)
    
    cols <- try(hex2RGB(pal(n)), silent=TRUE)
    if (inherits(cols, "try-error")) {
      msg <- "Palette results in invaild hexadecimal colors."
      tcltk::tkmessageBox(icon="error", message=msg, title="Color Error",
                   parent=tt)
      return()
    }
    
    f <- ChooseFile(cmd="Save As", win.title="Save Colors As",
                    initialfile=paste("colors_", type, sep=""),
                    defaultextension=".txt")
    if (is.null(f)) return()
    
    if (type == "HEX") {
      writehex(cols, file=f)
    } else {
      if (type == "sRGB") {
        cols <- as(cols, "sRGB")@coords
      } else if (type == "HSV") {
        cols <- as(cols, "HSV")@coords
      } else if (type == "HCL") {
        cols <- as(cols, "polarLUV")@coords
      } else if (type == "CMYK") {
        cols <- as(cols, "RGB")@coords
        red   <- cols[, "R"]
        green <- cols[, "G"]
        blue  <- cols[, "B"]
        black <- sapply(1:n, function(i) min(c(1 - red[i],
                                               1 - green[i],
                                               1 - blue[i])))
        cyan <- (1 - red - black) / (1 - black)
        magenta <- (1 - green - black) / (1 - black)
        yellow <- (1 - blue - black) / (1 - black)
        cols <- as.matrix(as.data.frame(list(C=cyan, M=black,
                                             Y=yellow, K=black)))
      }
      utils::write.table(cols, file=f, quote=FALSE, row.names=FALSE, sep="\t")
    }
  }

  # Save palette and quit
  SavePalette <- function() {
    if ( verbose) cat(sprintf("Calling SavePalette\n"))

    args <- list("type" = as.character(tcltk::tclvalue(nature.var)))
    for ( arg in vars.pal[!vars.pal %in% "type"] )
        args[[arg]] <- eval(parse(text=arg))
    args$reverse <- reverse
    pal.rtn <<- do.call(GetPalette, args)
    tcltk::tclvalue(tt.done.var) <- 1
  }

  # Scale change
  ScaleChange <- function(x, v, x.ent.var) {
    if ( verbose) cat(sprintf("Calling ScaleChange\n"))

    if (x == get(v)) return()
    assign(v, x, inherits=TRUE)
    fmt <- ifelse(v %in% c("p1", "p2"), "%.1f", "%.0f")
    tcltk::tclvalue(x.ent.var) <- sprintf(fmt, x)
    DrawPalette(v == "n")
  }

  # Entry change
  EntryChange <- function(v, x.lim, x.ent.var, x.scl.var) {
    if ( verbose) cat(sprintf("Calling EntryChange\n"))

    x <- suppressWarnings(as.numeric(tcltk::tclvalue(x.ent.var)))
    if (is.na(x))
      return()
    if (x < x.lim[1]) {
      tcltk::tclvalue(x.ent.var) <- x.lim[1]
      x <- x.lim[1]
    } else if (x > x.lim[2]) {
      tcltk::tclvalue(x.ent.var) <- x.lim[2]
      x <- x.lim[2]
    }
    assign(v, x, inherits=TRUE)
    tcltk::tclvalue(x.scl.var) <- x
    DrawPalette(v == "n")
  }

  # Helper function to create the hex palettes.
  # Generates "n" colors from palette "pal" and manipulates them
  # if desaturation or CVD simulation is required. 
  get_hex_colors <- function(pal,n) {
    pal.cols <- pal(n)
    pal.cols[is.na(pal.cols)] <- "#FFFFFF"
    if (as.logical(as.integer(tcltk::tclvalue(desaturation.var))))
      pal.cols <- desaturate(pal.cols)
    if (as.logical(as.integer(tcltk::tclvalue(colorblind.var)))) {
      type <- as.character(tcltk::tclvalue(colorblind.type.var))
      pal.cols <- do.call(type,list("col"=pal.cols))
    }
    pal.cols
  }

  # Draw current palette given the slider settings. Fills the
  # canvas object placed horizontally in the lower part of the GUI.
  # is.n is TRUE if the slider for "n" is moved. Else FALSE. IF
  # FALSE the "selected default color palette" polygon ("browse")
  # will be removed.
  DrawPalette <- function(is.n = FALSE) {
    if ( verbose) cat(sprintf("Calling DrawPalette\n"))

    args <- list("type" = as.character(tcltk::tclvalue(nature.var)))
    for ( arg in vars.pal[!vars.pal %in% "type"] )
        args[[arg]] <- eval(parse(text=arg))
    args$reverse <- reverse

    if ( ! is.n ) tcltk::tcl(frame2.cvs, "delete", "browse")
    pal <- do.call(GetPalette, args)
    tcltk::tcl(frame7.cvs, "delete", "pal")

    pal.cols <- get_hex_colors(pal,n)
    dx <- (cvs.width - 1) / n
    x2 <- 1
    y1 <- 1
    y2 <- cvs.height
    for (i in pal.cols) {
      x1 <- x2
      x2 <- x1 + dx
      pts <- tcltk::.Tcl.args(c(x1, y1, x2, y1, x2, y2, x1, y2))
      tcltk::tkcreate(frame7.cvs, "polygon", pts, fill=i, tag="pal")
    }
    RegenExample(pal,n)
  }

  # Update data type
  # @param ... used when called via callback.
  # @param init logical. Default is FALSE, if set to TRUE the first default
  #     palette of the current palette config is used. Only used when initializing
  #     the GUI.
  UpdateDataType <- function(..., init = FALSE) {
    if ( verbose ) cat(sprintf("Calling UpdateDataType\n"))
    tcltk::tcl(frame2.cvs, "delete", "browse")

    # Default palettes to data.frame
    pals_to_dataframe <- function(x) {
        x <- as.data.frame(t(as.matrix(sapply(x, function(x) x), ncol = length(x))))
        names(x)  <- vars.pal
        x$reverse <- FALSE
        return(x)
    }
    type <- as.character(tcltk::tclvalue(nature.var))

    # Loading default palettes via GetPaletteConfig
    palettes <- GetPaletteConfig(gui = TRUE)
    names(palettes)  <- tolower(names(palettes))
    palettes$fixup   <- TRUE
    palettes$reverse <- FALSE

    if (type == "Basic: Qualitative") {
      default.pals <<- subset(palettes, type == "qual")

    } else if ( type == "Basic: Sequential (single-hue)" ) {
      default.pals <<- subset(palettes, type == "seqs")

    } else if ( type == "Advanced: Sequential (single-hue)" ) {
      default.pals <<- subset(palettes, type == "seqs_advanced")

    } else if (type == "Basic: Sequential (multi-hue)") {
      default.pals <<- subset(palettes, type == "seqm")

    } else if (type == "Advanced: Sequential (multi-hue)") {
      default.pals <<- subset(palettes, type == "seqm_advanced")

    } else if (type == "Basic: Diverging") {
      default.pals <<- subset(palettes, type == "dive")

    } else if (type == "Advanced: Diverging") {
      default.pals <<- subset(palettes, type == "dive_advanced")

    }

    # Default palettes. Draws the canvas placed vertically side by side
    # in the upper part of the GUI.
    tcltk::tcl(frame2.cvs, "delete", "default")
    # Offset and width of the default palettes.
    frame2.cvs.paloffset <<- 3
    frame2.cvs.palwidth  <<- 380 / (nrow(default.pals)) - 2 * frame2.cvs.paloffset 
    frame2.cvs.palwidth  <<- max(10, min(30 + 2 * frame2.cvs.paloffset, frame2.cvs.palwidth))
    x1 <- frame2.cvs.paloffset # Start with one offset
    for (i in 1:nrow(default.pals)) {
      # Create numeric palette parameter list, drop name
      args <- as.list(default.pals[i,])
      args <- args[which(! names(args) %in% c("name", "gui"))]
      args$type    <- as.character(tcltk::tclvalue(nature.var))
      args$reverse <- FALSE

      pal      <- do.call(GetPalette, args=args)
      pal.cols <- pal(5)
      pal.cols[is.na(pal.cols)] <- "#FFFFFF" # Use white for NA colors (if fixup = FALSE)
      y2 <- frame2.cvs.paloffset

      for (j in pal.cols) {
        x2  <- x1 + frame2.cvs.palwidth
        y1  <- y2
        y2  <- y1 + (70 - 2*frame2.cvs.paloffset) / length(pal.cols)
        # Plading the tk element
        pts <- tcltk::.Tcl.args(c(x1, y1, x2, y1, x2, y2, x1, y2))
        tcltk::tkcreate(frame2.cvs, "polygon", pts, fill=j, tag="default")
      }

      if ( i == 1 ) {
        y1  <- frame2.cvs.paloffset
        y2  <- y1 + 70 - 2*frame2.cvs.paloffset
        pts <- tcltk::.Tcl.args(c(x1 - 2, y1 - 2, x2 + 1, y1 - 2, x2 + 1, y2 + 1, x1 - 2, y2 + 1))
        tcltk::tkcreate(frame2.cvs, "polygon", pts, fill = "", outline = "black", tag = "browse")
      }

      # Increase x1
      x1 <- x1 + frame2.cvs.palwidth + 2 * frame2.cvs.paloffset
    }

    # Use first default palette as default
    if ( init ) AssignAttributesToWidgets(as.list(default.pals[1,]))
    DrawPalette(TRUE)
  }

  # Select default palette
  # Triggered when the user clicks one of our default palettes.
  SelectDefaultPalette <- function(x, y) {
    if ( verbose) cat(sprintf("Calling SelectDefaultPalette\n"))

    # Position within tcltk object
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (is.na(x) | is.na(y)) return()

    y1 <- frame2.cvs.paloffset; y2 <- 70 - frame2.cvs.paloffset
    if (y < y1 | y > y2)     return()

    # max.x: width of the whole selectable area
    max.x <- nrow(default.pals) * (frame2.cvs.palwidth + 2 * frame2.cvs.paloffset)
    if (x < 0 | x > max.x)   return()
    x.seq <- seq(0, max.x, by = frame2.cvs.palwidth + 2 * frame2.cvs.paloffset)

    # Getting palette arguments
    i        <- findInterval(x, x.seq, rightmost.closed=TRUE)
    pal_args <- as.list(default.pals[i,])

    # Draw black frame around the selected palette
    x1 <- x.seq[i]
    x2 <- x.seq[i + 1]
    for ( key in vars[!vars %in% c("name", "type", "gui")] ) {
      val <- as.numeric(pal_args[[key]])
      if ( is.na(val) ) val <- 0
      #cat(sprintf("- Assign %-10s", key)); print(val)
      assign(key, val, inherits=TRUE)
    }
    AssignAttributesToWidgets(pal_args)
    DrawPalette()

    # Assign new palette attributes/settings to the slider elements
    tcltk::tcl(frame2.cvs, "delete", "browse")
    pts <- tcltk::.Tcl.args(c(x1 + 1, y1 - 2, x2 - 2, y1 - 2, x2 - 2, y2 + 1, x1 + 1, y2 + 1))
    tcltk::tkcreate(frame2.cvs, "polygon", pts, fill = "", outline = "black", tag = "browse")
  }


  # Convert palette to attributes
  ConvertPaletteToAttributes <- function(pal) {
    if ( verbose) cat(sprintf("Calling ConvertPaletteToAttributes\n"))

    # If input was missing or NULL, take a default sequential
    # multi hue palette (the first in the GetPaletteConfig data.frame)
    if ( missing(pal) | is.null(pal) ) {

        tcltk::tclvalue(nature.var) <- "Basic: Sequential (multi-hue)"
        pal_args <- GetPaletteConfig(gui = TRUE)
        pal_args <- as.list(subset(pal_args, type == "seqm")[1,])
        names(pal_args) <- tolower(names(pal_args))

    # Take arguments from function input, if set
    } else if (inherits(pal, "function")) {

        arg      <- sapply(formals(pal), function(i) {if (is.call(i)) eval(i) else i})

        # List to take up the palette parameters
        pal_args <- list("n" = 7)

        # De-parsing h, c, l, power to (h1, h2), (c1, c2), (l1, l2), (p1, p2)
        # Overwrite the NA defaults (if set)
        if ( length(arg$h) > 0 ) {
            for ( i in seq_along(arg$h) )
                eval(parse(text = sprintf("pal_args$h%1$d <- arg$h[%1$dL]", i)))
        }
        # For c1/cmax/c2 we need a bit of custom code. For different palette types
        # the "c" handling is different.
        # - Qualitative: they only have a single-element "c" (easy)
        # - Sequential single hue: c can be a c(c1) or c(c1, cmax)
        # - Sequential multi hue:  c can be a c(c1, c2) or c(c1, c2, cmax)
        # - Diverging: c can be a c(c1) or c(c1, cmax)
        # Thus, diverging_hcl "advanced" and sequential multi hue "not advanced"
        # can look very similar given there default arguments, e.g.,:
        # - diverging_hcl(n = 7, h = c(0, 100), c = c(50, 70), l = c(40, 90))
        # - sequential_hcl(n = 7, h = c(0, 100), c = c(50, 70), l = c(40, 90))
        # When loading a palette we do not get the function name, only the function
        # arguments (via formals). Do be able to distinguish between these two
        # types the diverging_hcl "advanced" stores a "cmax" in addition. Thus,
        # if we have a chroma vector of length 2 PLUS a cmax which maches c[2L]
        # we assume we have a diverging "advanced" palette and have to split
        # the arguments in a different way.
        # 
        # Sequential multi hue advanced with c(c1, c2, cmax)
        if ( length(arg$c) >= 3 ) {
            pal_args$c1   <- arg$c[1L]
            pal_args$cmax <- arg$c[2L]
            pal_args$c2   <- arg$c[3L]
        # Diverging advanced with c(c1, cmax) and c[2L] == cmax
        } else if ( length(arg$c == 2 & arg$c[2L] == arg$cmax) ) {
            pal_args$c1   <- arg$c[1L]
            pal_args$cmax <- arg$c[2L]
        # Else sequential multi hue (not advanced)
        } else if ( length(arg$c) > 0 ) {
            for ( i in seq(1, length(arg$c)) )
                eval(parse(text = sprintf("pal_args$c%1$d <- arg$c[%1$dL]", i)))
        }
        if ( length(arg$l) > 0 ) {
            for ( i in seq_along(arg$l) )
                eval(parse(text = sprintf("pal_args$l%1$d <- arg$l[%1$dL]", i)))
        }
        if ( length(arg$power) > 0 ) {
            for ( i in seq_along(arg$power) )
                eval(parse(text = sprintf("pal_args$p%1$d <- arg$power[%1$dL]", i)))
        }
        if ( is.logical(arg$rev) )
            pal_args[["rev"]] <- arg$rev
        # Fixup
        if (! is.null(arg$fixup) && is.logical(arg$fixup))
            pal_args[["fixup"]] <- as.integer(arg$fixup)
        else
            pal_args[["fixup"]] <- 1

        # Overrule settings with special arguments
        for ( key in names(arg) ) {
            if ( grepl("^([clh][12]|cmax)$", key) & ! is.null(arg[[key]]) )
                if ( ! inherits(arg[[key]], "name") ) pal_args[[key]] <- arg[[key]]
        }

        # If input is rainbow_hcl
        rb.args   <- c("c", "l", "start", "end")  # args for qualitative palettes
        if ( all(sapply(rb.args, function(i) inherits(arg[[i]], c("integer", "numeric")))) ) {
          tcltk::tclvalue(nature.var) <- "Basic: Qualitative"
          pal_args$h1 <- arg$start
          pal_args$h2 <- arg$end
        }
        
        # If has no c2, l2, p1, p2, cmax -> qualitative
        arg_names <- names(pal_args)[!is.na(pal_args)]
        # Qualitative palettes:
        # - Always NA: cmax, c2, l2, p1, p2
        if ( all( ! c("cmax", "c2", "l2", "p1", "p2") %in% arg_names) ) {
            pal_type <- "Basic: Qualitative"
        # Sequential single hue
        # - Always NA: h2, cmax, c2, p2
        } else if ( all( ! c("h2", "cmax", "c2", "p2") %in% arg_names) ) {
            pal_type <- "Basic: Sequential (single-hue)"
        # Diverging
        # - Always NA: cmax, c2
        } else if ( all( ! c("cmax", "c2") %in% arg_names) ) {
            pal_type <- "Basic: Diverging"
        # Diverging, advanced
        # - Always NA: c2, but have h1 and h2
        } else if ( all(! c("c2") %in% arg_names) & all(c("h1", "h2") %in% arg_names )) {
            pal_type <- "Advanced: Diverging"
        # Sequential single hue, advanced
        # - Always NA: h2
        } else if ( all(! c("h2") %in% arg_names) ) {
            pal_type <- "Advanced: Sequential (single-hue)"
        # Sequential multi hue, advanced
        # - Always NA: cmax
        } else if ( ! "cmax" %in% arg_names ) {
            pal_type <- "Basic: Sequential (multi-hue)"
        # Else we expect it to be a sequential hulti hue, advanced
        } else {
            pal_type <- "Advanced: Sequential (multi-hue)"
        }
        tcltk::tclvalue(nature.var) <- pal_type

        # Extending the palette args with NA's
        for ( key in slider_elements )
            if ( ! key %in% names(pal_args) ) pal_args[[key]] <- NA
        for ( key in names(pal_args) )
            if ( ! key %in% c("rev", slider_elements) ) pal_args[[key]] <- NULL # Remove

    } else {
        stop("Cannot interpret input palette in choose_palette")
    }

    # Return palette settings
    return(pal_args)
  }


  # Assign attributes to widgets
  # pal_args can be a named list with the settings of the
  # current palette (e.g,. when the user selects a new default
  # palette). If an element contains NA the corresponding
  # slider will be disabled.
  AssignAttributesToWidgets <- function(pal_args) {
    if ( verbose) cat(sprintf("Calling AssignAttributesToWidgets\n"))

    # Setting rev (reverse colors) if specified
    if ( is.logical(pal_args$rev) ) {
        tcltk::tclvalue(reverse.var) <- pal_args$rev
        reverse <<- pal_args$rev
    }

    # Looping trouch slider elements
    for ( i in seq_along(slider_elements) ) { 

        # Name of the slider element
        key <- slider_elements[i]

        # Format to set the current value
        if ( key %in% c("name", "gui", "n") ) {
            next
        } else if ( grepl("^[hcl]", key) ) {
            fmt <- "%.0f"
        } else {
            fmt <- "%.1f"
        }

        # If pal_args is given: check if value is.na. If NA:
        # disable label, slider, and the text output element.
        # Else enable.
        if ( !missing(pal_args) ) {
            state <- ifelse(is.na(pal_args[[key]]), FALSE, TRUE)
            #cat(sprintf(" - Setting sliders t/f: %s  %s", state, key))
            #cat(sprintf("   %s .... %.1f\n", key, pal_args[[key]]))
            cmd   <- sprintf("tcltk::tkconfigure(frame3.lab.%d.1, state = \"%s\")",
                           i, ifelse(state, "normal", "disabled"))
            eval(parse(text = cmd))
            cmd   <- sprintf("tcltk::tkconfigure(frame3.ent.%d.3, state = \"%s\")",
                           i, ifelse(state, "normal", "disabled"))
            eval(parse(text = cmd))
            cmd   <- sprintf("tcltk::tcl(frame3.scl.%d.2, \"state\", \"%s\")",
                           i, ifelse(state, "!disabled", "disabled"))
            eval(parse(text = cmd))
        }

        # If attribute is NA: set to 0
        #val <- ifelse(is.na(pal_args[[key]]), 0, pal_args[[key]])
        val <- pal_args[[key]]

        # Set current value (slider and text output)
        if ( is.na(val) ) {
            cmd <- sprintf("tcltk::tclvalue(%1$s.ent.var) <- \"NA\"", key)
        } else {
            cmd <- sprintf("tcltk::tclvalue(%1$s.ent.var) <- sprintf(\"%2$s\", %3$.1f)", key, fmt, val)
        }
        eval(parse(text = cmd))
        cmd <- sprintf("tcltk::tclvalue(%1$s.scl.var) <- %2$.1f", key, val)
        eval(parse(text = cmd))
        assign(key, val, inherits=TRUE)
    }

  }

  # Show example plot
  ShowExample <- function() {
    if (!dev.example %in% dev.list()) {
      dev.new(width=7L, height=7L)
      dev.example <<- dev.cur()
    }
    ExampleSetPar()
    DrawPalette(is.n = TRUE)
  }

  ExampleSetPar <- function() {
    if ( ! as.logical(as.numeric(tcltk::tclvalue(darkmode.var))) ) {
        par(oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0))
        par(bg = "white", fg = "black", col.axis = "black")
    } else {
        par(mar = rep(1, 4))
        par(bg = "black", fg = "white", col.axis = "white")
    }
  }

  ActivateDarkmode <- function() {
    if (dev.example %in% dev.list()) dev.set(which = dev.example)
    else                             return()
    # If dark mode has been set off
    # Draw palette, also regenerates example if open
    ExampleSetPar()
    DrawPalette()
  }

  # Regenerate example plot
  RegenExample <- function(pal,n) {
    if (dev.example %in% dev.list()) dev.set(which=dev.example)
    else                             return()
    plot_example <- eval(parse(text=sprintf("plot_%s",
                         gsub(" ", "", tolower(tcltk::tclvalue(example.var))))))
    # Spectrum plot: take 100 values (hardcoded)
    if ( tcltk::tclvalue(example.var) == "Spectrum" ) n <- 100
    pal.cols <- get_hex_colors(pal,n)

    if ( grepl("^HCL\\sPlot$", as.character(tcltk::tclvalue(example.var))) & !autohclplot ) {
        type <- as.character(tcltk::tclvalue(nature.var))
        if      ( grepl("[Dd]iverging", type) )   { type <- "diverging" }
        else if ( grepl("[Ss]equential", type) )  { type <- "sequential" }
        else if ( grepl("[Qq]ualitative", type) ) { type <- "qualitative" }
        else                                      { type <- NULL }
    } else { type = NULL }
    plot_example(pal.cols, type = type)
  }


  # ----------------------------------------------------------------
  # Main program
  # ----------------------------------------------------------------

  # Initialize directory
  initialdir   <- getwd()

  # Default
  reverse      <- FALSE
  
  # Initialize return palette
  pal.rtn      <- NULL

  # Initialize default palettes
  default.pals <- NULL
  
  # Initialize data for scatter plot example
  xyhclust     <- NULL
  
  # Initialize data for mosaic plot example
  msc.matrix   <- NULL
  
  # Flag graphics device
  dev.example  <- 1

  # Set default and initial palettes
  for ( key in vars.pal[!vars.pal %in% "type"] )
      eval(parse(text = sprintf("%s <- 0", key)))
  fixup <- 1

  # Load/Define palettes
  vars      <- vars.pal
  qual.pals <- qual.pals
  seqs.pals <- seqs.pals
  seqm.pals <- seqm.pals
  dive.pals <- dive.pals

  # Set limits for palette attributes
  h1.lim    <- c(-360, 360)
  h2.lim    <- c(-360, 360)
  c1.lim    <- c(   0, 100)
  cmax.lim  <- c(   0, 180)
  c2.lim    <- c(   0, 100)
  l1.lim    <- c(   0, 100)
  l2.lim    <- c(   0, 100)
  p1.lim    <- c(   0,   3)
  p2.lim    <- c(   0,   3)
  n.lim     <- c(   1,  50)

  # Set dimensions on palette canvas
  cvs.width  <- 328 # 30 * 10 + 10 + 18
  cvs.height <- 25

  # Assign additional variables linked to Tk widgets
  example.var <- tcltk::tclVar()
  nature.var  <- tcltk::tclVar()

  n.scl.var <- tcltk::tclVar(n)
  n.ent.var <- tcltk::tclVar(n)

  # Setting up tcltk variables and scale elements.
  for ( key in vars.pal[!vars.pal %in% "type"] ) {
      eval(parse(text = sprintf("%s.scl.var <- tcltk::tclVar()", key)))
      eval(parse(text = sprintf("%s.ent.var <- tcltk::tclVar()", key)))
  }
  
  # Adding tcltk variables for the color control elements
  # (checkboxes, radio buttons) with defaults.
  fixup.var           <- tcltk::tclVar(fixup)
  reverse.var         <- tcltk::tclVar(FALSE)
  desaturation.var    <- tcltk::tclVar(FALSE)
  darkmode.var       <- tcltk::tclVar(FALSE)
  colorblind.var      <- tcltk::tclVar(FALSE)
  colorblind.type.var <- tcltk::tclVar("deutan")

  tt.done.var         <- tcltk::tclVar(0)

  # Open GUI
  tcltk::tclServiceMode(FALSE)

  tt <- tcltk::tktoplevel()
  if (!is.null(parent)) {
    tcltk::tkwm.transient(tt, parent)
    geo <- unlist(strsplit(as.character(tcltk::tkwm.geometry(parent)), "\\+"))
    tcltk::tkwm.geometry(tt, paste("+", as.integer(geo[2]) + 25,
                            "+", as.integer(geo[3]) + 25, sep=""))
  }
  tcltk::tkwm.resizable(tt, 0, 0)
  tcltk::tkwm.geometry(tt, "425x745+0+10") 
  tcltk::tktitle(tt) <- "Choose Color Palette"

  # Top file menu
  top.menu <- tcltk::tkmenu(tt, tearoff=0)
  menu.file <- tcltk::tkmenu(tt, tearoff=0)
  tcltk::tkadd(top.menu, "cascade", label="File", menu=menu.file, underline=0)

  tcltk::tkadd(menu.file, "command", label="Open palette", accelerator="Ctrl+O",
               command=OpenPaletteFromFile)
  tcltk::tkadd(menu.file, "command", label="Save palette as",
               accelerator="Shift+Ctrl+S", command=SavePaletteToFile)

  menu.file.colors <- tcltk::tkmenu(tt, tearoff=0)
  tcltk::tkadd(menu.file.colors, "command", label="HEX",
               command=function() SaveColorsToFile("HEX"))
  tcltk::tkadd(menu.file.colors, "command", label="sRGB",
               command=function()  SaveColorsToFile("sRGB"))
  tcltk::tkadd(menu.file.colors, "command", label="HSV",
               command=function()  SaveColorsToFile("HSV"))
  tcltk::tkadd(menu.file.colors, "command", label="HCL",
               command=function()  SaveColorsToFile("HCL"))
  tcltk::tkadd(menu.file.colors, "command", label="CMYK",
               command=function()  SaveColorsToFile("CMYK"))
  tcltk::tkadd(menu.file, "cascade", label="Save colors as", menu=menu.file.colors)

  tcltk::tkconfigure(tt, menu=top.menu)

  # Frame 0, ok and cancel buttons
  frame0 <- tcltk::ttkframe(tt, relief="flat")
  frame0.but.3 <- tcltk::ttkbutton(frame0, width=12, text="OK", command=SavePalette)
  frame0.but.4 <- tcltk::ttkbutton(frame0, width=12, text="Cancel",
                            command=function() {
                              pal.rtn <<- NULL
                              tcltk::tclvalue(tt.done.var) <- 1
                            })
  tcltk::tkgrid("x", frame0.but.3, frame0.but.4, pady=c(10, 10))
  tcltk::tkgrid.configure(frame0.but.3, sticky="e")
  tcltk::tkgrid.configure(frame0.but.4, sticky="w", padx=c(4, 10))
  tcltk::tkgrid.columnconfigure(frame0, 0, weight=1)

  tcltk::tkpack(frame0, fill="x", side="bottom", anchor="e")

  # Frame 1, choose nature of data
  frame1 <- tcltk::ttkframe(tt, relief = "flat")
  frame1.lab.1 <- tcltk::ttklabel(frame1, text = "Type of palette: ")
  frame1.box.2 <- tcltk::ttkcombobox(frame1, state = "readonly", textvariable = nature.var,
                   values=c("Basic: Qualitative",
                            "Basic: Sequential (single-hue)",
                            "Basic: Sequential (multi-hue)",
                            "Basic: Diverging",
                            "Advanced: Sequential (single-hue)",
                            "Advanced: Sequential (multi-hue)",
                            "Advanced: Diverging"))

  tcltk::tkgrid(frame1.lab.1, frame1.box.2, pady=c(10, 0))
  tcltk::tkgrid.configure(frame1.lab.1, padx=c(10, 2))
  tcltk::tkgrid.configure(frame1.box.2, padx=c(0, 10), sticky="we")

  tcltk::tkgrid.columnconfigure(frame1, 1, weight=1)

  tcltk::tkpack(frame1, fill="x")

  # Frame 2, default color schemes
  frame2 <- tcltk::ttklabelframe(tt, relief = "flat", borderwidth = 5, padding = 5,
                          text = "Default color schemes")
  frame2.cvs <- tcltk::tkcanvas(frame2, relief="flat", width = 310, height = 70,
                         background = "white", confine = TRUE, closeenough = 0,
                         borderwidth = 0, highlightthickness = 0)
  tcltk::tkgrid(frame2.cvs, sticky="we")
  tcltk::tkgrid.columnconfigure(frame2, 0, weight=1)
  tcltk::tkpack(frame2, fill="x", padx=10, pady=10)

  # Frame 3, color description
  txt <- "Palette description: Hue, Chroma, Luminance, Power"
  frame3 <- tcltk::ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text=txt)

  # Appending labels, tcltk grid and add variables.
  # This vector will be used in several places to set values
  # and/or enable/disable the elements.
  slider_elements <- c("h1", "h2", "c1", "cmax", "c2", "l1", "l2", "p1", "p2", "n")

  # Setting up the GUI elements (combination of label, slider, and text output)
  for ( i in seq_along(slider_elements) ) {
    # Name of the current slider element
    key = slider_elements[i]

    # Command to create the slider label
    cmd_label <- sprintf("frame3.lab.%1$d.1 <- tcltk::ttklabel(frame3, text=\"%2$s\", width=5)",
                         i, toupper(key))

    # Command to create the slider
    # For p1/p2: numeric(...), for all others: round(numeric(...))
    if ( grepl("^p", key) ) {
        scl_fun <- "as.numeric(...)"
    } else {
        scl_fun <- "round(as.numeric(...))"
    }
    cmd_slider <- sprintf(paste("frame3.scl.%1$d.2 <- tcltk::tkwidget(frame3,",
                                "\"ttk::scale\", from = %3$s.lim[1L], to = %3$s.lim[2L],",
                                "orient = \"horizontal\", value = %3$s, variable = %3$s.scl.var,",
                                "command = function(...) {",
                                "   ScaleChange(x = %2$s, v=\"%3$s\", x.ent.var = %3$s.ent.var)",
                                "})"), i, scl_fun, key)

    # Command to create the text output
    cmd_text <- sprintf(paste("frame3.ent.%1$d.3 <- tcltk::ttkentry(frame3,",
                              "textvariable=%2$s.ent.var, width = 4)"), i, key)

    # Execute the commands
    eval(parse(text = cmd_label))
    eval(parse(text = cmd_slider))
    eval(parse(text = cmd_text))


    # Place all sliders (except n) on frame3
    if ( ! key == "n" ) {
        cmd <- sprintf(paste("tcltk::tkgrid(frame3.lab.%1$d.1, frame3.scl.%1$d.2,",
                             "frame3.ent.%1$d.3, pady=c(0, 5))"), i)
        eval(parse(text = cmd))
    }
  }

  tcltk::tkgrid.configure(frame3.scl.1.2, frame3.scl.2.2, frame3.scl.3.2,
                          frame3.scl.4.2, frame3.scl.5.2, frame3.scl.6.2,
                          frame3.scl.7.2, frame3.scl.8.2, frame3.scl.9.2,
                          sticky="we", padx=c(4, 10))

  tcltk::tkgrid.columnconfigure(frame3, 1, weight=1)

  tcltk::tkpack(frame3, fill="x", padx=10, pady=0)
  
  # Frame 4, color palette fixup
  frame4 <- tcltk::ttkframe(tt, relief="flat")
  txt <- "Correct all colors to valid RGB color model values"
  frame4.chk.1 <- tcltk::ttkcheckbutton(frame4, text=txt, variable=fixup.var,
                                 command=function() {
                                   fixup <<- as.integer(tcltk::tclvalue(fixup.var))
                                   DrawPalette(is.n = TRUE)
                                 })
  tcltk::tkgrid.configure(frame4.chk.1, padx=c(12, 0), pady=c(2, 0))
  tcltk::tkpack(frame4, fill="x")

  # Frame 5, number of colors in palette
  txt <- "Number of colors in palette"
  frame5 <- tcltk::ttklabelframe(tt, relief="flat", borderwidth=5, padding=5, text=txt)

  frame5.lab.1 <- tcltk::ttklabel(frame5, text="n", width=2)
  frame5.ent.3 <- tcltk::ttkentry(frame5, textvariable=n.ent.var, width=4)
  frame5.scl.2 <- tcltk::tkwidget(frame5, "ttk::scale", from=n.lim[1], to=n.lim[2],
                           orient="horizontal", value=n, variable=n.scl.var,
                           command=function(...) {
                             ScaleChange(x=round(as.numeric(...)), v="n",
                                         x.ent.var=n.ent.var)
                           })

  tcltk::tkgrid(frame5.lab.1, frame5.scl.2, frame5.ent.3)
  tcltk::tkgrid.configure(frame5.scl.2, sticky = "we", padx = c(4, 10))
  tcltk::tkgrid.columnconfigure(frame5, 1, weight = 1)

  tcltk::tkpack(frame5, fill = "x", padx = 10, pady = 10)

  # Frame 6, example plots and reverse colors
  frame6 <- tcltk::ttklabelframe(tt, relief = "flat", borderwidth = 5, padding = 5, 
                          text = "Show example")
  frame6.lab.1 <- tcltk::ttklabel(frame6, text="Plot type")
  frame6.box.2 <- tcltk::ttkcombobox(frame6, state="readonly", 
                              textvariable = example.var,
                              values = example.plots)
  frame6.chk.3 <- tcltk::ttkcheckbutton(frame6, text = "Reverse colors", 
                                 variable = reverse.var,
                                 command = function() {
                                     reverse <<- as.logical(as.integer(tcltk::tclvalue(reverse.var)))
                                     DrawPalette(is.n = TRUE)
                                 })
  tcltk::tkgrid(frame6.lab.1, frame6.box.2, frame6.chk.3)
  tcltk::tkgrid.configure(frame6.box.2, padx = c(2, 10), sticky = "we")
  tcltk::tkgrid.columnconfigure(frame6, 1, weight = 1)
  tcltk::tkpack(frame6, fill = "x", padx = 10, pady = 0)
  
  # Frame 7, color palette and robustness checks
  frame7 <- tcltk::ttkframe(tt, relief="flat")
  frame7.cvs   <- tcltk::tkcanvas(frame7, relief="flat",
                                 width=cvs.width + 1, height=cvs.height + 1,
                                 background = "black", confine = TRUE, closeenough = 0,
                                 borderwidth = 0, highlightthickness = 0)
  tcltk::tkgrid(frame7.cvs, padx=10, pady=c(12,10))

  frame7.chk.1 <- tcltk::ttkcheckbutton(frame7, text="Desaturation",
                                 variable=desaturation.var,
                                 command=function() DrawPalette(is.n = TRUE))
  frame7.chk.2 <- tcltk::ttkcheckbutton(frame7, text="Dark mode",
                                 variable=darkmode.var,
                                 command=function() ActivateDarkmode())

  tcltk::tkgrid(frame7.chk.1, frame7.chk.2, "x",
                pady = c(2, 0), sticky = "w")    
  tcltk::tkgrid.configure(frame7.chk.2, padx = c(7, 0))
  tcltk::tkgrid.configure(frame7.cvs, columnspan=5)
  tcltk::tkgrid.columnconfigure(frame7, 4, weight = 1)
  tcltk::tkgrid.configure(frame7.chk.1, padx=c(10, 0))
  tcltk::tkpack(frame7, fill="x")


  # Frame 8, cvd options
  frame8 <- tcltk::ttkframe(tt, relief="flat")
  frame8.chk.1 <- tcltk::ttkcheckbutton(frame8, text = "Color blindness:",
                                 variable = colorblind.var,
                                 command = function() DrawPalette(is.n = TRUE))
  frame8.rb.2  <- tcltk::ttkradiobutton(frame8, variable = colorblind.type.var,
                                 value = "deutan", text = "deutan",
                                 command = function() DrawPalette(is.n = TRUE))
  frame8.rb.3  <- tcltk::ttkradiobutton(frame8, variable = colorblind.type.var,
                                 value = "protan", text = "protan",
                                 command = function() DrawPalette(is.n = TRUE))
  frame8.rb.4  <- tcltk::ttkradiobutton(frame8, variable = colorblind.type.var,
                                 value = "tritan", text = "tritan",
                                 command = function() DrawPalette(is.n = TRUE))

  tcltk::tkgrid(frame8.chk.1, frame8.rb.2, frame8.rb.3, frame8.rb.4,
                pady = c(2, 0), sticky = "w")    
  tcltk::tkgrid.configure(frame8.rb.2, padx = c(7, 0))
  tcltk::tkgrid.columnconfigure(frame8, 4, weight = 1)
  tcltk::tkgrid.configure(frame8.chk.1, padx=c(10, 0))
  tcltk::tkpack(frame8, fill="x")

  # Initial commands
  pal_args <- ConvertPaletteToAttributes(pal)
  # Assign attributes to widget, also enables/disables the sliders
  AssignAttributesToWidgets(pal_args)
  UpdateDataType(init = TRUE)

  # Bind events
  tcltk::tclServiceMode(TRUE)

  tcltk::tkbind(tt, "<Control-o>", OpenPaletteFromFile)
  tcltk::tkbind(tt, "<Shift-Control-S>", SavePaletteToFile)
  
  UpdateDataTypeInit <- function() UpdateDataType(init = TRUE)
  tcltk::tkbind(frame1.box.2, "<<ComboboxSelected>>", UpdateDataTypeInit)
  tcltk::tkbind(frame6.box.2, "<<ComboboxSelected>>", ShowExample)

  tcltk::tkbind(frame2.cvs, "<ButtonPress>", function(x, y) SelectDefaultPalette(x, y))

  tcltk::tkbind(frame3.ent.1.3, "<KeyRelease>",
         function() EntryChange("h1", h1.lim, h1.ent.var, h1.scl.var))
  tcltk::tkbind(frame3.ent.2.3, "<KeyRelease>",
         function() EntryChange("h2", h2.lim, h2.ent.var, h2.scl.var))
  tcltk::tkbind(frame3.ent.3.3, "<KeyRelease>",
         function() EntryChange("c1", c1.lim, c1.ent.var, c1.scl.var))
  tcltk::tkbind(frame3.ent.4.3, "<KeyRelease>",
         function() EntryChange("cmax", cmax.lim, cmax.ent.var, cmax.scl.var))
  tcltk::tkbind(frame3.ent.5.3, "<KeyRelease>",
         function() EntryChange("c2", c2.lim, c2.ent.var, c2.scl.var))
  tcltk::tkbind(frame3.ent.6.3, "<KeyRelease>",
         function() EntryChange("l1", l1.lim, l1.ent.var, l1.scl.var))
  tcltk::tkbind(frame3.ent.7.3, "<KeyRelease>",
         function() EntryChange("l2", l2.lim, l2.ent.var, l2.scl.var))
  tcltk::tkbind(frame3.ent.8.3, "<KeyRelease>",
         function() EntryChange("p1", p1.lim, p1.ent.var, p1.scl.var))
  tcltk::tkbind(frame3.ent.9.3, "<KeyRelease>",
         function() EntryChange("p2", p2.lim, p2.ent.var, p2.scl.var))

  tcltk::tkbind(frame5.ent.3, "<KeyRelease>",
         function() EntryChange("n", n.lim, n.ent.var, n.scl.var))

  tcltk::tkbind(tt, "<Destroy>", function() tcltk::tclvalue(tt.done.var) <- 1)

  # GUI control
  tcltk::tkfocus(tt)
  tcltk::tkgrab(tt)

  tcltk::tkwait.variable(tt.done.var)

  tcltk::tclServiceMode(FALSE)
  tcltk::tkgrab.release(tt)
  tcltk::tkdestroy(tt)
  tcltk::tclServiceMode(TRUE)

  if (dev.example %in% dev.list()) dev.off(which = dev.example)

  invisible(pal.rtn)
}

# Get color palette as function of n
GetPalette <- function(...) { #type, h1, h2, c1, c2, l1, l2, p1, p2, fixup, reverse, cmax) {

   attach(list(...), warn.conflicts = FALSE)

   fixup <- as.logical(fixup)
   #type <- as.character(tcltk::tclvalue(nature.var))
   if (grepl("^(qual|.*[Qq]ualitative)", type)) {
      f <- qualitative_hcl
      formals(f) <- eval(substitute(alist(n=, h=hh, c=d1, l=d2, start=d3, end=d4,
                                          fixup=d5, gamma=NULL, alpha=1,
                                          palette=NULL, rev=d6, ...=,
                                          h1=, h2=, c1=, l1=, cmax=),
                                    list(hh = c(h1, h2), #0,360), 
                                         d1=c1, d2=l1, d3=h1, d4=h2, d5=fixup, d6=reverse)))
   #} else if (type %in% c("seqs","Sequential (single hue)")) {
   } else if (grepl("^(seqs|.*[Ss]equential.*single)", type)) {
      f <- sequential_hcl
      formals(f) <- eval(substitute(alist(n=, h=d1, c=d2, l=d3, power=d4,
                                          gamma=NULL, fixup=d5, alpha=1,
                                          palette=NULL, rev=d6, ...=,
                                          h1=, h2=, c1=, c2=, l1=, l2=, p1=, p2=, cmax=, c.=),
                                    list(d1=h1, d2=c(c1,cmax,c2), d3=c(l1, l2),
                                         d4=p1, d5=fixup, d6=reverse)))
   #} else if (type %in% c("seqm","Sequential (multiple hues)")) {
   } else if (grepl("^(seqm|.*[Ss]equential.*multi)", type)) {
      f <- sequential_hcl
      formals(f) <- eval(substitute(alist(n=, h=d1, c=d2, l=d3, power=d4,
                                          gamma=NULL, fixup=d5, alpha=1,
                                          palette=NULL, rev=d6, ...=,
                                          h1=, h2=, c1=, c2=, l1=, l2=, p1=, p2=, cmax=, c.=),
                                    list(d1=c(h1, h2), d2=c(c1, cmax, c2),
                                          d3=c(l1, l2), d4=c(p1, p2), d5=fixup, d6=reverse)))
   #} else if (type %in% c("dive","Diverging")) {
   } else if (grepl("^(dive|.*[Dd]iverging)", type)) {
      f <- diverging_hcl
      arg_names <- names(list(...))[!is.na(list(...))]
      if ( all(c("p1", "p2")   %in% arg_names) ) power <- c(p1, p2) else power <- p1
      if ( all(c("c1", "cmax") %in% arg_names) ) chroma <- c(c1, cmax) else chroma <- c1
      formals(f) <- eval(substitute(alist(n=, h=d1, c=d2, l=d3, power=d4,
                                          gamma=NULL, fixup=d5, alpha=1,
                                          palette=NULL, rev=d6, ...=,
                                          h1=, h2=, c1=, l1=, l2=, p1=, p2=, cmax=d7),
                                    list(d1=c(h1, h2), d2=chroma, d3=c(l1, l2),
                                         d4=power, d5=fixup, d6=reverse, d7=cmax)))
   }
   f
}

