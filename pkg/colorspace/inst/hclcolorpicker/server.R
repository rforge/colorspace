#' Shiny app to pick colors in HCL space
#' 
#' The app visualizes colors either along the hue-chroma plane for a given luminance value or along the
#' luminance-chroma plane for a given hue. Colors can be entered by specifying the hue (H), chroma (C),
#' and luminance (L) values via sliders, by entering an RGB hex code, or by clicking on a color in the
#' hue-chroma or luminance-chroma plane. It is also possible to select individual colors and add them
#' to a palette for comparison and future reference. 
#'
#' @return \code{hcl_color_picker} invisibly returns a vector of colors choosen.
#'    If no colors have been selected \code{NULL} will be returned.
#' @examples
#' \dontrun{
#' hcl_color_picker()
#' }
#' @export
#' @importFrom methods as

library("shiny")

shiny::shinyServer(function(input, output, session) {

    picked_color_list <- shiny::reactiveValues(cl=c())
    shiny::observeEvent({input$HC_plot_click}, {
      # store the old colors
      coords_old_LUV <- coords(as(polarLUV(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H)), "LUV"))
      U <- input$HC_plot_click$x
      if (is.null(U)) U <- coords_old_LUV[2L]
      V <- input$HC_plot_click$y
      if (is.null(V)) V <- coords_old_LUV[3L]
      L <- input$L
      coords_HCL <- coords(as(LUV(L, U, V), "polarLUV"))
      shiny::updateSliderInput(session, "C", value = round(coords_HCL[2L]))
      shiny::updateSliderInput(session, "H", value = round(coords_HCL[3L]))
    })

    shiny::observeEvent({input$LC_plot_click}, {
      # store the old colors
      Lold <- as.numeric(input$L)
      Cold <- as.numeric(input$C)
      C <- input$LC_plot_click$x
      if (is.null(C)) C <- Cold
      L <- input$LC_plot_click$y
      if (is.null(L)) L <- Lold
      shiny::updateSliderInput(session, "C", value = round(C))
      shiny::updateSliderInput(session, "L", value = round(L))
    })

    shiny::observeEvent({input$palette_click}, {
      x <- input$palette_click$x
      if (is.null(x)) return()
      i <- ceiling(x*length(picked_color_list$cl))
      col_RGB <- hex2RGB(picked_color_list$cl[i])
      coords_HCL <- coords(as(col_RGB, "polarLUV"))
      shiny::updateSliderInput(session, "L", value = round(coords_HCL[1L]))
      shiny::updateSliderInput(session, "C", value = round(coords_HCL[2L]))
      shiny::updateSliderInput(session, "H", value = round(coords_HCL[3L]))
    })

    shiny::observeEvent({input$Hgrad_click}, {
      H <- input$Hgrad_click$x
      if (!is.null(H)) {
        shiny::updateSliderInput(session, "H", value = round(H))
      }
    })

    shiny::observeEvent({input$Lgrad_click}, {
      L <- input$Lgrad_click$x
      if (!is.null(L)) {
        shiny::updateSliderInput(session, "L", value = round(L))
      }
    })

    shiny::observeEvent({input$Cgrad_click}, {
      C <- input$Cgrad_click$x
      if (!is.null(C)) {
        shiny::updateSliderInput(session, "C", value = round(C))
      }
    })

    shiny::observeEvent({input$set_hexcolor}, {
      # only execute this on complete color hex codes
      if (grepl("^#[0123456789ABCDEFabcdef]{6}$", input$hexcolor)) {
          col_RGB <- hex2RGB(input$hexcolor)
          coords_HCL <- coords(as(col_RGB, "polarLUV"))
          shiny::updateSliderInput(session, "L", value = round(coords_HCL[1L]))
          shiny::updateSliderInput(session, "C", value = round(coords_HCL[2L]))
          shiny::updateSliderInput(session, "H", value = round(coords_HCL[3L]))
      }
    })


    # save color code
    shiny::observeEvent(input$color_picker, {
      # cannot rely on hex color in text-input field, so recalculate from set H, C, L values
      hexcolor <- hex(polarLUV(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H)))

      # only add color if it's not already in the list
      if ( ! is.na(hexcolor) && ! hexcolor %in% picked_color_list$cl) {
        picked_color_list$cl <- c(picked_color_list$cl, hexcolor)
      }
    })

    # undo pick color
    shiny::observeEvent(input$color_unpicker, {
      if (input$hexcolor %in% picked_color_list$cl){
        picked_color_list$cl <- picked_color_list$cl[picked_color_list$cl != input$hexcolor]
      }else{
        # It's a better user interface to leave the list alone if the color is not in the list
        # picked_color_list$cl <- head(picked_color_list$cl,-1)
      }
    })

    # clear saved color code
    shiny::observeEvent(input$clear_color_picker, {
      picked_color_list$cl <- c()
    })

    shiny::observe({
      shiny::updateTextInput(session, "hexcolor", value = hex(polarLUV(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))))
    })

    output$colorbox <- shiny::renderUI({
      shiny::tags$div(style=paste0("width: 100%; height: 40px; border: 1px solid rgba(0, 0, 0, .2); background: ",
                     hex(polarLUV(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))), ";"))
    })

    # generate HC plot with given inputs
    output$HC_plot <- shiny::renderPlot({
      color_picker_hue_chroma_plot(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })

    # generate LC plot with given inputs
    output$LC_plot <- shiny::renderPlot({
      color_picker_luminance_chroma_plot(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })


    output$Hgrad <- shiny::renderPlot({
      color_picker_H_gradient(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })

    output$Hgrad2 <- shiny::renderPlot({
      color_picker_H_gradient(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })

    output$Cgrad <- shiny::renderPlot({
      color_picker_C_gradient(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })

    output$Cgrad2 <- shiny::renderPlot({
      color_picker_C_gradient(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })


    output$Lgrad <- shiny::renderPlot({
      color_picker_L_gradient(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })

    output$Lgrad2 <- shiny::renderPlot({
      color_picker_L_gradient(as.numeric(input$L), as.numeric(input$C), as.numeric(input$H))
    })

    # generate palette plot with given hex code
    output$palette_plot <- shiny::renderPlot({
      if (length(picked_color_list$cl) != 0){
        pal_plot(picked_color_list$cl)
      }
    })

    # add R color code line
    output$palette_line_R <- shiny::renderText({
      if (length(picked_color_list$cl) != 0){
        color_list <- picked_color_list$cl
        color_list <- paste(color_list, collapse = "', '")
        color_string <- paste("colors <- c('", color_list, "')", sep = '')
        sprintf( "<b style=\"display:block;margin-top:5px\">R style color vector</b>%s",color_string)
      }else{
        "Currently no colors picked. Define a color and press <b>pick</b> to add the color to your selection."
      }
    })
    # Add matlab style output
    output$palette_line_matlab <- shiny::renderText({
      if (length(picked_color_list$cl) != 0){
        # Convert colors to RGB for matlab
        color_string <- sprintf( "colors = [%s]",
               paste0(base::apply(colorspace::hex2RGB(picked_color_list$cl)@coords,1, function(x)
               sprintf("%.3f,%.3f,%.3f",x[1L],x[2L],x[3L])),collapse="; ") )
        sprintf( "<b style=\"display:block;margin-top:5px\">matlab style color vector</b>%s",color_string)
      }
    })

    # Return data to R-console
    shiny::observeEvent(input$closeapp,
        shiny::stopApp(invisible( picked_color_list$cl ))
    );
})


color_picker_hue_chroma_plot <- function(L = 75, C = 20, H = 0, n = 200) {
  Cmax <- max(max_chroma(0:360, L))
  Vmax <- Cmax
  Umax <- Cmax
  U <- seq(-Umax, Umax, length.out = n)
  V <- seq(Vmax, -Vmax, length.out = n)
  grid <- expand.grid(U = U, V = V)
  image <- matrix(hex(LUV(L, grid$U, grid$V)), nrow = n, byrow = TRUE)
  grob <- grid::rasterGrob(image)

  sel_col <- polarLUV(L, C, H) # selected color in polar LUV
  sel_pt <- coords(as(sel_col, "LUV")) # coordinates of selected point in LUV
  df_sel <- data.frame(U = sel_pt[2L], V = sel_pt[3L])

  #gg# ggplot2::ggplot(df_sel, ggplot2::aes(U, V)) + ggplot2::annotation_custom(grob) +
  #gg#   ggplot2::geom_point(size = 5, color = cursor_color(L), fill = hex(sel_col), shape = 21) +
  #gg#   ggplot2::annotate("path",
  #gg#                     x=C*cos(seq(0, 2*pi, length.out=100)),
  #gg#                     y=C*sin(seq(0, 2*pi, length.out=100)), color = cursor_color(L), size = 0.2, alpha = 0.5) +
  #gg#   ggplot2::coord_fixed(xlim = c(-Umax, Umax), ylim = c(-Vmax, Vmax), expand = FALSE) +
  #gg#   ggplot2::theme_minimal()

   # Non-gg version
   grid$hex <- as.vector( t(image) )
   limits   <- lapply( na.omit(grid), function(x)
      if ( ! is.numeric(x) ) { return(NULL) } else { max(abs(x))*c(-1,1) } )
   par( mar = c(3,3,1,1) )
   with( grid, graphics::plot( V ~ U, type="n", bty = "n", axes = FALSE,
               xaxs = "i", yaxs = "i", asp = 1, xlim=limits$U, ylim=limits$V ) )
   graphics::abline( h = seq(-200,200,by=25), v = seq(-200,200,by=25), col = "gray80" )
   graphics::axis( side = 1, at = seq(-200,200,by=50), col = NA, col.ticks = 1 )
   graphics::axis( side = 2, at = seq(-200,200,by=50), col = NA, col.ticks = 1 )
   graphics::points( grid$V ~ grid$U, col = grid$hex, pch = 19 )
   # Selected color
   points( sel_pt[1,"V"] ~ sel_pt[1,"U"], cex = 2 )
   sel_radius <- sqrt(sum(sel_pt[1,c("U","V")]^2))
   graphics::lines( sin(seq(0,2*pi,length=300))*sel_radius, cos(seq(0,2*pi,length=300))*sel_radius, col = "gray40" )
   # Box
   graphics::box( col = "gray40" )


}

color_picker_luminance_chroma_plot <- function(L = 75, C = 20, H = 0, n = 200) {
  Cmax <- max(C + 5, 150)
  Cseq <- seq(0, Cmax, length.out = n)
  Lseq <- seq(100, 0, length.out = n)
  grid <- expand.grid(C = Cseq, L = Lseq)
  image <- matrix(hex(polarLUV(grid$L, grid$C, H)), nrow = n, byrow = TRUE)
  grob <- grid::rasterGrob(image, width = 1, height = 1)

  sel_col <- polarLUV(L, C, H) # selected color in polar LUV
  df_sel <- data.frame(C = C, L = L)

  #gg# ggplot2::ggplot(df_sel, ggplot2::aes(C, L)) + ggplot2::annotation_custom(grob) +
  #gg#   ggplot2::geom_point(size = 5, color = cursor_color(L), fill = hex(sel_col), shape = 21) +
  #gg#   ggplot2::coord_fixed(xlim = c(0, Cmax), ylim = c(0, 100), expand = FALSE) +
  #gg#   ggplot2::theme_minimal()


   # Non-gg version
   grid$hex <- as.vector( t(image) )
   limits <- with( na.omit(grid), list( L = c(0,100), C = c(0,max(C)) ) )
   par( mar = c(3,3,1,1) )
   with( grid, graphics::plot( L ~ C, type="n", bty = "n", axes = FALSE,
                               asp = 1, xaxs = "i", yaxs = "i", xlim=limits$C, ylim=limits$L ) )
   graphics::abline( h = seq(-200,200,by=25), v = seq(-200,200,by=25), col = "gray80" )
   graphics::axis( side = 1, at = seq(limits$C[1L],limits$C[2L],by=25), col = NA, col.ticks = 1 )
   graphics::axis( side = 2, at = seq(limits$L[1L],limits$L[2L],by=25), col = NA, col.ticks = 1 )
   graphics::points( grid$L ~ grid$C, col = grid$hex, pch = 19 )
   # Selected color
   points( df_sel[1,"L"] ~ df_sel[1,"C"], cex = 2 )
   # Box
   graphics::box( col = "gray40" )
}

# Helper function to draw the color bar (gradient's).
# Input \code{seq} has to be numeric, sequence of values
# along the color bar dimension. Cols is an object of
# NA's and hex colors which can be converted to a vector.
# Typically a matrix. Length of \code{cols} has to be equal to
# length of \code{seq}.
plot_color_gradient <- function( seq, cols, sel, ylab = NA, ticks ) {
      # Change pars
      #hold.par <- par( no.readonly = TRUE )
      #on.exit( do.call( "par", hold.par ) )
      par( mar = c(2,2,.1,1) )
      # Compute args
      dx   <- 0.5 * median(diff(seq))
      seq  <- seq( min(seq), max(seq), length=length(cols) )
      args <- list( ybottom = rep(0,length(cols)), ytop = rep(1,length(cols)) )
      args$xleft <- seq - dx;        args$xright <- seq + dx
      args$col <- as.vector(cols);   args$border <- NA
      # Create color bar
      graphics::plot( NA, ylim = c(0,1), xlim = range(seq), xaxs = "i", yaxs = "i",
            xlab = NA, ylab = NA, bty = "n", axes = FALSE )
      do.call( "rect", args )
      if ( ! is.na(ylab) ) graphics::mtext( side = 2, line = 0.5, ylab, las = 2 )
      if ( missing(ticks)  ) ticks <- base::pretty( seq )
      graphics::points( sel, 0.5, cex = 2 )
      graphics::axis( side = 1, at = ticks, col = NA, col.ticks = 1 )
      graphics::box( col = "gray40" )
}

color_picker_C_gradient <- function(L = 75, C = 20, H = 0, n = 100) {
  Cmax <- max(C + 5, 150)
  Cseq <- seq(0, Cmax, length.out = n)
  image <- matrix(hex(polarLUV(L, Cseq, H)), nrow = 1, byrow = TRUE)
  grob <- grid::rasterGrob(image, width = 1, height = 1)

  sel_col <- hex(polarLUV(L, C, H))
  df_sel <- data.frame(C = C, H = H, L = L, y = 0)

  #y <- 0 # dummy assignment to make CRAN check happy
  #gg# ggplot2::ggplot(df_sel, ggplot2::aes(C, y)) + ggplot2::annotation_custom(grob) +
  #gg#   ggplot2::geom_point(size = 5, color = cursor_color(L), fill = sel_col, shape = 21) +
  #gg#   ggplot2::scale_y_continuous(expand = c(0, 0)) +
  #gg#   ggplot2::scale_x_continuous(limits = c(0, Cmax), expand = c(0, 0)) +
  #gg#   ggplot2::ylab("C") +
  #gg#   ggplot2::theme_minimal() +
  #gg#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
  #gg#                  axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5, size = 14),
  #gg#                  axis.text.y = ggplot2::element_blank(),
  #gg#                  axis.line.y = ggplot2::element_blank(),
  #gg#                  axis.ticks.y = ggplot2::element_blank(),
  #gg#                  panel.grid.major.y = ggplot2::element_blank(),
  #gg#                  panel.grid.minor.y = ggplot2::element_blank(),
  #gg#                  plot.margin = ggplot2::margin(3, 20, 3, 0))

   # Craw color gradient/color bar
   plot_color_gradient( Cseq, image, df_sel$C, "C" )
}

color_picker_H_gradient <- function(L = 75, C = 20, H = 0, n = 100) {
  Hseq = seq(0, 360, length.out = n)
  image <- matrix(hex(polarLUV(L, C, Hseq)), nrow = 1, byrow = TRUE)
  grob <- grid::rasterGrob(image, width = 1, height = 1)

  sel_col <- hex(polarLUV(L, C, H))
  df_sel <- data.frame(C = C, H = H, L = L, y = 0)

  #gg# y <- 0 # dummy assignment to make CRAN check happy
  #gg# ggplot2::ggplot(df_sel, ggplot2::aes(H, y)) + ggplot2::annotation_custom(grob) +
  #gg#   ggplot2::geom_point(size = 5, color = cursor_color(L), fill = sel_col, shape = 21) +
  #gg#   ggplot2::scale_y_continuous(expand = c(0, 0)) +
  #gg#   ggplot2::scale_x_continuous(limits = c(0, 360), expand = c(0, 0)) +
  #gg#   ggplot2::ylab("H") +
  #gg#   ggplot2::theme_minimal() +
  #gg#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
  #gg#                  axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5, size = 14),
  #gg#                  axis.text.y = ggplot2::element_blank(),
  #gg#                  axis.line.y = ggplot2::element_blank(),
  #gg#                  axis.ticks.y = ggplot2::element_blank(),
  #gg#                  panel.grid.major.y = ggplot2::element_blank(),
  #gg#                  panel.grid.minor.y = ggplot2::element_blank(),
  #gg#                  plot.margin = ggplot2::margin(3, 20, 3, 0))

   # Craw color gradient/color bar
   plot_color_gradient( Hseq, image, df_sel$H, "H", seq(0,360,by=45) )
}

color_picker_L_gradient <- function(L = 75, C = 20, H = 0, n = 100) {
  Lseq = seq(0, 100, length.out = n)
  image <- matrix(hex(polarLUV(Lseq, C, H)), nrow = 1, byrow = TRUE)
  grob <- grid::rasterGrob(image, width = 1, height = 1)

  sel_col <- hex(polarLUV(L, C, H))
  df_sel <- data.frame(C = C, H = H, L = L, y = 0)

  #gg# y <- 0 # dummy assignment to make CRAN check happy
  #gg# ggplot2::ggplot(df_sel, ggplot2::aes(L, y)) + ggplot2::annotation_custom(grob) +
  #gg#   ggplot2::geom_point(size = 5, color = cursor_color(L), fill = sel_col, shape = 21) +
  #gg#   ggplot2::scale_y_continuous(expand = c(0, 0)) +
  #gg#   ggplot2::scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  #gg#   ggplot2::ylab("L") +
  #gg#   ggplot2::theme_minimal() +
  #gg#   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
  #gg#                  axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5, size = 14),
  #gg#                  axis.text.y = ggplot2::element_blank(),
  #gg#                  axis.line.y = ggplot2::element_blank(),
  #gg#                  axis.ticks.y = ggplot2::element_blank(),
  #gg#                  panel.grid.major.y = ggplot2::element_blank(),
  #gg#                  panel.grid.minor.y = ggplot2::element_blank(),
  #gg#                  plot.margin = ggplot2::margin(3, 20, 3, 0))

   # Craw color gradient/color bar
   plot_color_gradient( Lseq, image, df_sel$L, "L" )
}

pal_plot <- function(colors)
{
  # convert colors to hex and find luminance for each
  col <- hex2RGB(colors)
  col <- as(col, "LUV")
  text_col <- cursor_color(col@coords[, 1L])

  n <- length(colors)
  graphics::par(mai = c(0, 0, 0, 0),
      mar = c(0, 0, 0, 0))
  graphics::plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab="")
  graphics::rect((0:(n-1)+.1)/n, 0, (1:n-.1)/n, 1, col = colors, border = NA)
  graphics::text((0:(n-1)+.5)/n, .5, labels = colors, col = text_col, cex = 1.2)
}

cursor_color <- function(L) {
  ifelse(L >= 50, "#000000", "#FFFFFF")
}

