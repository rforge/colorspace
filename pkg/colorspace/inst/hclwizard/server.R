# -------------------------------------------------------------------
# - NAME:        server.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2015-05-01
# -------------------------------------------------------------------
# - DESCRIPTION: This is the shiny guitar server script here.
# -------------------------------------------------------------------
# - EDITORIAL:   2015-05-01, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2016-10-26 13:34 on thinkreto
# -------------------------------------------------------------------

library(shiny)
library(colorspace)
library(dichromat)

#options( shiny.trace = TRUE )

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {


   if ( Sys.info()["nodename"] == "sculptor.uberspace.de" ) {
      delay(0, toggleState("closeapp", condition = F))
   }

   # ----------------------------------------------------------------
   # The different slider elements. Used later to show/hide or
   # set the sliders.
   # ----------------------------------------------------------------
   sliderElements <- c("H1","H2","C1","C2","L1","L2","P1","P2") #,"LEV")

   # ----------------------------------------------------------------
   # Loading PAL palette information and examples
   # ----------------------------------------------------------------
   palettes <- colorspace:::GetPaletteConfig()
   updateSelectInput(session,"EXAMPLE",choices=colorspace:::example.plots)

   # ----------------------------------------------------------------
   # Initialization of the first color map is triggering the plot
   # function 15 times which only creates workload on the server.
   # The deadcounter counts how often showColorMap has been triggered
   # and only plots the color Map on the X'th time. Note that the
   # x'th time is hardcoded in showColorMap()!!
   # ----------------------------------------------------------------
   deadcounter <- 0

   # ----------------------------------------------------------------
   # Helper function. Input: character vectors to show/hide.
   # ----------------------------------------------------------------
   showHideElements <- function(show,hide) {
      #for ( elem in hide ) hide(elem)
      #for ( elem in show ) show(elem)
      for ( elem in hide ) addClass(   elem,"hcl-is-hidden")
      for ( elem in show ) removeClass(elem,"hcl-is-hidden")
   }

   # ----------------------------------------------------------------
   # Used in different functions: returns the current settings for all
   # Parameters
   # ----------------------------------------------------------------
   getCurrentPalette <- function() {
      pal <- list()
      for ( elem in sliderElements ) {
         eval(parse(text=sprintf("pal$%s <- input$%s",elem,elem)))
      }
      pal$N <- input$N
      return(pal)
   }

   # ----------------------------------------------------------------
   # Change PAL (palettes to chose) whenever the typ changes
   # ----------------------------------------------------------------
   observeEvent(input$typ, {
      x <- list()
      for ( i in which(palettes$typ == input$typ) )
         x[[sprintf("%s",palettes$name[i])]] <- palettes$name[i]
      updateSelectInput(session,"PAL",choices = x)
   })

   # ----------------------------------------------------------------
   # Change palettes whenever the palette "typ" changes
   # ----------------------------------------------------------------
   observeEvent(input$PAL, {

      # Getting settings of the choosen color palette
      idx <- which(palettes$typ==input$typ & palettes$name==input$PAL)
      if ( length(idx) == 0 ) { return(FALSE); }
      name   <- input$PAL
      curPAL <- as.list(palettes[idx,])

      # In this case we have to set the slider values as
      # the user selected a new scheme!
      for ( elem in sliderElements ) {
         if ( is.na(palettes[idx,elem]) ) next
         updateSliderInput(session,elem,value=curPAL[[elem]])
      }

      # Show or hide sliders depending on the palette typ configuration
      showColorSliders(input$typ)
      showColorMap()
      
   })

   # ----------------------------------------------------------------
   # When the user changes the slider settings
   # ----------------------------------------------------------------
   sliderChanged <- function(elem) {
      # Else getting inputs and show color map
      if ( nchar(Sys.getenv("hclwizard_lock")) > 0 ) return
      pal <- getCurrentPalette()
      # Show new color map
      showColorMap()
   }
   observeEvent(input$N,   { sliderChanged("N")   })
   observeEvent(input$H1,  { sliderChanged("H1")  })
   observeEvent(input$H2,  { sliderChanged("H2")  })
   observeEvent(input$C1,  { sliderChanged("C1")  })
   observeEvent(input$C2,  { sliderChanged("C2")  })
   observeEvent(input$L1,  { sliderChanged("L1")  })
   observeEvent(input$L2,  { sliderChanged("L2")  })
   observeEvent(input$P1,  { sliderChanged("P1")  })
   observeEvent(input$P2,  { sliderChanged("P2")  })
   observeEvent(input$LEV, { sliderChanged("LEV") })
   # Do the same whenever the example changes
   observeEvent(input$EXAMPLE,  { sliderChanged("EXAMPLE")  })

   # ----------------------------------------------------------------
   # When the user changes the options (like setting visual
   # constraints or the desaturate option).
   # ----------------------------------------------------------------
   observeEvent(input$reverse,    { showColorMap() });
   observeEvent(input$fixup,      { showColorMap() });
   observeEvent(input$desaturate, { showColorMap() });
   observeEvent(input$constraint, { showColorMap() });

   # ----------------------------------------------------------------
   # When the user changes one of the values
   # ----------------------------------------------------------------
   setValueByUser <- function(elem) {
      value = eval(parse(text=sprintf("input$%sval",elem)))
      if ( nchar(value) > 0 ) {
         updateSliderInput(session,elem,value=value)
         updateTextInput(session, sprintf("%sval",elem), value="")
      }
   }
   observeEvent(input$H1set,    { setValueByUser("H1")   })
   observeEvent(input$H2set,    { setValueByUser("H2")   })
   observeEvent(input$C1set,    { setValueByUser("C1")   })
   observeEvent(input$C2set,    { setValueByUser("C2")   })
   observeEvent(input$L1set,    { setValueByUser("L1")   })
   observeEvent(input$L2set,    { setValueByUser("L2")   })
   observeEvent(input$P1set,    { setValueByUser("P1")   })
   observeEvent(input$P2set,    { setValueByUser("P2")   })
   observeEvent(input$Nset,     { setValueByUser("N")    })
   #observeEvent(input$LEVset,   { setValueByUser("LEV")  })

   # ----------------------------------------------------------------
   # Getting currently selected color scheme
   # ----------------------------------------------------------------
   getColors <- function(N=NULL,fun=FALSE) {
      # Current palette settings
      curtyp <- input$typ
      curPAL <- getCurrentPalette()
      if ( ! is.null(N) ) curPAL$N <- N
      fixup  <- input$fixup
      # Getting palette function
      if ( input$typ == "base" ) {
         pal <- `if`(tolower(input$PAL)=="bpy",colorspace:::bpy,eval(parse(text=tolower(input$PAL))))
      } else {
         pal <- colorspace:::GetPalette(curtyp,curPAL$H1,curPAL$H2,curPAL$C1,curPAL$C2,
                                               curPAL$L1,curPAL$L2,curPAL$P1,curPAL$P2,input$fixup)
      }
      # If fun is set to false: return values
      if ( ! fun ) {
         # Remove alpha (base color maps)
         colors <- pal(curPAL$N)
         # Add desaturation or constraints
         if ( input$desaturate ) colors <- desaturate(colors)
         if ( any(tolower(input$constraint) %in% c("protan","deutan","tritan")) )
            colors <- dichromat(colors,tolower(input$constraint))
         # Reverse if required
         if ( input$reverse ) colors <- rev(colors)
         return(colors)
      } else {
         return(pal)
      }
   }

   # ----------------------------------------------------------------
   # Show color map
   # Note that showColorMap also controls the output in the main
   # panel. Depending on the user view (if the user is on the 
   # export or choosecolors tab) we have to update the export
   # tab or the image. Not both.
   # ----------------------------------------------------------------
   showColorMap <- function() {

      deadcounter <<- deadcounter + 1
      if ( deadcounter <= 16 ) return(FALSE)

      colors <- getColors()

      output$colormap <- renderPlot({
         par(mar=rep(0,4),xaxt="n",yaxt="n",oma=rep(0,4),bty="n")
         image(matrix(1:length(colors),ncol=1),col=colors)
      },width=500,height=2)

      # Update the export tabs
      if ( input$maintabs == "export" ) {
         generateExport() 
      # Show image (plot)
      } else if ( input$maintabs == "choosecolors" & nchar(input$PAL) > 0 ) {
         plotExample(colors) #showColorMap()
      # Show spectrum
      } else if ( input$maintabs == "spectrum" ) {
         showSpectrum()
      }

      # Save color set to parent environment as we use it to generate
      # the output on the export tabs on demand.
      colormap <<- colors
   }

   # ----------------------------------------------------------------
   # Display spectrum
   # ----------------------------------------------------------------
   plotSpectrum <- function(colors) {
         RGB <- hex2RGB(colors)
         HCL <- as(RGB, "polarLUV")
         par(xaxt="n",yaxs="i",xaxs="i",mfrow=c(2,1),
             mar=c(2,0,0,0), oma=c(2,3,2,3))
         # RGB
         plot(0,type="n",ylim=c(0,1),xlim=c(1,length(colors)))
            lines(coords(RGB)[,"R"],lwd=2,col=2)
            lines(coords(RGB)[,"G"],lwd=2,col=3)
            lines(coords(RGB)[,"B"],lwd=2,col=4)
            legend("topleft",ncol=3,bty="n",fill=2:4,legend=c("red","green","blue"))
         mtext(side=3,"RGB Spectrum",cex=1.5,line=0.2)
         mtext(side=2,"all coordinates",line=2)
         # HCL
         plot(0,type="n",ylim=c(0,100),xlim=c(1,length(colors)))
            cols <- rainbow_hcl(3)
            lines((coords(HCL)[,"H"]+360)/7.2,lwd=2,col=cols[1L])
            lines(coords(HCL)[,"C"],lwd=2,col=cols[2L])
            lines(coords(HCL)[,"L"],lwd=2,col=cols[3L])
            labels <- seq(-360,360,length.out=9)
            axis(side=4,at=(labels+360)/7.2,labels=labels)
            legend("bottomleft",ncol=3,bty="n",fill=cols,legend=c("hue","chroma","luminance"))
         mtext(side=3,"HCL Spectrum",cex=1.5,line=0.2)
         mtext(side=2,"chroma and luminance",line=2)
         mtext(side=4,"hue",line=2)
      }
   showSpectrum <- function() {
      colors <- getColors(100)
      output$spectrum <- renderPlot(
         plotSpectrum(colors),
         width=800, height=800
      )
   }

   # ----------------------------------------------------------------
   # Plotting example
   # ----------------------------------------------------------------
   plotExample <- function(colors) {
      cmd <- sprintf("output$plot <- renderPlot({colorspace:::Plot%s(colors)},width=800,height=600)",input$EXAMPLE)
      eval(parse(text=cmd))
   }

   # ----------------------------------------------------------------
   # Getting currently selected color scheme and draws the
   # corresponding color bar using shiny
   # ----------------------------------------------------------------
   showColorSliders <- function(curtyp) {
      idx <- which(palettes$typ==curtyp & palettes$name==input$PAL)
      show <- hide <- c()
      for ( elem in sliderElements ) {
         if ( is.na(palettes[idx,elem]) ) { hide <- c(hide,sprintf("%s-wrapper",elem)) }
         else                             { show <- c(show,sprintf("%s-wrapper",elem)) }
         showHideElements(show,hide)
      }
   }

   # ----------------------------------------------------------------
   # Export colors: generate export content
   # ----------------------------------------------------------------
   generateExport <- function() {

      colors <- getColors()
      colors[is.na(colors)] <- "#ffffff"

      # --------------------------
      # RAW
      # --------------------------
      # Generate RGB coordinates
      RGB <- attr(hex2RGB(colors),"coords")

      # Generate output string
      append <- function(x,new) c(x,new)
      raw1 <- raw2 <- raw3 <- raw4 <- list()
      # RGB 0-1
      raw1 <- append(raw1,"<div style=\"clear: both;\">")
      raw1 <- append(raw1,"<span class=\"output-raw\">")
      raw1 <- append(raw1,"RGB values [0-1]")
      for ( i in 1:nrow(RGB) )
         raw1 <- append(raw1,sprintf("<code>%5.3f %5.3f %5.3f</code>",RGB[i,1],RGB[i,2],RGB[i,3]))
      raw1 <- append(raw1,"</span>")
      # RGB 0-255
      raw2 <- append(raw2,"<span class=\"output-raw\">")
      raw2 <- append(raw2,"RGB values [0-255]")
      RGB <- round(RGB*255)
      for ( i in 1:nrow(RGB) )
         raw2 <- append(raw2,gsub(" ","&nbsp;",sprintf("<code>%4d %4d %4d</code>",RGB[i,1],RGB[i,2],RGB[i,3])))
      raw2 <- append(raw2,"</span>")
      # HEX colors
      raw3 <- append(raw3,"<span class=\"output-raw\">")
      raw3 <- append(raw3,"HEX colors, no alpha")
      for ( col in colors )
         raw3 <- append(raw3,sprintf("<code>%s</code>",col))
      raw3 <- append(raw3,"</span>")
      # Color boxes (visual bar) 
      raw4 <- append(raw4,"<span class=\"output-raw\">")
      raw4 <- append(raw4,"Color Map")
      for ( col in colors )
         raw4 <- append(raw4,sprintf("<cbox style='background-color: %s'></cbox>",col))
      raw4 <- append(raw4,"</span>")
      raw4 <- append(raw4,"</div>")

      output$exportRAW1 <- renderText(paste(raw1,collapse="\n"))
      output$exportRAW2 <- renderText(paste(raw2,collapse="\n"))
      output$exportRAW3 <- renderText(paste(raw3,collapse="\n"))
      output$exportRAW4 <- renderText(paste(raw4,collapse="\n"))

      
      # -----------------------------
      # For GrADS
      # -----------------------------
      gastr <- c()
      gastr <- append(gastr,"<div class=\"output-grads\">")
      gastr <- append(gastr,"<comment>** Define colors palette</comment>") 
      for ( i in 1:nrow(RGB) ) {
         gastr <- append(gastr,gsub(" ","&nbsp;",sprintf("<code>'set rgb %02d %4d %4d %4d'</code>",
                                             i+19,RGB[i,1],RGB[i,2],RGB[i,3])))
      }
      gastr <- append(gastr,sprintf("<code>'set ccols %s'</code>",paste(1:nrow(RGB)+19,collapse=" ")))
      gastr <- append(gastr,sprintf("<code>'set clevs %s'</code>",paste(round(seq(0,100,length=nrow(RGB)-1),1),collapse=" ")))
      gastr <- append(gastr,"<comment>** Open data set via DODS</comment>")
      gastr <- append(gastr,"<comment>** Open data set via DODS</comment>")
      gastr <- append(gastr,strftime(Sys.Date()-1,"<code>'sdfopen http://nomads.ncep.noaa.gov:9090/dods/gfs_1p00/gfs%Y%m%d/gfs_1p00_00z_anl'</code>"))
      output$exportGrADS <- renderText(paste(gastr,collapse="\n"))

      # -----------------------------
      # For Python
      # -----------------------------
      pystr <- c()
      pystr <- append(pystr,"<div class=\"output-python\">")
      pystr <- append(pystr,"<comment>## Define choosen color palette first</comment>") 
      pystr <- append(pystr,sprintf("<code>colors = (%s)</code>",
                      paste(sprintf("\"%s\"",colors),collapse=",")))
      pystr <- append(pystr,"</div>")

      output$exportPython <- renderText(paste(pystr,collapse="\n"))

      # -----------------------------
      # For Matlab
      # -----------------------------
      RGB <- attr(hex2RGB(colors),"coords")
      mstr <- c()
      mstr <- append(mstr,"<div class=\"output-matlab\">")
      mstr <- append(mstr,"<comment>%% Define rgb matrix first (matrix size ncolors x 3)</comment>")
      vardef <- "colors = ["
      for ( i in 1:nrow(RGB) ) {
         if ( i == 1 ) {
            tmp <- sprintf("<code>%s%5.3f,%5.3f,%5.3f;</code>",
                           vardef,RGB[i,1],RGB[i,2],RGB[i,2])
         } else if ( i < nrow(RGB) ) {
            tmp <- sprintf("<code>%s%5.3f,%5.3f,%5.3f;</code>",
                           paste(rep(" ",nchar(vardef)),collapse=""),RGB[i,1],RGB[i,2],RGB[i,2])
         } else {
            tmp <- sprintf("<code>%s%5.3f,%5.3f,%5.3f]</code>",
                           paste(rep(" ",nchar(vardef)),collapse=""),RGB[i,1],RGB[i,2],RGB[i,2])
         }
         mstr <- append(mstr,gsub(" ","&nbsp;",tmp))
      }
      mstr <- append(mstr,"</div>")

      output$exportMatlab <- renderText(paste(mstr,collapse="\n"))
      
   }
   
   # ----------------------------------------------------------------
   # Update main tabs (show export output, spectrum, or example plot)
   # ----------------------------------------------------------------
   updateMainTabContent <- function() {
      # Update the export tabs
      if ( input$maintabs == "export" ) {
         generateExport() 
      # Show image (plot)
      } else if ( input$maintabs == "choosecolors" & nchar(input$PAL) > 0 ) {
         plotExample(colormap)
      # Spectrum plot
      } else if ( input$maintabs == "spectrum" ) { 
         showSpectrum()
      }
   }
   observeEvent(input$maintabs,  updateMainTabContent() );


   # downloadHandler() takes two arguments, both functions.
   # The content function is passed a filename as an argument, and
   #   it should write out data to that filename.
   getRGB <- function(int=FALSE) {
      colors <- getColors()
      if ( int ) { scale = 255; digits = 0 } else { scale = 1; digits = 3 }
      RGB <- round(attr(hex2RGB(colors),"coords")*scale,digits)
      return(RGB)
   }
   output$downloadRAW1 <- downloadHandler(
      file <- "colormap_rgb.txt",
      content = function(file) {
         write.table(getRGB(FALSE), file, sep = ",", col.names = TRUE, row.names = FALSE)
      }
   )
   output$downloadRAW2 <- downloadHandler(
      file <- "colormap_RGB.txt",
      content = function(file) {
         write.table(getRGB(TRUE), file, sep = ",", col.names = TRUE, row.names = FALSE)
      }
   )
   output$downloadRAW3 <- downloadHandler(
      file <- "colormap_hex.txt",
      content = function(file) {
         write.table(getColors(), file, sep = ",",
            col.names = FALSE, row.names = FALSE)
      }
   )

   # ----------------------------------------------------------------
   # Return colors at the moment as soon as the user stops the app.
   # Would be nicer to return a function as from choose_palette,
   # but I have not found a way yet.
   # ----------------------------------------------------------------
   observeEvent(input$closeapp, stopApp(invisible(getColors(fun=TRUE))) );

   # - Configuration
   ticks <- FALSE # to show ticks or not to show ticks


})
