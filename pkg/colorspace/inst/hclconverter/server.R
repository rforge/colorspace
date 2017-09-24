# -------------------------------------------------------------------
# - NAME:        server.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2017-09-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2017-09-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2017-09-24 12:47 on thinkreto
# -------------------------------------------------------------------


library("shiny")
library("colorspace")
library("png")  # Processing png images
library("jpeg") # Processing jpeg images
library("RCurl")

# Restrict upload file size to 1MB (online) and 50MB (local)
options(shiny.maxRequestSize=1*1024^2)
if ( Sys.getenv('SHINY_PORT') == "" ) options(shiny.maxRequestSize=50*1024^2)

# Resize the image (neares neighbor interpolation/approximation)
resizeImage = function(im, w.out=600, h.out) {

   # Dimensions of the input image 
   din <- list("height"=dim(im)[1],"width"=dim(im)[2],"layers"=dim(im)[3])
   # If h.out is missing: proportional scaling
   if ( missing(h.out) ) h.out <- round(din$height/(din$width/w.out))
   # Create empty array to store resized image
   out = array(0,c(h.out,w.out,din$layers))
   # Compute ratios -- final number of indices is n.out, spaced over range of 1:n.in
   w_ratio = din$width  / w.out
   h_ratio = din$height / h.out
   # Do resizing -- select appropriate indices
   for ( l in 1:din$layers )
      out[,,l] <- im[ floor(h_ratio* 1:h.out), floor(w_ratio* 1:w.out), l]

   return(out)
}

# The convert function used
convert <- function(img, type, target) {

   # - Fast if bw
   if ( type == 'desaturate' ) {
      bw <- (img[,,1]*0.2126+img[,,2]*0.7152+img[,,3]*0.0722)
      writePNG(bw,target=target)
      return(TRUE)
   }
   # - Save original colors
   if ( type == 'original' ) {
      writePNG(img,target=target)
     return(TRUE)
   }
   # - Loading RGB values of the original image
   R <- as.vector(img[,,1])
   G <- as.vector(img[,,2])
   B <- as.vector(img[,,3])
   # - Convert to hex colors 
   hcols <- hex(sRGB(R=R,G=G,B=B))
   # - Convert to color blindness  
   hex <- do.call(type,list("col"=hcols))
   RGB <- hex2RGB(hex)
 
   RGB <- attr(RGB,'coords')
   img2 <- img
   img2[,,1] <- matrix( RGB[,'R'] ,dim(img)[1],dim(img)[2])
   img2[,,2] <- matrix( RGB[,'G'] ,dim(img)[1],dim(img)[2])
   img2[,,3] <- matrix( RGB[,'B'] ,dim(img)[1],dim(img)[2])
 
   # Save image to disc
   writePNG(img2,target=target)
   return(TRUE)
}

# Function which converts the image and displays it in the shiny app
show <- function(img,type,output) {

   if ( is.character(img) ) {
      tmp <- img
      rm  <- FALSE
   } else {
      # Create temporary file name
      tmp <- tempfile(sprintf("hclconvert_%s",type),fileext=".png")
      # Convert image
      convert(img, type, tmp)
      rm <- TRUE
   }

   # Base64-encode file
   txt <- base64Encode(readBin(tmp, "raw", file.info(tmp)[1, "size"]), "txt")
   # Create inline image, save & open html file in browser 
   html <- sprintf('data:image/png;base64,%s', txt)
   # Rendering images
   output[[sprintf("image%s",type)]] <- renderUI({ img(src=html) })
   # Delete temporary file
   if ( rm ) file.remove(tmp)
}

showInit <- function( type, output ) {
   output[[sprintf("image%s",type)]] <- renderUI({img(src=sprintf("images/rainbow_%s.png",type))})
}

# Define server logic to read selected file ----
shinyServer(function(input, output, session) {

   observe({
      output$status <- renderText({paste("Please upload an image first.",
         "After uploading the conversion needs few seconds, so please be patient!")})
      for ( type in c("orig","protan","deutan","desaturate") ) showInit(type,output)
   })

   # Does everything
   observe({
      # Info
      if ( ! is.null(input$file) ) {
         output$status <- renderText({"File uploaded, starting conversion ..."})
      }

      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      # Is a data.frame containing one row (multiple file upload
      # is not allowed),
      # Columns: name, size, type (mime), datapath (temporary file location)
      req(input$file)

      ## Identify file type (postfix)
      file     <- input$file$datapath[1]
      filename <- input$file$name[1]
      postfix  <- tolower(tail(strsplit(basename(filename),"\\.")[[1]],1))
      #showNotification( sprintf("%s: %s",postfix,file) )

      ispng <- NULL
      if ( postfix == "png" ) {
         ispng <- TRUE
      } else if ( postfix %in% c("jpeg","jpg") ) {
         ispng <- FALSE
      }
      
      # If is either png or jpeg: go ahead
      if ( is.logical(ispng) ) {
         if ( ispng ) {
            img <- try( readPNG( file ) )
         } else {
            img <- try( readJPEG( file ) )
         }

         # Broken image or no PNG/JPEG
         if ( "try-error" %in% class(img) ) {
            tmp <- paste("Sorry, image could not have been read. Seems that the file you",
               "uploaded was no png/jpg/jpeg file or a broken file. Please check your file",
               "and try again!")
            output$status <- renderText({tmp})
            return(FALSE)
         }

         # Resize image if width > 800
         if ( dim(img)[2] > 800 ) img <- resizeImage(img,800)
         ##if ( dim(img)[2] > 800 && Sys.getenv('SHINY_PORT') != "" ) img <- resizeImage(img,800)

         # Show uploaded image
         show(file,"orig",output)
         # Convert
         for ( type in c("protan","deutan","desaturate") ) show(img,type,output)

         # Delete uploaded file
         file.remove( file )
         rm(list=c("img","ispng","file","filename","postfix"))
         output$status <- renderText({"Image successfully converted."})

         return(TRUE)
      } else {
         tmp <- paste("Uploaded image has had unknown file name extension!",
                  "Please upload png, jpg or jpeg (not case sensitive).")
         output$status <- renderText({tmp})
         return(FALSE)
      }
   })

}) # End of shinyServer
