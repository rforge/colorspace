# -------------------------------------------------------------------
# - NAME:        server.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2017-09-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2017-09-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2017-09-24 15:30 on thinkreto
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


# Function which converts the image and displays it in the shiny app
show <- function(img,type,output) {

   if ( is.character(img) ) {
      tmp <- img
      rm  <- FALSE
   } else {
      # Create temporary file name
      tmp <- tempfile(sprintf("hclconvert_%s",type),fileext=".png")
      # Convert image
      colorspace:::convert_image(img, type, tmp)
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

# Show images when loading the app (included in www/images)
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
      is_img   <- colorspace:::check_image_type( filename )

      # If is either png or jpeg: go ahead
      if ( is_img$png | is_img$jpg ) { 
         if ( is_img$png ) {
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
         rm(list=c("img","is_img","file","filename"))
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
