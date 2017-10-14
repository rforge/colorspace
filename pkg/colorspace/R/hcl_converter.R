#' Graphical User Interface to Check Images for Color Constriants
#'
#' A graphical user interface (GUI) to check an existing jpg/png image for (possible)
#' color constraints. The image will be converted to protanope vision, deutanope vision,
#' and a desaturated version (monochromatic vision).
#' Allows a rapid check whether the colors used in the image show some
#' constraints with respect to color deficiency or color blindness.
#'
#' @param x If not set an interactive GUI will be started. If \code{x} is of type
#'    \code{character} it has to be the full path to an image of type png or jpg/jpeg.
#'    The image will be converted and stored on disc, no GUI.
#' @param overwrite \code{logical}. Only used if \code{x} is provided. Allow the
#'    function to overwrite files on disc if they exist.
#' @param shiny.trace \code{logical}. Can be set to \code{TRUE} for more verbose
#'    output when the GUI is started (development flag). 
#' @rdname hcl_converter
#' @export
hcl_converter <- function( x, overwrite = FALSE, shiny.trace = FALSE) {

   # If input 'x' is missing: start interactive GUI
   if ( missing(x) ) {
      # Requirements for shiny application
      stopifnot(requireNamespace("shiny"))
      appDir <- system.file("hclconverter", package = "colorspace")
      if (appDir == "")
         stop("Could not find hclconverter app directory. Try re-installing `colorspace`.", call. = FALSE)
      # Start shiny
      options(shiny.trace=shiny.trace)
      pal <- shiny::runApp(appDir, display.mode = "normal", quiet = TRUE )

   # Do not start shiny: convert and save images on disc
   } else {
      is_img <- check_image_type(x)
      if ( ! is_img$png & ! is_img$jpg )
         stop(sprintf("Image \"%s\" is neither png nor jpg/jpeg. Stop.",x))
      types <- c("deutan","protan","tritan","desaturate")
      files <- sprintf("%s/%s_%s",dirname(x),types,basename(x))
      # If overwrite is FALSE: check if files exist and stop if.
      if ( ! overwrite ) {
         for ( file in files ) {
            if ( file.exists(file) )
               stop(sprintf("File \"%s\" exists. Remove image first, or use hcl_converter( x, overwrite = TRUE)",file))
         }
      }
      # Read image
      if ( is_img$png ) {
         in.img <- png::readPNG( x )
      } else {
         in.img <- jpeg::readJPEG( x )
      }
      # Convert and save image
      for ( type in types ) {
         outfile <- sprintf("%s/%s_%s",dirname(x),type,basename(x))
         cat(sprintf("Convert %-15s -> %s\n",type,outfile))
         out.img <- convert_image( in.img, type, outfile ) 
      }
   }
}

#' Check Image Type based on Postfix
#'
#' Checking image file type based on image file name! Used to decide
#' which package has to be used to read an image from disc (\code{png}/\code{jpeg}).
#'
#' @param x, \code{string} containing (full) path to image.
#' @return Returns a \code{list} with two elements. Each can take
#'    \code{TRUE} or \code{FALSE}. If image is of type \code{png}
#'    \code{png=TRUE}, if image is of type \code{jpg/jpeg} \code{jpg=TRUE}.
#'    If non of both, both will be \code{FALSE}. Method is not case sensitive.
check_image_type <- function( x ) {
   ## Identify file type (postfix)
   tail1 <- function(x) x[length(x)]
   postfix  <- tolower(tail1(strsplit(basename(x),"\\.")[[1]]))
   list("png" = postfix == "png",
        "jpg" = postfix %in% c("jpeg","jpg"))
}

# The convert function used
#' Convert Colors of an Image
#'
#' Used in \code{hcl_converter}. Takes an image object and converts
#' the colors using deutan/protan/tritan/desaturate functions. Image
#' will be written to disc as a png image.
#'
#' @param img \code{array} as returned by \code{readPNG} and \code{readJPEG}
#'    of size \code{height x width x depth}. The depth coordinate contains
#'    R/G/B and alpha if given (png).
#' @param type \code{string} name of the function which will be used to
#'    convert the colors (\code{deutan}, \code{protan}, \code{tritan}, \code{desaturate}).
#'    If set to \code{original} the image will be written as is.
#' @param target \code{string} with (full) path to resulting image. Has to
#'    be a png image name!
convert_image <- function(img, type, target) {

   # - Save original colors
   if ( type == 'original' ) {
      png::writePNG(img,target=target)
      return(TRUE)
   }
   # Picking data
   RGB       <- matrix(as.numeric(img[,,1:3])*255,nrow=3,byrow=TRUE,
                       dimnames=list(c("R","G","B"),NULL))
   RGB       <- do.call(type,list("col"=RGB))
   img[,,1]  <- matrix( RGB["R",]/255, dim(img)[1], dim(img)[2])
   img[,,2]  <- matrix( RGB["G",]/255, dim(img)[1], dim(img)[2])
   img[,,3]  <- matrix( RGB["B",]/255, dim(img)[1], dim(img)[2])
   png::writePNG(img,target=target)
   return(TRUE)

   # - Loading RGB values of the original image
   R         <- as.vector(img[,,1])
   G         <- as.vector(img[,,2])
   B         <- as.vector(img[,,3])
   # - Convert to hex colors 
   hcols     <- colorspace::hex(sRGB(R=R,G=G,B=B))
   # - Convert to color blindness  
   hex       <- do.call(type,list("col"=hcols))
   RGB       <- colorspace::hex2RGB(hex)
   RGB       <- attr(RGB,'coords')
   img2      <- img
   img2[,,1] <- matrix( RGB[,'R'], dim(img)[1], dim(img)[2])
   img2[,,2] <- matrix( RGB[,'G'], dim(img)[1], dim(img)[2])
   img2[,,3] <- matrix( RGB[,'B'], dim(img)[1], dim(img)[2])

   # Save image to disc
   png::writePNG(img2,target=target)
   return(TRUE)
}
