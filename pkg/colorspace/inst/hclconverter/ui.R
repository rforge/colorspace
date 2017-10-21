# -------------------------------------------------------------------
# - NAME:        ui.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2017-09-16
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2017-09-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2017-10-19 13:22 on thinkreto
# -------------------------------------------------------------------
library("shiny")

desc <- paste("Please select a file from your disc which you want",
              "to convert. Only PNG/JPG/JPEG files are allowed.",
              "Maximum allowed file size is 1 Megabyte.")

# Define UI for data upload app ----
shiny::shinyUI(bootstrapPage(
#shiny::shinyUI(fluidPage(

   theme = "hclconvert.css",

   # App title ----
   titlePanel("HCLwizard Image Converter"),

   fluidRow(
     column(4,
         img(src="images/descimage.png",id="descimage"),
         p(desc)
     ),
     column(4, 
         # Input: Select a file ----
         fileInput("file", label=h4("Select an image to upload"),
                   multiple = FALSE,
                   accept = c("image/png","image/jpeg"))
     ),
     column(4,h4("HCL converter status"), textOutput("status"))
   ),

 
   # Horizontal line ----
   tags$hr(),
 
 
   tabsetPanel(
      tabPanel("Images",
         withTags( div(class="outputgrid", h3("Original"),    uiOutput("imageorig")) ),
         withTags( div(class="outputgrid", h3("Protanope"),   uiOutput("imageprotan")) ),
         withTags( div(class="outputgrid", h3("Deuteranope"), uiOutput("imagedeutan")) ),
         withTags( div(class="outputgrid", h3("Desaturated"), uiOutput("imagedesaturate")) )
      ),
      tabPanel("App Info", 
         htmlOutput("appInfo"),
         includeHTML("html/appInfo.html")
      ),
      selected = "Images"
   )

)) # End of shinyUI
