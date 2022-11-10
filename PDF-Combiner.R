#------------Info-------------
# This code was written by Leo Taffazoli 
# Created: 2022-10-26
# Last edited: 2022-11-09
# Tested in: R 4.2.1
# Run command at the end of the script after loading the packages
#-----------------------------

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#------Install and/or Load packes----
# We first ask the system if these packages in the list are installed.
list.of.packages <- c("qpdf", "shiny", "shinyjs", "shinydashboard", "shinyWidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
  #If one or many packages are not installed, the system makes sure to install them right away
  cat('\nBefore we begin, let us get the library needed.\n')
  print(new.packages)
  cat('\nDont worry, this will only happen the first time\nor when the packages needs an update!\n\n')
  
  if(length(new.packages)) install.packages(new.packages,type="win.binary")
  #if(length(new.packages)) install.packages(new.packages,lib.loc=CoD)
  cat('\n-------------------------\n')
}
#Remove the data/variables from the enviroment
rm(list = ls())
cat("\014")

#Loads the packages and ignores warnings 
library(qpdf, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets, warn.conflicts = FALSE)

#------------------

#------Shiny Options to allow larger file to be uploaded----
options(java.parameters = "-Xmx8000m",
        shiny.maxRequestSize = 100 * 1024 ^ 2,
        shiny.sanitize.errors = FALSE,
        encoding = 'UTF-8')
#------------------

PDFset1 <<- NULL
PDFset2 <<- NULL
PDFname1 <<- NULL
PDFname2 <<- NULL

#------Shiny UI----
ui <- fluidPage(
  
  #Makes the javascript functions avalible for Shiny applications
  useShinyjs(),
  
  #setBackgroundColor set background color.
  setBackgroundColor(
    color = c("#CACAD7", "#DEDEEB"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  
  
  # Application title
  fluidRow(column(width = 12, align = "center", offset = 1,)),
  
  #Buttons and text
  mainPanel(tabsetPanel(type = "tabs",
                        tabPanel("Home", h3("Combine pdf"),
                                 h5("Welcome"),
                                 hr(h4("Combine", style="color:red")),
                                 fileInput(inputId = "doPDFFront", label="Upload PDF Front", placeholder = "No file(s) selected", width = "230px", multiple = TRUE, accept=c(".pdf")),
                                 fileInput(inputId = "doPDFBack", label="Upload PDF Back", placeholder = "No file(s) selected", width = "230px", multiple = TRUE, accept=c(".pdf")),
                                 textOutput("brows1"),
                                 textOutput("brows2"),
                                 
                                 hr(h4("Export")),
                                 actionButton("startCombine", "Start Combining"),
                                 br(),
                                 textOutput("brows3"))
  )
  )
)
#------------------

#------Shiny functions----
server <- function(input, output, session) {
  
  #When pressing the Upload PDF Front button
  observeEvent(input$doPDFFront, {
    
    PDFname1 <<- NULL
    PDFset1 <<- NULL
    
    for(nr in 1:length(input$doPDFFront$datapath)) {   
      
      #If the file(s) contains .pdf
      if (grepl('.pdf', input$doPDFFront$datapath[nr]) == TRUE) {
        
        PDFname1 <<- c(PDFname1, input$doPDFFront$name[nr])
        PDFset1 <<- c(PDFset1, input$doPDFFront$datapath[nr])
        
        output$brows1 <- renderText({ "Front batch selected" })
      }
      else {
		#Reset if one file isn't a .pdf file
        reset('doPDFFront')
        PDFname1 <<- NULL
        PDFset1 <<- NULL
        output$brows1 <- renderText({ "Invalid file(s)" })
        break
      }}
    
  })
  
  #When pressing the Upload PDF Back button
  observeEvent(input$doPDFBack, {
    
    PDFname2 <<- NULL
    PDFset2 <<- NULL
    
    for(nr in 1:length(input$doPDFBack$datapath)) {   
      
      #If the file(s) contains .pdf
      if (grepl('.pdf', input$doPDFBack$datapath[nr]) == TRUE) {
        
        PDFname2 <<- c(PDFname2, input$doPDFBack$name[nr])
        PDFset2 <<- c(PDFset2, input$doPDFBack$datapath[nr])
        
        output$brows2 <- renderText({ "Back batch selected" })
      }
      else {
	   #Reset if one file isn't a .pdf file
        reset('doPDFBack')
        PDFname2 <<- NULL
        PDFset2 <<- NULL
        output$brows2 <- renderText({ "Invalid file(s)" })
        break
      }}
    
  })
  
  #When pressing the Start Combining button
  observeEvent(input$startCombine, {
    #TryCatch
    result = tryCatch({
      output$brows3 <- renderPrint({ "Loading..." })
      
      #Match by name
      usersName <- Sys.info()[["user"]]
      
      #Creating a temporary folder to put all the pdf combined, this will be saved on desktop
      usersDesktop <- sprintf("C:\\Users\\%s\\Desktop", tolower(usersName))
      tempFolder <<- paste(usersDesktop, "TempFolder", sep = "\\")
      dir.create(tempFolder, showWarnings = FALSE)
      
      y <<- NULL
      
      #Going through the list of datapath in alphabetic order
      for(nr in 1:length(input$doPDFBack$datapath)) {
        
        cat(paste(nr, "out of" , length(input$doPDFBack$datapath), "\n", sep = " "))
        
        #Combining pdf into a new pdf and giving it a generic name
        qpdf::pdf_combine(input = c(PDFset1[nr], PDFset2[nr]),
                          output = paste(tempFolder, nr, sep = "\\"))
        
        #Rename the files
        file.rename(paste(tempFolder, nr, sep = "\\"),
                    paste(tempFolder, PDFname1[nr], sep = "\\"))
        
        
      }
      
      
      output$brows3 <- renderPrint({ "Sucess! The file is now in your directoy." })
      cat("Sucess! The file is now in your directoy.")
    }, warning = function(w) {
      #If warning, do this
      winDialog(type = "ok", paste("Warning! Something is not right here!", w, sep = "\n\n"))
      cat(paste("Warning! Something is not right here!", w, sep = "\n\n"))
    }, error = function(e) {
      #If error, do this
      winDialog(type = "ok", paste("Error!", e, sep = "\n\n"))
      cat(paste("Error!", e, sep = "\n\n"))
      errorMessage <<- e
    }, finally = {
      
    })
  })
  
  
}
#------------------

#------Run the Shiny application----
shinyApp(ui=ui,server=server)
#------------------
