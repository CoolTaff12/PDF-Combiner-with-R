#-----------------------------
# This code was written by Leo Taffazoli 
# Created: 2022-10-26
# Last edited: 2022-10-28
# Tested in: R 4.2.1
#------------------------------

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages <- c("qpdf", "shiny", "shinyjs", "shinydashboard", "shinyWidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
  cat('\nBefore we begin, let us get the library needed.\n')
  print(new.packages)
  cat('\nDont worry, this will only happen the first time\nor when the packages needs an update!\n\n')
  
  if(length(new.packages)) install.packages(new.packages,type="win.binary")
  #if(length(new.packages)) install.packages(new.packages,lib.loc=CoD)
  cat('\n-------------------------\n')
}

rm(list = ls())
cat("\014")

library(qpdf, warn.conflicts = FALSE)
library(shiny, warn.conflicts = FALSE)
library(shinyjs, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(shinyWidgets, warn.conflicts = FALSE)

options(java.parameters = "-Xmx8000m",
        shiny.maxRequestSize = 100 * 1024 ^ 2,
        shiny.sanitize.errors = FALSE,
        encoding = 'UTF-8')

PDFset1 <<- NULL
PDFset2 <<- NULL
PDFname1 <<- NULL
PDFname2 <<- NULL

ui <- fluidPage(
  
  useShinyjs(),
  
  setBackgroundColor(
    color = c("#CACAD7", "#DEDEEB"),
    gradient = "linear",
    direction = "bottom"
  ),
  
  
  
  # Application title
  fluidRow(column(width = 12, align = "center", offset = 1,)),
  
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

server <- function(input, output, session) {
  
  
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
        reset('doPDFFront')
        PDFname1 <<- NULL
        PDFset1 <<- NULL
        output$brows1 <- renderText({ "Invalid file(s)" })
        break
      }}
    
  })
  
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
        reset('doPDFBack')
        PDFname2 <<- NULL
        PDFset2 <<- NULL
        output$brows2 <- renderText({ "Invalid file(s)" })
        break
      }}
    
  })
  
  
  observeEvent(input$startCombine, {
    #TryCatch
    result = tryCatch({
      output$brows3 <- renderPrint({ "Loading..." })
      
      #Match by name
      usersName <- gsub("CFI", "", (Sys.info()[["user"]]))
      
      usersDesktop <- sprintf("C:\\Users\\%s\\Desktop", tolower(usersName))
      tempFolder <<- paste(usersDesktop, "ProdTemp", sep = "\\")
      dir.create(tempFolder, showWarnings = FALSE)
      
      y <<- NULL
      
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
      winDialog(type = "ok", paste("Warning! Something is not right here!", w, sep = "\n\n"))
      cat(paste("Warning! Something is not right here!", w, sep = "\n\n"))
    }, error = function(e) {
      winDialog(type = "ok", paste("Error!", e, sep = "\n\n"))
      cat(paste("Error!", e, sep = "\n\n"))
      errorMessage <<- e
    }, finally = {
      
    })
  })
  
  
}

# Run the application 
shinyApp(ui=ui,server=server)
