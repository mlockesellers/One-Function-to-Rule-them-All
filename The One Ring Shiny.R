#Load packages
library(shiny)
library(bslib)
library(car)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggrepel)
library(ggpp)
library(patchwork)
library(stringr)
library(stats)
setwd("~/Documents/The One Ring Code")

#Source function
source("One Function to Rule Them All.R")

#Define UI for app that runs a for loop
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  #App title ----
  titlePanel("One Function to Rule Them All"),
 
   #Sidebar panel for inputs ----
  sidebarLayout(
    sidebarPanel (
      fileInput("csv_file_name",label = "Upload Your .csv File Here", accept = ".csv"),
      selectInput("CpFT_ref", label = "Cuspal Formation Time Reference Population", c("Ohio_R","Ohio_M","Southern_Africa_R","Northern_Europe_R"), selected = "Ohio_R"),
      actionButton("action", label = "Submit")
    ),
 #Main panel with outputs
    mainPanel (
   tabsetPanel(
     tabPanel("Age at Defect", tableOutput("StressEvents")),
     tabPanel("Download", 
              downloadButton("downloadData","Download .csv"))
      )
    )
  )
)

#Define server logic
server <- function(input,output) {
 
  #Store inputs with reactive values
   rv <- reactiveValues(
    csv_file_name = NULL,
    CpFT_ref = NULL
  )
   
   observeEvent(input$action, {
     #Check if a file has been uploaded
     if (is.null(input$csv_file_name)) {
       showModal (modalDialog(
         title = "Error",
         "Please upload your .csv file to get started.",
         easyClose = TRUE
       ))
       return()
     }
     
     #Store inputs in reactive values
     rv$csv_file <- input$csv_file_name$datapath
     rv$CpFT_ref <- input$CpFT_ref
   })
   
   #Render table of Stress Events
   output$StressEvents <- renderTable({
     #Ensure both inputs are available
     req(rv$csv_file, rv$CpFT_ref) 
       
       #Perform long-running operation within withProgress
       #Save function output as a variable
     Stress_Events <- LEH.fun(rv$csv_file,rv$CpFT_ref)
       
       #Return result
       Stress_Events
   })
   
   # Download handler for CSV file
   output$downloadData <- downloadHandler(
     filename = function() {
       "Stress_Events.csv"
     },
     content = function(file) {
       # Generate the data frame again to ensure it's up-to-date
       Stress_Events <- LEH.fun(rv$csv_file, rv$CpFT_ref)
       
       # Write CSV to file
       write.csv(Stress_Events, file, row.names = FALSE)
     }
   )
}

#Run the app
shinyApp(ui = ui, server = server)
