#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Install and import libraries
install.packages("shiny")
install.packages("shinythemes")
library(shiny)
library(shinythemes)

# Read in the DT model and NB model
DT_Model <- readRDS("DT.rds")
NB_D_Model <- readRDS("NB.rds")


####################################
# User interface                   
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Body Image Dissatisfaction among Malaysian Adults'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("BESAA1", 
                 label = "I like what I look like in pictures.", 
                 value = 0),
    numericInput("BESAA2", 
                 label = "Other people consider me good looking.", 
                 value = 0),
    numericInput("BESAA3", 
                 label = "I'm proud of my body.", 
                 value = 0),
    numericInput("BESAA6", 
                 label = "I like what I see when I look in the mirror.", 
                 value = 0),
    numericInput("BESAA8", 
                 label = "I am satisfied with my weight.", 
                 value = 0),
    numericInput("BESAA9", 
                 label = "I wish I looked better.", 
                 value = 0),
    numericInput("BESAA10", 
                 label = "I really like what I weigh.", 
                 value = 0),
    numericInput("BESAA13", 
                 label = "My looks upset me.", 
                 value = 0),
    numericInput("BESAA14", 
                 label = "I'm as nice looking as most people.", 
                 value = 0),
    numericInput("BESAA15", 
                 label = "I'm pretty happy about the way I look.", 
                 value = 0),
    numericInput("BESAA16", 
                 label = "I feel I weigh the right amount for my height.", 
                 value = 0),
    numericInput("BESAA17", 
                 label = "I feel ashamed of how I look.", 
                 value = 0),
    numericInput("BESAA18", 
                 label = "Weighing myself depresses me.", 
                 value = 0),
    numericInput("BESAA19", 
                 label = "My weight makes me unhappy ", 
                 value = 0),
    numericInput("BESAA20", 
                 label = "My looks help me to get dates.", 
                 value = 0),
    numericInput("BESAA22", 
                 label = "I think I have a good body.", 
                 value = 0),
    numericInput("BESAA23", 
                 label = "I'm looking as nice as I'd like to.", 
                 value = 0),
  
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("A",
               "B",
               "C",
               "D",
               "E",
               "F",
               "G",
               "H",
               "I",
               "J",
               "K",
               "L",
               "M",
               "N",
               "O",
               "P",
               "Q"),
      Value = as.character(c(input$BESAA1,
                             input$BESAA2,
                             input$BESAA3,
                             input$BESAA6,
                             input$BESAA8,
                             input$BESAA9,
                             input$BESAA10,
                             input$BESAA13,
                             input$BESAA14,
                             input$BESAA15,
                             input$BESAA16,
                             input$BESAA17,
                             input$BESAA18,
                             input$BESAA19,
                             input$BESAA20,
                             input$BESAA22,
                             input$BESAA23)),
      stringsAsFactors = FALSE)
    
    cluster <- 0
    df <- rbind(df, cluster)
    input <- transpose(df)
    write.table(input,"BID.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    testing123 <- read.csv(paste("BID", ".csv", sep=""), header = TRUE)
    
    output <- data.frame(Prediction=predict(NB_D_model,testing123), round(predict(NB_D_model,testing123,type="prob"), 16))
    print(output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


ui <- fluidPage(
  uiOutput("numInput"),
  textOutput("text")
)

server <- function(input, output) {
  global <- reactiveValues(numVal = 4, numMin = 0, numMax = 4)
  
  numVal <- reactive({
    if(!is.null(input$num)){
      if(input$num < global$numMin) return(global$numMin)
      if(input$num > global$numMax) return(global$numMax)     
      return(input$num)
    }else{
      return(global$numVal)
    }
  })
  
  output$numInput <- renderUI(numericInput("num", "", min = global$numMin, 
                                           max = global$numMax, value = numVal()))
  
  output$text <- renderText(input$num)
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
