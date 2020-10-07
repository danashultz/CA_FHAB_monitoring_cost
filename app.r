#Attempt to build shiny app for FHABs monitoring cost project

#Start date: 10/5/2020
#DS

library(shiny)
library(tidyverse)
library(readxl)

explanation <- read_excel("ExplanationOfCalculations.xlsx")

########## Set cost variables ##########

#water quality cost per parameter (TN, TP, Chla, Phyco, SS) as wq
wq <- 50

#ELISA cost per toxin as ec
ec <- 150

#Base pcr cost for each sample qp_base (maybe the extraction?)
qp_prep <- 36

#Pcr cost for each sample as qp
qp_analysis <- 50 

#Microscopy cost for each sample as mi
mi <- 40

#Should change to this format for field costs, but haven't tried this, yet
sh1 <- 1000
sh2 <- sh1*0.5
bo1 <- 3000
bo2 <- bo1*0.5

########## Shiny UI ##########

ui <- fluidPage(
  titlePanel("FHAB Monitoring Cost Shiny App"), 
  sidebarLayout(
    sidebarPanel("Enter information about proposed field plan in the boxes below",
                 
#BLOCK 1 - FIELD COSTS
      numericInput(inputId = "waterbodies",
        label = "Number of waterbodies",
        value = 1),
      numericInput(inputId = "shore",
        label = "Number of shore stations per waterbody",
        value = 0),
      numericInput(inputId = "boat",
        label = "Number of boat stations per waterbody",
        value = 0),
    
#BLOCK 2 - RECURRENCE
    numericInput(inputId = "period",
                 label = "Sampling period (weeks)",
                 value = 1),
    numericInput(inputId = "interval",
                 label = "Sampling interval (weeks between visits)",
                 value = 1),
    
#BLOCK 3 - Selection of Indicators
    checkboxGroupInput(inputId = "wq",
                       label = "Select water quality measurements",
                       choices = c("Total Nitrogen"=1, "Total Phosphorus"=2,
                                   "Chlorophyll-a"=3, "Phycocyanin" = 4,
                                   "Suspended Solids"=5)),
    checkboxGroupInput(inputId = "ELISA",
                 label = "Select toxins to measure with ELISA",
                 choices = c("Microcystin", "Cylindrospermopsin", "Anatoxin", "Saxitoxin")),
    checkboxGroupInput(inputId = "qPCR",
                   label = "Select toxins genes to quantify with qPCR",
                   choices = c("Microcystin", "Cylindrospermopsin", "Anatoxin", "Saxitoxin")),
    checkboxGroupInput(inputId = "micros",
                  label = "Will microscopy ID be conducted?",
                  choices = "Yes")
    ),       

#Specify WHERE TO PUT outputs
#Server function below specifies HOW TO GET output content
    mainPanel(tags$h3("Scope of work"),
              textOutput(outputId = "visits"),
              textOutput(outputId = "sites"),
              tags$hr(),
              tags$h3("Cost of field work"),
              textOutput(outputId = "shore1"),
              textOutput(outputId = "shore2"),
              textOutput(outputId = "boat1"),
              textOutput(outputId = "boat2"),
              #Need to summarize calculated field costs for a total
              tags$hr(),
              tags$h3("Cost of water quality measurements"),
              textOutput(outputId = "waterquality"),
              tags$hr(),
              tags$h3("Cost of cyanotoxin analyses"),
              textOutput(outputId = "ELISA"),
              textOutput(outputId = "qPCR_prep"),
              textOutput(outputId = "qPCR_analysis"),
              textOutput(outputId = "Microscopy"),
              tags$hr(),
              tags$h3("Explanation of Calculations for Cost"),
              tableOutput('table'))))

########## Shiny server ##########

server <- function(input, output) {
#Create a reactive variable, vis, to use downstream as the number of visits
  #This works like a funciton,
  #Downstream, vis needs to be used like a function (aka "vis()")
  vis <- reactive( {input$period/input$interval})

#Create a reactive variable, N, to use downstream as the total number of sites 
  N <- reactive( {(input$waterbodies*input$shore*vis()) +
                   (input$waterbodies*input$boat*vis())})

#Create outputs

####FIELD WORK COSTS####  
    
#Visits as calculated in vis() above  
  output$visits <- renderText({
    paste("Number or visits to each waterbody:", vis())
  })
  
#Visits as calculated in vis() above  
  output$sites <- renderText({
    paste("Total stations visited (and samples collected) during program:", N())
  })  
  
#Cost of first shore station if input$shore>0
  output$shore1 <- renderText({
    if (input$shore > 0) {
    paste("First shore site across waterbodies: $",
          input$waterbodies*sh1*vis())} else {
            "No shore stations"
          }
  })
  
#Cost of any additional shore stations if shore>1
  output$shore2 <- renderText({
    if (input$shore > 1) {
    paste("Additional shore sites across waterbodies: $",
          input$waterbodies*(input$shore-1)*sh2*vis())} else {
          "No additional shore stations"
    }
  })

#Cost of first boat station if input$boat>0
  output$boat1 <- renderText({
    if (input$boat > 0) {
    paste("First boat site across waterbodies: $",
          input$waterbodies*bo1*vis())} else {
            "No boat stations"
          }
  })

#Cost of first boat station if input$boat>0
  output$boat2 <- renderText({
    if (input$boat > 1) {
    paste("Additional boat sites across waterbodies: $",
          input$waterbodies*(input$boat-1)*bo2*vis())} else {
            "No additional boat stations"
          }
  })

#### WATER QUALITY COSTS ####
  
  output$waterquality <- renderText({
    if (length(input$wq > 1)) {
      paste("Cost of WQ analyses: $",
            length(input$wq)*wq*N())} else {
              "No Water Quality Costs"
            }
  })  
  
#Old if else method, keeping just in case needed for another purpose
  #   output$waterquality <- renderText({
  #   if (length(input$wq) == 5) {
  #   paste("Cost of WQ analyses: $", N()*250)
  #     } else if (length(input$wq) == 4) {
  #       paste("Cost of WQ analyses: $", N()*200)
  #     } else if (length(input$wq) == 3) {
  #       paste("Cost of WQ analyses: $", N()*150)
  #     } else if (length(input$wq) == 2) {
  #       paste("Cost of WQ analyses: $", N()*100)
  #     } else if (length(input$wq) == 1) {
  #       paste("Cost of WQ analyses: $", N()*50)
  #     } else
  #       "No Water Quality Costs"
  # })

#### CYANOTOXIN COSTS ####

#ELISA  
  output$ELISA <- renderText({
    if (length(input$ELISA > 1)) {
      paste("ELISA: $",
            length(input$ELISA)*ec*N())} else {
              "No ELISA costs"
            }
  })  

#qPCR
#prep - cost per sample (maybe this is an extraction step?)
  output$qPCR_prep <- renderText({
    if (length(input$qPCR > 1)) {
      paste("qPCR prep: $",
            N()*qp_prep)} else {
              "No qPCR prep costs"
    }
  })
  
#analysis - cost per sample and per number of toxin genes selecte
  output$qPCR_analysis <- renderText({
    if (length(input$qPCR > 1)) {
      paste("qPCR analysis: $",
            length(input$qPCR)*qp_analysis*N())} else {
              "No qPCR analysis costs"
    }
  })
  
 output$Microscopy <- renderText({
   if (length(input$micros > 1)) {
     paste("Microscopy ID: $",
           mi*N())} else {
             "No Microscopy ID costs"
   }
 }) 

  
  output$table <- renderTable(explanation)    
}


# Run the application (knit ui and server together)
shinyApp(ui = ui, server = server)
