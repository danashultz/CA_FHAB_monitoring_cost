#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Reprex for Heili
#DS 11/2/2020

#Issue: how to manipulate reactive data frame

#Re-create shiny app, but with only a few inputs
#Shore station
#ELISA
#Microscopy

#Load packages
library(shiny)
library(tidyverse)

########## Set cost variables ##########

#Multipliers 

#ELISA cost per toxin as ec
ec <- 150

#Microscopy cost for each sample as mi
mi <- 40

########## Define UI ##########

ui <- fluidPage(
    
    titlePanel(h1("Reprex - how to manipulate a reactive data frame")), 
    
    sidebarLayout(
        sidebarPanel("Enter information about proposed field plan below:",
                     
                     checkboxGroupInput(inputId = "ELISA",
                                        label = "Select toxins to measure with ELISA",
                                        choices = c("Microcystin"=1, "Cylindrospermopsin"=2, "Anatoxin"=3, "Saxitoxin"=4),
                                        selected = 1:4),
                     checkboxGroupInput(inputId = "micros",
                                        label = "Will microscopy ID be conducted?",
                                        choices = "Yes",
                                        selected = "Yes"),
        ), #close out sidebar panel       
        
        #Specify WHERE TO PUT outputs
        mainPanel(textOutput(outputId = "ELISA"),
                  textOutput(outputId = "Microscopy"),
                  tableOutput('table_a')
        ) #close out mainPanel
    ) #close out Side Panel layout
) # close out fluid page ui

########################################
########## Shiny server ################

server <- function(input, output) {

#Calculate costs as reactive expressions
    
    #Reactive expression for cost of ELISA analyses
    e1<- reactive( {length(input$ELISA)*ec })
 
    #Reactive expression for cost of microscopy analysis
    m1<- reactive( {length(input$micros)*mi })
    
 #Create outputs
    
    #Costs are calculated as reactive variables in the lines above (e1, m1, etc.),
    #Only need to call variable for outputs (not re-calculate cost)
    
    #ELISA  
    output$ELISA <- renderText({
        if (length(input$ELISA > 1)) {
            paste("ELISA: $",e1())}
        else {
            "No ELISA costs"}
    })  
    
    output$Microscopy <- renderText({
        if (length(input$micros > 1)) {
            paste("Microscopy ID: $",m1())}
        else {
            "No Microscopy ID costs"}
    }) 

    #Create a data frames from reactive variables for plotting
    a <-   reactive ( {
        data.frame(ELISA = e1(), microscopy = m1(),
                   stringsAsFactors = TRUE) %>%
            gather(factor_key = TRUE) %>%
            rename("cost" = "value") %>% 
            add_column(test = c(1,2))
    } )
    
#Things I have tried to create a new column:
    
    #(1) Calling reactive data frame with new column name
    #a()$test2 <- c(1:2) 
    #Error in a()$test2 <- c(1:2) : invalid (NULL) left side of assignment
    
    #(2) Piping in the calculation of percent of overall cost
    # a <-   reactive ( {
    #     data.frame(ELISA = e1(), microscopy = m1(),
    #                stringsAsFactors = TRUE) %>%
    #         gather(factor_key = TRUE) %>%
    #         rename("cost" = "value") %>% 
    #         add_column(test = c(1,2)) %>% 
    #         add_column(test2 = a()$cost)
    # } )
    #Error: C stack usage  15925120 is too close to the limit
    #Creates a recursive function even trying to reprint a()$cost without any sort of calculation
    
    #Create output
    output$table_a <- renderTable(a())    

} #close out server script


# Run the application (knit ui and server together)
shinyApp(ui = ui, server = server)
