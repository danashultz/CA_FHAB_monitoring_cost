#Attempt to build shiny app for FHABs monitoring cost project

#Start date: 10/5/2020
#DS

#Hi Jayme

#test commit

library(shiny)
library(tidyverse)
library(readxl)

explanation <- read_excel("ExplanationOfCalculations.xlsx")

compute_angle = function(perc){
  angle = -1
  
  if(perc < 0.5) # 1st half [90, -90]
    angle = (180 - (perc/0.5) * 180) - 90
  else # 2nd half [90, -90]
    angle = (90 - ((perc - 0.5)/0.5) * 180)
  
  return(angle)
}

########################################
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

#field costs
sh1 <- 1000
sh2 <- sh1*0.5
bo1 <- 3000
bo2 <- bo1*0.5

########################################
########## Shiny UI ####################

ui <- fluidPage(
  titlePanel(h1("FHAB Monitoring Cost Shiny App",
  
  h6("This is a draft website to estimate the cost of an
                                                    FHABs Ambient Monitoring Program. Do not use without prior
                                                    consulting with Jayme Smith (jaymes@sccwrp.org) and Keith
                                                    Bouma-Gregson (keith.bouma-gregson@waterboards.ca.gov)"))), 
  
  sidebarLayout(
    sidebarPanel("Enter information about proposed field plan below:",
                 
#BLOCK 1 - FIELD COSTS
      numericInput(inputId = "waterbodies",
        label = "Number of waterbodies",
        value = 50),
      numericInput(inputId = "shore",
        label = "Number of shore stations per waterbody",
        value = 2),
      numericInput(inputId = "boat",
        label = "Number of boat stations per waterbody",
        value = 0),
    
#BLOCK 2 - RECURRENCE
    numericInput(inputId = "period",
                 label = "Sampling period (weeks)",
                 value = 20),
    numericInput(inputId = "interval",
                 label = "Sampling interval (weeks between visits)",
                 value = 2),
    
#BLOCK 3 - Selection of Indicators
    checkboxGroupInput(inputId = "wq",
                       label = "Select water quality measurements",
                       choices = c("Total Nitrogen"=1, "Total Phosphorus"=2,"Chlorophyll-a"=3, "Phycocyanin" = 4,"Suspended Solids"=5)),
    checkboxGroupInput(inputId = "ELISA",
                       label = "Select toxins to measure with ELISA",
                       choices = c("Microcystin", "Cylindrospermopsin", "Anatoxin", "Saxitoxin")),
    checkboxGroupInput(inputId = "qPCR",
                       label = "Select toxins genes to quantify with qPCR",
                       choices = c("Microcystin", "Cylindrospermopsin", "Anatoxin", "Saxitoxin")),
    checkboxGroupInput(inputId = "micros",
                       label = "Will microscopy ID be conducted?",
                       choices = "Yes"),
#add agency logos
#tags$hr(),
#img(src = "Final logo DarkerFishSmall5inRGB_72dpi.png", height = 100, width = 137, style = "display:block;margin-left: auto; margin-right: auto;"),
#tags$hr(),
#img(src = "SWAMP_logo_RGB.png", height = 120, width = 100, style = "display:block;margin-left: auto; margin-right: auto;")


tags$hr(),
img(src = "Final logo DarkerFishSmall5inRGB_72dpi.png", height = 100, width = 137),
#tags$hr(),
img(src = "SWAMP_logo_RGB.png", height = 120, width = 100)

    ), #close out sidebar panel       

#Specify WHERE TO PUT outputs
#Server function below specifies HOW TO GET output content
    mainPanel(
      tabsetPanel(
        tabPanel("Cost", 
                 tags$h3("Scope of work"),
                 textOutput(outputId = "visits"),
                 textOutput(outputId = "sites"),
                 tags$hr(),
                 tags$h3("Cost of field work"),
                 textOutput(outputId = "shore1"),
                 textOutput(outputId = "shore2"),
                 textOutput(outputId = "boat1"),
                 textOutput(outputId = "boat2"),
                 br(),
                 tags$b(tags$em((textOutput(outputId = "FieldCostSum")))),
                 #Need to summarize calculated field costs for a total
                 tags$hr(),
                 tags$h3("Cost of water quality measurements"),
                 tags$b(tags$em(textOutput(outputId = "waterquality"))),
                 tags$hr(),
                 tags$h3("Cost of cyanotoxin analyses"),
                 textOutput(outputId = "ELISA"),
                 textOutput(outputId = "qPCR_prep"),
                 textOutput(outputId = "qPCR_analysis"),
                 textOutput(outputId = "Microscopy"),
                 br(),
                 tags$b(tags$em(textOutput(outputId = "CyanotoxSum"))),
                 tags$hr()),
        
        tabPanel("Explanation of Cost",
                 tableOutput('table')),
        tabPanel("Plot_Dev",
                 tableOutput('table_a'),
                 tableOutput('table_b'),
                 tableOutput('myFirstLevel'),
                 tableOutput('mySecondLevel'),
                 tableOutput('myThirdLevel'),
                 textOutput('sumTotalCost'),
                 plotOutput('plot_level1'),
                 plotOutput('plot_level2.1'),
                 plotOutput('plot_level2.2'),
                 plotOutput('plot_level3.1'),
                 plotOutput('plot_level3.2'))
        ) #close out tabs
      ) #close out mainPanel
    ) #close out Side Panel layout
  ) # close out fluid page ui

########################################
########## Shiny server ################

server <- function(input, output) {
  #Create a reactive variable, vis, to use downstream as the number of visits
  #This works like a funciton,
  #Downstream, vis needs to be used like a function (aka "vis()")
  vis <- reactive( {input$period/input$interval})
  
  #Create a reactive variable, N, to use downstream as the total number of sites 
  N <- reactive( {(input$waterbodies*input$shore*vis()) +
      (input$waterbodies*input$boat*vis())})
  
  #All calculated values will be stored as variables in order to make a dataframe
  
  #Cost of first shore station
  s1<- reactive( { if (input$shore > 0) {
    input$waterbodies*sh1*vis()}
    else
    0
  })
  #Cost of additional shore stations
  s2<- reactive( { if (input$shore > 1) {
    input$waterbodies*(input$shore-1)*sh2*vis()}
    else
      0
  })
    
  #Cost of first boat station
  b1<- reactive( { if (input$boat > 0) {
    input$waterbodies*bo1*vis()}
    else
      0
  })

  #Cost of additional boat stations
  b2<- reactive( { if (input$boat > 1) {
    input$waterbodies*(input$boat-1)*bo2*vis()}
    else
      0
  })
  
  
FieldCostSum <- reactive ( {
  s1()+s2()+b1()+b2()
})
  
  #Cost of water quality analyses
  w1<- reactive( {length(input$wq)*wq*N() })
  
  #Cost of ELISA analyses
  e1<- reactive( {length(input$ELISA)*ec*N() })
  #Cost of qPRR extraction
  qp1<- reactive( { if (length(input$qPCR > 0)) {
    N()*qp_prep
  } else
    0
    })

  #Cost of qPCR analysis
  qa1<- reactive( {length(input$qPCR)*qp_analysis*N() })
  #Cost of microscopy analysis
  m1<- reactive( {length(input$micros)*mi*N() })
  
  CyanotoxSum <- reactive( {
    e1()+qp1()+qa1()+m1()
  }) 
  
#Create outputs

####FIELD WORK COSTS####  
  
#Costs are calculated as reactive variables in the lines above (s1, s2, etc.),
  #Only need to call variable for outputs (not re-calculate cost)
    
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
    paste("First shore site across waterbodies: $", s1())}
    else {
      "No shore stations"}
  })
  
#Cost of any additional shore stations if shore>1
  output$shore2 <- renderText({
    if (input$shore > 1) {
    paste("Additional shore sites across waterbodies: $", s2())}
    else {
          "No additional shore stations"}
  })

#Cost of first boat station if input$boat>0
  output$boat1 <- renderText({
    if (input$boat > 0) {
    paste("First boat site across waterbodies: $",b1())}
    else {
            "No boat stations"}
  })

#Cost of first boat station if input$boat>0
  output$boat2 <- renderText({
    if (input$boat > 1) {
    paste("Additional boat sites across waterbodies: $",b2())}
    else {
            "No additional boat stations"}
  })
  
  # output$FieldCostSum <- renderText({
  #   if (input$boat == 0 & input$shore == 0) {
  #     paste("No field costs")
  #   } else if (input$shore == 0) {
  #     paste("Sum of field costs: $",(b1()+b2()))
  #   } else if (input$boat == 0) {
  #     paste("Sum of field costs: $",(s1()+s2()))
  #   } else 
  #     paste("Sum of field costs: $", (s1()+s2()+b1()+b2()))
  # })
  
output$FieldCostSum <- renderText({
  if (FieldCostSum() > 1) {
    paste("Sum of field costs: $",FieldCostSum())}
  else {
    "No field costs"}
})

  
#### WATER QUALITY COSTS ####
  
  output$waterquality <- renderText({
    if (length(input$wq > 1)) {
      paste("Sum of WQ analysis costs: $",w1())}
    else {
              "No Water Quality Costs"}
  })  
  
#### CYANOTOXIN COSTS ####

#ELISA  
  output$ELISA <- renderText({
    if (length(input$ELISA > 1)) {
      paste("ELISA: $",e1())}
    else {
              "No ELISA costs"}
  })  

#qPCR
#prep - cost per sample (maybe this is an extraction step?)
  output$qPCR_prep <- renderText({
    if (length(input$qPCR > 1)) {
      paste("qPCR prep: $",qp1())}
    else {
              "No qPCR prep costs"}
  })
  
#analysis - cost per sample and per number of toxin genes selecte
  output$qPCR_analysis <- renderText({
    if (length(input$qPCR > 1)) {
      paste("qPCR analysis: $",qa1())}
    else {
              "No qPCR analysis costs"}
  })
  
 output$Microscopy <- renderText({
   if (length(input$micros > 1)) {
     paste("Microscopy ID: $",m1())}
   else {
             "No Microscopy ID costs"}
 }) 

# output$CyanotoxSum <- renderText({
#    if (length(input$ELISA) == 0 & length(input$qPCR) == 0 & length(input$micros) == 0) {
#      paste("No cyantoxin analysis costs")
#    } else 
#      paste("Sum of cyanotoxin analyses costs: $", (e1()+qp1()+qa1()+m1()))
#  }) 
 
 output$CyanotoxSum <- renderText({
   if (CyanotoxSum() > 0) {
     paste("Sum of cyanotoxin analyses costs: $", CyanotoxSum())}
   else {
     "No cyantoxin analysis costs"}
   })
   
   
  output$table <- renderTable(explanation)    


#Plot

#Create two data frames from reactive variables for plotting
#a sub categories
a <- reactive ( {
  tibble(s1(), s2(), b1(), b2(), w1(), e1(), qp1(), qa1(), m1()) %>%
  gather() %>%
  rename("cost" = "value")
  } )

 # a$key() <- reactive ( {
 #   fct_relevel(a$key(), levels = c("s1", "s2", "b1", "b2", "w1", "e1", "qp1", "qa1", "m1"))
 # } )

 #b broad categories
 b <- reactive ( {
   tibble(FieldCostSum() , w1() , CyanotoxSum()) %>%
   gather()%>%
   rename("cost" = "value")
 } )
# 
# b$key <-fct_relevel(b$key, level = c("FieldCostSum", "w1", "CyanotoxSum"))
# 
#FIRST LEVEL INSIDE THE SUNBURST
 
myFirstLevel = reactive ( {
  a() %>% summarize(total_cost=sum(cost))
  } )
sum_total_cost = reactive ( {
  sum(a()$cost)
} )

#Visualize total cost
sunburst_1 = reactive(
  ggplot() +
  geom_bar(data = myFirstLevel(), aes(x=1, y=total_cost), fill='darkgrey', stat='identity') +
  geom_text(data = myFirstLevel(), aes(x=1, y=sum_total_cost()/2, label=paste('Total Field Cost = $\n',
            sum_total_cost())), color='white')
)
# 
# myColors <- reactive({
#   c(s1() = "#440154FF", 
#               s2() = "#472D7BFF",   
#               b1() = "#3B528BFF",
#               b2() = "#2C728EFF",
#               w1() = "#21908CFF", 
#               e1() = "#27AD81FF", 
#               qp1() = "#5DC863FF",
#               qa1() = "#AADC32FF",
#               m1() = "#FDE725FF",
#               FieldCostSum() = "#472D7BFF", 
#               CyanotoxSum() = "#85D54AFF")
#               })

#Visualize total cost and stacked bar of subcategories
sunburst_2.1 = reactive (
sunburst_1() +
  geom_bar(data=a(),
           aes(x=2, y=cost, fill=fct_rev(key)),
           color='white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=a(), aes(label=paste(key, cost), x=2, y=cost), position='stack') 
)


#First, function to rotate text to a readable angle
mySecondLevel = reactive ( {
  a() %>%
  mutate(running=cumsum(cost), pos = running - cost/2) %>%
  group_by(1:n()) %>%
  mutate(angle=compute_angle((running - cost/2) / sum_total_cost()))
} )



sunburst_2.2 = reactive(
  sunburst_1() +
  geom_bar(data=mySecondLevel(), aes(x=2, y=cost, fill=fct_rev(key)),
           color='white', position='stack', stat = 'identity', size = 0.6) +
  geom_text(data=mySecondLevel(),aes(label=paste(key),
                                 x=2, y=pos, angle=angle))+
  #scale_y_continuous(labels=comma) +
  #scale_fill_continuous(low='white', high='darkred') +
  scale_fill_viridis(discrete = TRUE)+
  #scale_fill_manual(values = myColors)+
  coord_polar('y') + theme_minimal()
)

#Third LEVEL INSIDE THE SUNBURST

#Visualize as a stacked bar
sunburst_3.1 =
  reactive(
sunburst_2.1() +
  geom_bar(data=b(),
           aes(x=3, y=cost, fill=fct_rev(key)),
           color = 'white', position='stack', stat='identity', size=0.6) + 
  geom_text(data=b(), aes(label=paste(key, cost), x=3, y=cost), position='stack')
)
#Function to rotate text to a readable angle should be run above

myThirdLevel = reactive ( {
  b() %>%
  mutate(running=cumsum(cost), pos = running - cost/2) %>%
  group_by(1:n()) %>%
  mutate(angle=compute_angle((running - cost/2) / sum_total_cost()))
} )

sunburst_3.2 =
  reactive(
  sunburst_2.2() +
  geom_bar(data=myThirdLevel(), aes(x=3, y=cost, fill=fct_rev(key)),
           color='white', position='stack', stat = 'identity', size = 0.6) +
  geom_text(data=myThirdLevel(),aes(label=paste(key),
                                x=3, y=pos, angle=angle))
)

# #CREATE PLOT TAB OUTPUTS

output$table_a <- renderTable(a())    
output$table_b <- renderTable(b())

output$myFirstLevel <- renderTable(myFirstLevel())
output$mySecondLevel <- renderTable(mySecondLevel())
output$myThirdLevel <- renderTable(myThirdLevel())

output$sumTotalCost <- renderText(sum_total_cost())

output$plot_level1 <- renderPlot( {sunburst_1() } ) 

output$plot_level2.1 <- renderPlot( {sunburst_2.1() } ) 

output$plot_level2.2 <- renderPlot( {sunburst_2.2() } ) 

output$plot_level3.1 <- renderPlot( {sunburst_3.1()})

output$plot_level3.2 <- renderPlot( {sunburst_3.2()})

} #close out server script





# Run the application (knit ui and server together)
shinyApp(ui = ui, server = server)
