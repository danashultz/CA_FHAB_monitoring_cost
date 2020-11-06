#Shiny app for FHABs monitoring cost project
#For Jayme Smith, Keith Bouma-Gregson and Marisa VanDyke

#Start date: 10/5/2020
#Dana Shultz

library(shiny)
library(tidyverse)
library(readxl)

#Load file that will print as a table_costCalculations
explanation <- read_excel("ExplanationOfCalculations.xlsx")

########################################
########## Set cost variables ##########

#Cost of each analysis as initially defined by KBG

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
        value = 50, min = 0),
      numericInput(inputId = "shore",
        label = "Number of shore stations per waterbody",
        value = 2, min = 0),
      numericInput(inputId = "boat",
        label = "Number of boat stations per waterbody",
        value = 0, min = 0),
    
#BLOCK 2 - RECURRENCE
    numericInput(inputId = "period",
                 label = "Sampling period (weeks)",
                 value = 20, min = 0),
    numericInput(inputId = "interval",
                 label = "Sampling interval (weeks between visits)",
                 value = 2, min = 0),
    
#BLOCK 3 - Selection of Indicators
    checkboxGroupInput(inputId = "wq",
                       label = "Select water quality measurements",
                       choices = c("Total Nitrogen"=1, "Total Phosphorus"=2,"Chlorophyll-a"=3, "Phycocyanin" = 4,"Suspended Solids"=5),
                       selected = 1:5),
    checkboxGroupInput(inputId = "ELISA",
                       label = "Select toxins to measure with ELISA",
                       choices = c("Microcystin"=1, "Cylindrospermopsin"=2, "Anatoxin"=3, "Saxitoxin"=4),
                       selected = 1:4),
    checkboxGroupInput(inputId = "qPCR",
                       label = "Select toxins genes to quantify with qPCR",
                       choices = c("Microcystin"=1, "Cylindrospermopsin"=2, "Anatoxin"=3, "Saxitoxin"=4),
                       selected = 1:4),
    checkboxGroupInput(inputId = "micros",
                       label = "Will microscopy ID be conducted?",
                       choices = "Yes",
                       selected = "Yes"),

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
        tabPanel("Costs", 
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
                 tags$h3("Cost of FHAB analyses"),
                 textOutput(outputId = "ELISA"),
                 textOutput(outputId = "qPCR_prep"),
                 textOutput(outputId = "qPCR_analysis"),
                 textOutput(outputId = "Microscopy"),
                 br(),
                 tags$b(tags$em(textOutput(outputId = "CyanotoxSum"))),
                 tags$hr(),
                 tags$h3("Total Cost of FHAB Monitoring Program"),
                 textOutput(outputId = "siteCost"),
                 textOutput(outputId = "visitCost"),
                 tags$b(tags$em(textOutput(outputId = "program"))),
                 tags$hr()),
        
        tabPanel("Table and Plot",
                 tableOutput('table_b'), 
                 tableOutput('table_a'),
                 plotOutput('plot1')),
                 
        tabPanel("Explanation of Cost Calculations",
                 tableOutput('table_costCalculations'))
        
        ) #close out tabs
      ) #close out mainPanel
    ) #close out Side Panel layout
  ) # close out fluid page ui

########################################
########## Shiny server ################

server <- function(input, output) {
  #Create a reactive variable, vis, to use downstream as the number of visits
  #Reactive expressions works like a function,
  #Downstream, vis needs to be called like a function (aka "vis()")
  vis <- reactive( {input$period/input$interval})
  
  #Create a reactive variable, N, to use downstream as the total number of sites 
  N <- reactive( {(input$waterbodies*input$shore*vis()) +
      (input$waterbodies*input$boat*vis())})
  
  #Calculated values will be stored as below variables
  
  ####Field costs
  
  #Cost of first shore station
  s1<- reactive( { if (input$shore > 0) {input$waterbodies*sh1*vis()} else 0})
  
  #Cost of additional shore stations
  s2<- reactive( { if (input$shore > 1) {input$waterbodies*(input$shore-1)*sh2*vis()}
    else 0})
    
  #Cost of first boat station
  b1<- reactive( { if (input$boat > 0) {input$waterbodies*bo1*vis()}else 0})

  #Cost of additional boat stations
  b2<- reactive( { if (input$boat > 1) {input$waterbodies*(input$boat-1)*bo2*vis()}
    else 0})
  
  #Summation of all field costs
  FieldCostSum <- reactive ({s1()+s2()+b1()+b2()})

  ####Water Quality Costs
  
  #Cost of water quality analyses
  w1<- reactive( {length(input$wq)*wq*N() })
  
  ####FHAB analysis costs
  
  #Cost of ELISA analyses
  e1<- reactive({length(input$ELISA)*ec*N()})
  
  #Cost of qPRR extraction
  qp1<- reactive({if(length(input$qPCR > 0)){N()*qp_prep} else 0})

  #Cost of qPCR analysis
  qa1<- reactive( {length(input$qPCR)*qp_analysis*N() })
  
  #Cost of microscopy analysis
  m1<- reactive( {length(input$micros)*mi*N() })
  
  #Summation of FHAB analysis costs
  CyanotoxSum <- reactive({e1()+qp1()+qa1()+m1()}) 

  #Summation of of all analyses for each site aka waterbody
  siteCost <- reactive({
    (s1()+s2()+b1()+b2()+(length(input$wq)*wq*N())+e1()+qp1()+qa1()+m1())/input$waterbodies
  })
  
  #Summation of all analyses for each visit to a waterbody
   visitCost<- reactive({
    (s1()+s2()+b1()+b2()+(length(input$wq)*wq*N())+e1()+qp1()+qa1()+m1())/input$waterbodies/vis()
  })
  
  #Summation of all costs for the program
  prog <- reactive({
    s1()+s2()+b1()+b2()+(length(input$wq)*wq*N())+e1()+qp1()+qa1()+m1()
  })    
  
#Create outputs

####FIELD WORK COSTS####  
  
#Costs are calculated as reactive variables in the lines above (s1, s2, etc.),
  #Only need to call variable for outputs (not re-calculate cost)
    
#Visits as calculated in vis() above  
  output$visits <- renderText({
    paste("Number or visits to each waterbody:", format(vis(), big.mark = ",", scientific = FALSE))
  })
  
#Sites as calculated in N() above  
  output$sites <- renderText({
    paste("Total shore and boat stations visited during program:", format(N(), big.mark = ",", scientific = FALSE))
  })  
  
#Cost of first shore station if input$shore>0
  output$shore1 <- renderText({
    if (input$shore > 0) {
    paste("First shore site across waterbodies: $", format(s1(), big.mark = ",", scientific = FALSE))}
    else {
      "No shore stations"}
  })
  
#Cost of any additional shore stations if shore>1
  output$shore2 <- renderText({
    if (input$shore > 1) {
    paste("Additional shore sites across waterbodies: $",
          format(s2(), big.mark = ",", scientific = FALSE))}
    else {
          "No additional shore stations"}
  })

#Cost of first boat station if input$boat>0
  output$boat1 <- renderText({
    if (input$boat > 0) {
    paste("First boat site across waterbodies: $",format(b1(), big.mark = ",", scientific = FALSE))}
    else {
            "No boat stations"}
  })

#Cost of any additional boat stations if input$boat>1
  output$boat2 <- renderText({
    if (input$boat > 1) {
    paste("Additional boat sites across waterbodies: $",format(b2(), big.mark = ",", scientific = FALSE))}
    else {
            "No additional boat stations"}
  })

#Summation of Field Costs    
output$FieldCostSum <- renderText({
  if (FieldCostSum() > 1) {
    paste("Sum of field costs: $", format(FieldCostSum(), big.mark = ",", scientific = FALSE))}
  else {
    "No field costs"}
})

#### WATER QUALITY COSTS ####
  
  output$waterquality <- renderText({
    if (length(input$wq > 1)) {
      paste("Sum of WQ analysis costs: $",format(w1(), big.mark = ",", scientific = FALSE))}
    else {
              "No Water Quality Costs"}
  })  
  
#### FHAB Analysis COSTS ####

#ELISA  
  output$ELISA <- renderText({
    if (length(input$ELISA > 1)) {
      paste("ELISA: $", format(e1(), big.mark = ",", scientific = FALSE))}
    else {
              "No ELISA costs"}
  })  


#qPCR - qPCR_prep is calculated for each sample, but there is an option for 4 analyses
#to be done on each extracted sample (qPCR_analysis)

#prep - cost per sample (maybe this is the extraction step?)
  output$qPCR_prep <- renderText({
    if (length(input$qPCR > 1)) {
      paste("qPCR prep: $", format(qp1(), big.mark = ",", scientific = FALSE))}
    else {
              "No qPCR prep costs"}
  })
  
#analysis - cost per sample and per number of toxin genes selected
  output$qPCR_analysis <- renderText({
    if (length(input$qPCR > 1)) {
      paste("qPCR analysis: $", format(qa1(), big.mark = ",", scientific = FALSE))}
    else {
              "No qPCR analysis costs"}
  })
  
#Cost of microscopy analysis  
 output$Microscopy <- renderText({
   if (length(input$micros > 1)) {
     paste("Microscopy ID: $", format(m1(), big.mark = ",", scientific = FALSE))}
   else {
             "No Microscopy ID costs"}
 }) 

#Summation of FHAB analysis costs 
 output$CyanotoxSum <- renderText({
   if (CyanotoxSum() > 0) {
     paste("Sum of FHAB analyses costs: $", format(CyanotoxSum(), big.mark = ",", scientific = FALSE))}
   else {
     "No FHAB analysis cost"}
   })

 
#Program Summary Costs

#Cost for each lake - all visits to shore/boat sites and all analyses
 output$siteCost <- renderText({
   if (siteCost() > 0) {
     paste("Cost for program at each waterbody: $", format(siteCost(), big.mark = ",", scientific = FALSE))}
   else{
     "Cannot calculate cost per site"
   }
 })
 
#Cost for each visit to each lake - shore/boat sites and all analyses of one visist
 output$visitCost <- renderText({
   if (siteCost() > 0) {
     paste("Cost for each waterbody visit: $", format(visitCost(), big.mark = ",", scientific = FALSE))}
   else{
     "Cannot calculate cost per site"
   }
   
 })

#Total program cost 
 output$program <- renderText({
   if (prog() > 0) {
     paste("Sum FHAB Monitoring Program Costs at", input$waterbodies, "waterbodies: $",
           format(prog(), big.mark = ",", scientific = FALSE))}
   else {
     "Cannot calculate program costs"}
 }) 
 
    
#Explanation tab output
 
output$table_costCalculations <- renderTable(explanation)    


#Plot
# Create a new reactive expression to be called below.

#create a list with the percent of total calculated for each category
Perc_cost <- reactive({ c(
  paste(round(((s1())/prog())*100, digits = 0), '%'),#s1
  paste(round(((s2())/prog())*100, digits = 0), '%'),#s2
  paste(round(((b1())/prog())*100, digits = 0), '%'),#s1
  paste(round(((b2())/prog())*100, digits = 0), '%'),#s2
  paste(round(((w1())/prog())*100, digits = 0), '%'),#s1
  paste(round(((e1())/prog())*100, digits = 0), '%'),#s2
  paste(round(((qp1())/prog())*100, digits = 0), '%'),#s1
  paste(round(((qa1())/prog())*100, digits = 0), '%'),#s2
  paste(round(((qa1())/prog())*100, digits = 0), '%')#m1
)
})

#Crate a list with the percent of total calculated for each broad category
Perc_cost_broad <- reactive({ c(
  paste(round(((FieldCostSum())/prog())*100, digits = 0), '%'),#fieldwork
  paste(round(((w1())/prog())*100, digits = 0), '%'),#water qual
  paste(round(((CyanotoxSum())/prog())*100, digits = 0), '%')#cyano

)
})

#Category labels for table - no line breaks
labs1 <- factor(c("First Shore St.", "Additional Shore Sts.", "First Boat St.", "Additional Boat Sts.", "Water Quality",
  "ELISA", "qPCR Prep", "qPCR Analysis", "Microscopy"),
  levels = c("First Shore St.", "Additional Shore Sts.", "First Boat St.", "Additional Boat Sts.", "Water Quality",
           "ELISA", "qPCR Prep", "qPCR Analysis", "Microscopy"))

#Category labels with line breaks
labs2 <- factor(c("First\nShore St", "Additional\nShore Sts.", "First\nBoat St.", "Additional\nBoat Sts.", "Water\nQuality",
                  "ELISA", "qPCR Prep", "qPCR\nAnalysis", "Microscopy"),
                levels = c("First\nShore St", "Additional\nShore Sts.", "First\nBoat St.", "Additional\nBoat Sts.", "Water\nQuality",
                           "ELISA", "qPCR Prep", "qPCR\nAnalysis", "Microscopy"))

#Broad category Labels for plot with line breaks
labs3 <- factor(c("Field\nWork", "Water\nQuality", "FHAB\nAnalyses"),
                levels = c("Field\nWork", "Water\nQuality", "FHAB\nAnalyses"))

#Create two data frames from reactive variables for plotting

#a sub categories
a_table <-   reactive ( {
  tibble(shore1=format(s1(), big.mark = ",", scientific = FALSE),
         shore2 =format(s2(), big.mark = ",", scientific = FALSE),
         boat1=format(b1(), big.mark = ",", scientific = FALSE),
         boat2=format(b2(), big.mark = ",", scientific = FALSE),
         waterqual = format(w1(), big.mark = ",", scientific = FALSE),
         ELISA = format(e1(), big.mark = ",", scientific = FALSE),
         qPCR_prep = format(qp1(), big.mark = ",", scientific = FALSE),
         qPCR_analysis = format(qa1(), big.mark = ",", scientific = FALSE),
         microscopy = format(m1(), big.mark = ",", scientific = FALSE),) %>%
  gather(factor_key = TRUE) %>% #change from wide to long format
  rename("Cost" = "value") %>%
    add_column(Percent = Perc_cost()) %>%  #Add calculated percentage column
    add_column(Category = labs1) %>% 
    select(Category, Cost, Percent) #Select call also allows definition of order of columns
} )

#Defined 'labs1' in tibble creation - no reason, but these took a lot of trial and
#error and came about the same solution two different ways
a <-   reactive ( {
  tibble("First Shore St."=s1(),
         "Additional Shore Sts." =s2(),
         "First Boat St."=b1(),
         "Additional Boat Sts."=b2(),
         "Water Quality" = w1(),
         "ELISA" = e1(),
         "qPCR Prep" = qp1(),
         "qPCR Analysis" = qa1(),
         "Microscopy" = m1()) %>%
    gather(factor_key = TRUE) %>%
    rename("Cost" = "value") %>%
    rename("Labels" = "key") %>% 
    add_column(Percent = Perc_cost()) %>% 
    filter(Cost > 0)
} )


#b broad categories
b_table <- reactive ( {
   tibble(FieldCostSum = format(FieldCostSum(), big.mark = ",", scientific = FALSE),
          waterqual = format(w1(), big.mark = ",", scientific = FALSE),
          CyanotoxSum = format(CyanotoxSum(), big.mark = ",", scientific = FALSE)) %>%
   gather(factor_key = TRUE)%>%
   rename("Cost" = "value") %>% 
    add_column(Percent = Perc_cost_broad()) %>% 
    add_column("Broad Category" = labs3) %>% 
    select("Broad Category", Cost, Percent)
 } )

b <- reactive ( {
  tibble(FieldCostSum = FieldCostSum() , waterqual = w1() , CyanotoxSum = CyanotoxSum()) %>%
    gather(factor_key = TRUE)%>%
    rename("Cost" = "value") %>%
    add_column(Percent = Perc_cost_broad()) %>%
    add_column(Component1 = labs3) %>%
    select(Component1, Cost, Percent)
} )


#reactive expression of total cost of program
# sum_total_cost = reactive ( {
#   sum(a()$Cost)
# } )


#Visualize total cost as stacked bar

p1 =
  reactive(
  ggplot()+
  #subdivisions  
  geom_bar(data=a(),aes(x=2, y=Cost, fill=fct_rev(Labels)),
           color='white', position='stack', stat='identity', size=0.6, alpha = 0.8) +
  geom_text(data=a(), aes(label=paste(Labels), x=1.6, y=Cost), hjust = 0,
            position=position_stack(vjust = 0.5), size=3)+
  #broad categories
  geom_bar(data=b(),aes(x=1, y=Cost, color=fct_rev(Component1)),
           color = 'white', position='stack', stat='identity', size=0.6, alpha = 0.3) +
  geom_text(data=b(), aes(label=paste(labs3), x=1, y=Cost), position=position_stack(vjust = 0.5),
            size=4)+
  #formatting
  theme_classic()+
  scale_fill_brewer(palette = "Spectral")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  labs(x="test", y="test",
       title =  paste("Program Cost: $", format(prog(), big.mark = ",", scientific = FALSE)))+
  theme(legend.title = element_blank())
  )

# #CREATE PLOT TAB OUTPUTS

output$table_a <- renderTable({a_table()}) 
output$table_b <- renderTable({b_table()}) 

output$plot1 <- renderPlot({p1()})

} #close out server script


# Run the application (knit ui and server together)
shinyApp(ui = ui, server = server)
