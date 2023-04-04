#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Name: Anoop Nair
#Module: Intro to R
#Student Id: R00223644 

######## set directory  #######

setwd("D:/STUDY MATERIAL/Masters DS & Analytics/Intro to R/Project/")
#getwd()

####### Install and load required packages #######
library(shiny)
library (ggplot2)
library(plotly)
library(tidyr)
library(pastecs)
library(tidyverse)
library(psych)
library(plyr)
library(e1071)
library(readxl)
library(lubridate)
library(anytime)
library(gganimate)
library(tibble)
library(formattable)
library(gridExtra)
library(GGally)
library(stats)
library(dplyr)
library(shiny)
library(DT)
library(ggpubr)


######### Loading and cleaning the dataset ######

Co2data=read.csv("STAT8010_2022_assignment2_2022.csv")
head(Co2data)
str(Co2data)

#### change the long names  to concise and relevant ones ####

colnames(Co2data)=c('Make','Model','Class','Enginesize','Cylinder','Transmissiontype',
                    'Fueltype','Fuelconsumption_city','Fuelconsumption_highway','Fuelconsumption_both','Milespergallon_both','Co2emmissions')


######### collapsing levelS Of Class of vehicle  #############

Co2data$Class[grepl("SUV", Co2data$Class)]="SUV"
Co2data$Class[grepl("PICKUP", Co2data$Class)]="PICKUP"
Co2data$Class[grepl("VAN", Co2data$Class)]="VAN"
Co2data$Class[grepl("COMPACT", Co2data$Class)]="COMPACT"
Co2data$Class[grepl("SPECIAL", Co2data$Class)]="SPECIAL"
Co2data$Class[grepl("TWO-SEATER", Co2data$Class)]="SPORT"


######### CATEGORIZING OTHER CLASS VALUES INTO TOURISM ##########

Co2data$Class[grepl("STATION WAGON - MID-SIZE", Co2data$Class)]="TOURISM"
Co2data$Class[grepl("STATION WAGON - SMALL", Co2data$Class)]="TOURISM"
Co2data$Class[grepl("MID-SIZE", Co2data$Class)]="TOURISM"
Co2data$Class[grepl("FULL-SIZE", Co2data$Class)]="TOURISM"

######### Converting levelS Of Class of vehicle  #############


Co2data$Transmissiontype[grepl("A", Co2data$Transmissiontype)]="Automatic"
Co2data$Transmissiontype[grepl("M", Co2data$Transmissiontype)]="Manual"


####### Update gasoline #####


Co2data$Fueltype[Co2data$Fueltype=="X"]="Regular gasoline"
Co2data$Fueltype[Co2data$Fueltype=="Z"]="Premium gasoline"
Co2data$Fueltype[Co2data$Fueltype=="D"]="Diesel"
Co2data$Fueltype[Co2data$Fueltype=="E"]="Ethanol"
Co2data$Fueltype[Co2data$Fueltype=="N"]="Natural Gas"



########## convert certain variables into factors 


Co2data$Model=as.factor(Co2data$Model)
Co2data$Make=as.factor(Co2data$Make)
Co2data$Class=as.factor(Co2data$Class)
Co2data$Cylinder=as.factor(Co2data$Cylinder)
Co2data$Fueltype=as.factor(Co2data$Fueltype)
Co2data$Transmissiontype=as.factor(Co2data$Transmissiontype)

colmn=colnames(Co2data)

############# Consider only Factor variables #######

Columnfactor=c('Class','Cylinder','Fueltype','Transmissiontype','Please select')


########################  APP Data ###########################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Dashboard for Co2 Emissions of Vehicle"),
 #   checkboxInput("toggle", "toggle"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel
      ( width=4,
        selectInput
        (
          "Var1",
          label = tags$span(style="color: blue;","Select variable 1"),
          choices = colmn ### for var1 selection
        ),
        selectInput
        (
          "Var2",
          label =  tags$span(style="color: blue;","Select variable 2"),
          choices = colmn  ### for var2 selection
        ),
        
        selectInput
        (
          "radio",  ### To warn before selecting regression line checkbox
          label = tags$span(style="color: Green;","Color Scatter plot by Factor
                  (**Please reset to 'Please select' before selecting Regression line checkbox)"),
          choices = Columnfactor,selected = "Please select"
        ),
        
        
        checkboxInput('Regline','Do you want to fit a model'), #check box to fir the regression line
        span(textOutput("numvar3"), style="color:red") # to display text in Var3
      ),
        

        # specifying different tabs of the dashboard
        mainPanel(
                    tabsetPanel(
                     tabPanel("Main Page",plotOutput("graphs")),
                     tabPanel("Summary",verbatimTextOutput("summm"),verbatimTextOutput("summm1")),
                     tabPanel("Histogram",span(textOutput("numvar1"), style="color:red"),span(textOutput("numvar2"), style="color:red"),
                              plotOutput("histo1"),plotOutput("histo2")
                              )
                     )

            )
     
          
        )
    
    
    )

# Define server logic required to draw a histogram

server <- function(input, output) 
  {
  
  ####### Histogram message
  
  output$numvar1 <- renderText({
    if (is.numeric(Co2data[,c(input$Var1)])==TRUE){ paste("")}
    else { paste("**Please Select Numeric Variable1 for Histogram ") } # warning message incase Histogram 1 is not available
  })

  output$numvar2 <- renderText({
    if (is.numeric(Co2data[,c(input$Var2)])==TRUE){ paste("")}
    else { paste("**Please Select Numeric Variable2 for Histogram ") } # Warning message incase Histogram 2 is not available
  })
  
  ######## Linear Model Message
  output$numvar3 <- renderText({
    if (is.numeric(Co2data[,c(input$Var1)])==TRUE & is.numeric(Co2data[,c(input$Var2)])==TRUE){ paste("")}
    else { paste("**Both Var1 and Var2 has to be numeric for fitting the Regression Line") }  # warning message before fitting regression line
  })
  
  
  #c('Class','Cylinder','Fueltype','Transmissiontype')
    output$graphs <- renderPlot(
      {
      
        # generate bins based on input$bins from ui.R
        col1=input$Var1
        col2=input$Var2
        titlename=paste(' between ',input$Var1 ,' & ',input$Var2) # combining variables for having the title for the graph
        x=Co2data[,col1]
        y=Co2data[,col2]


        
        if ( (is.numeric(x)==TRUE) & (is.numeric(y)==TRUE))
        {
          
          titlename=paste('Scatterplot',titlename)
          
          if (input$radio!="Please select")
          {
            ########### to plot scatter plot with factor colors
            FactorColors=Co2data[,input$radio]
            
            p=ggplot(Co2data)+ aes(x =x , y=y,color=FactorColors) +
              geom_point() + 
              xlab(input$Var1)+ylab(input$Var2)+ggtitle(titlename)+
              theme(text = element_text(size = 20)) 
            p
          }
          else
          {
            ############## to plot scatter plot without factor colors ##########
            p=ggplot(Co2data)+ aes(x =x , y=y) +
              geom_point() + 
              xlab(input$Var1)+ylab(input$Var2)+ggtitle(titlename)+
              theme(text = element_text(size = 20)) 
 
            p
            
          }

        
            ################### for adding regression line ###################            
          
          if (input$Regline){
            
            p=p+geom_smooth(method="lm")+
              stat_regline_equation(label.x=max(x)*0.2, label.y=max(y)*0.95,cex=8)+ 
              stat_cor(aes(label=..rr.label..), label.x=max(x)*0.2, label.y=max(y)*0.9,cex=8)
            p
            }
          else {
            p
          }
  
          
          
          
        }
        else if ((is.numeric(x)==FALSE) & (is.numeric(y)==FALSE))
        {
          
          ########### to add Bar plot #########
          titlename=paste('Barplot',titlename)
          barplot(prop.table(table(x,y))*100,
                  col=1:nrow(Co2data)
                  ,xlab=input$Var2,ylab="Percentage",main=titlename,legend.text = TRUE,cex.main=2,cex.lab=1.5)
          
          
        }
        else
        {
          if (is.numeric(x)==FALSE){
           ######### to add boxplot ######### 

            titlename=paste('Boxplot',titlename)
            boxplot(y~x,xlab = input$Var1,ylab=input$Var2,main=titlename,cex.main=2,cex.lab=1.5,col="light pink")
            
          }
          else{

            titlename=paste('Boxplot',titlename)
            boxplot(x~y,xlab = input$Var2,ylab=input$Var1,main=titlename,cex.main=2,cex.lab=1.5,col="light pink")
            
            
          }
          
          
        }

        
    })
    
########### summary output of the enttire data set in the second tab ########
    
    output$summm=renderPrint({
    summary(Co2data)
      
    })

########## Tab 3 Histograms for var1 and var 2 incase of numerical variables ##########

    output$histo1 <- renderPlot(
      {
        titlename=paste('Histogram of ',input$Var1)
        if (is.numeric(Co2data[,c(input$Var1)])==TRUE ){
        
        hist(Co2data[,c(input$Var1)],xlab = input$Var1,main=titlename,col="light pink")
          
        }
        else {  print("") }
          
          
          
        }

        
    )


    
    
    output$histo2 <- renderPlot(
      {
        
        titlename=paste('Histogram of ',input$Var2)
        if (is.numeric(Co2data[,c(input$Var2)])==TRUE){
          
          hist(Co2data[,c(input$Var2)],xlab = input$Var2,main=titlename,col="light pink")
          
        }
        else{
          print("")
        }
        
      })
    
    
  }

# Run the application 
shinyApp(ui = ui, server = server)
