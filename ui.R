#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)
library(highcharter)
library(shinythemes)

# Define UI for application 
ui <- fluidPage(theme=shinytheme("flatly"),
                navbarPage(
                  "US Mass Shooting",
                  # Create description panel, incl. text output and file input functionality
                  tabPanel("Description", h4('Analysing mass shooting in the United States'), textOutput("text"), hr()),
                  
                  # Create input choice for map
                  tabPanel("Interactive map", leafletOutput("map"), hr(),fluidRow(
                    column(3, selectInput(
                      "Cluster",
                      "Cluster or not",
                      c("Cluster"=1,"Not Cluster"=0),
                      selected = 0,
                    )),
                    column(4,offset=1,checkboxGroupInput(
                      "Year",
                      "Years of mass shootings:",
                      c("1966-1975"=1,"1976-1985"=2,"1986-1995"=3,"1996-2005"=4,"2006-2017"=5),
                      selected = c(1:5)
                    )),
                    column(4,checkboxGroupInput(
                      "Gender", 
                      "Shooter's Gender",
                      c("Female"="F","Male"="M","Female & Male"="M/F","Unknown"="Unknown"),
                      selected = c("F","M","M/F","Unknown")
                      
                    ))
                    
                  )),
                  #tabPanel("Heat map",plotOutput("Heat")),
                  tabPanel("Pyramid graph",highchartOutput("Pyramid")),
                  #create tabs for plots             
                  tabPanel("Plots",
                           tabsetPanel(
                             tabPanel("victims", 
                                      sidebarLayout(sidebarPanel(selectInput("victims","Victims:",
                                                                             c("Fatalities"=1,"Injured"=2,"Total_victims"=3,"Fatalities & Injured"=4),
                                                                             selected=1),selectInput("plotchoice","Plot choices:",c("Bar"=1,"Point"=2),
                                                                                                     selected=1)),mainPanel(plotOutput("plot1")))),
                             tabPanel("Frequency", 
                                      sidebarLayout(sidebarPanel(selectInput("plotchoice2","Plot choices:",c("Bar"=1,"Point"=2),
                                                                             selected=1)),mainPanel(plotOutput("plot2")))),
                             tabPanel("Heat map",plotOutput("Heat"))
                             
                           )),
                  # Create tab for table
                  tabPanel("Table", fluidRow(
                    column(4,selectInput("state","Select states:",c(state.name, 'District of Columbia'), multiple=TRUE)),
                    column(4,offset=2,dateRangeInput("date","Inpute date range:",start="1966-01-01",end = "2017-12-31",min="1966-01-01",max="2017-12-31",format = "yyyy-mm-dd")),
                    
                    dataTableOutput("table")
                  ))
                  
                  
                ))
