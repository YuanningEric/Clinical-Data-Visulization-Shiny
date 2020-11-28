#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(navbarPage(theme = shinytheme("flatly"), "Acupuncture Study",
    tabPanel("Data Exploration",
             h4("Raw Data Input:"),
             DT::dataTableOutput("raw_data"),
             h4("Processed Data:"),
             DT::dataTableOutput("processed_data")
             ),
    
    tabPanel("Patient Distributions",
         h4("Sex Distribution:"),
         fluidRow(
             column(3, offset =1,  h5("Table:"), tableOutput("sex_count"), h5("Chi-squared test:"), tableOutput("sex_chi")),
             column(7, offset =1,  plotlyOutput("sex_dist"))
         ),
         hr(),
         h4("Age Distribution:"),
         selectInput(
             "plotType", "Plot Type",
             c(Box = "box", Bar = "bar"),
             selected = "violin"
             ),
         
         fluidRow(
             column(4, offset =0, h5("Table:"), tableOutput("age_count"), h5("Test:"), tableOutput("age_chi")),
             column(7, offset =1,  plotlyOutput("age_dist"))
             ),
    
         hr(),
         h4("Baseline Score Distribution:"),
         fluidRow(
             column(4, h5("t-test:"), tableOutput("baseline_tbl")),
             column(7, offset =1,   plotlyOutput("baseline_dist"))
         )
        ),
    
    navbarMenu("Treatment Results",
         tabPanel("Histogram",
                  sidebarLayout(
                      sidebarPanel(width = 3,
                          radioButtons("sex_select", "Select Population:", c("male", "female", "all"),
                                       selected = "all",
                                       inline = TRUE
                          ),
                          sliderInput("MaxNum",
                                      "Range:",
                                      min = 1,
                                      max = 100,
                                      value = 100
                          ),
                      ),
                      # Show a plot of the generated distribution
                      mainPanel(width = 9,
                          h4("Distribution of Score Change:"),
                          plotlyOutput("pct_histo"),
                      )
                    )
         ),
         tabPanel("BoxPlot",
                  sidebarLayout(
                      sidebarPanel(width = 3,
                          radioButtons("sex_box", "Select Population:", c("male", "female", "all"),
                                       selected = "all",
                                       inline = TRUE
                          ),
                          
                          selectInput("age_group", "Age Group:", c("18-34", "35-44", "45-54", "55-65", "all"),
                                      selected = "all"
                          ),
                          
                          radioButtons("t_method", "Method for t test:", c("one-sided", "two-sided"),
                                       selected = "two-sided",
                                       inline = TRUE
                          )
                      ),
                      mainPanel(width = 9,
                          h4("Pencentage change of headache scores:"),
                          plotlyOutput("pct_box"),
                          br(),
                          h4("t-test for pencentage change of headache scores:"),
                          DT::dataTableOutput("t_test"),
                          br(),
                      )
                  )
        ),
        tabPanel("Linear Regression",
                 sidebarLayout(
                     sidebarPanel(width = 3,
                          radioButtons("sex_lin", "Select Population:", c("male", "female", "all"),
                                       selected = "all",
                                       inline = TRUE
                          ),
                          
                          selectInput("age_lin", "Age Group:", c("18-34", "35-44", "45-54", "55-65", "all"),
                                      selected = "all"
                          ),
                          selectInput("x_var", "Variable X:", c("age", "baseline score"),
                                      selected = "baseline score"
                          ),
                          selectInput("y_var", "Variable y:", c("baseline score", "percentage change from baseline"),
                                      selected = "percentage change from baseline"
                          ),
                     ),
                     mainPanel(width = 9,
                         h4("Compute Parameters for Linear Regression:"),
                         verbatimTextOutput("summary"),
                         plotlyOutput("lin_reg") 
                     )
                 )
        )
    )
))
