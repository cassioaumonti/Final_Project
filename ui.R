
library(shiny)
library(shinythemes)
library(tidyverse)

df = iris

navbarPage("Monti's App", theme = shinytheme("flatly"),
           tabPanel(title="About", icon = icon("house"),
              fluidRow(
                  column(6,
                    h3("Welcome to Monti's App", style="margin-top:0px;"),
                  ),
                  column(3,actionButton("btn_landing",
                                         label="Info: To know more about the data",
                                         icon=icon('circle-question'),
                                         class="down"),
                         br(),
                         img(class="img-polaroid",
                             src=paste0("http://upload.wikimedia.org/",
                                        "wikipedia/commons/9/92/",
                                        "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                         tags$small(
                           "Source: Photographed at the Bay State Antique ",
                           "Automobile Club's July 10, 2005 show at the ",
                           "Endicott Estate in Dedham, MA by ",
                           a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                             "User:Sfoskett")
                        )
                  )
              )
          ),
           tabPanel("Data Exploration",
              navbarPage(title = "EDA Features",
                  tabPanel("Univariate Summary",
                      sidebarLayout(
                          sidebarPanel(
                               selectInput(label="Select Plot Type",inputId = "plot_type",
                                    choices = c("Empirical CDF"=1, "Histogram"=2,
                                    "BoxPlot"=3), selected = 2),
                               selectInput(label="Select Summary Type", inputId = "summ_type",
                                           choices=c("Contingency Table"=1,
                                                     "Continuous Summary"=2),selected = 2),
                               varSelectInput(inputId = "var_uni_plot", 
                                              "Select Response Variable (Continuous):", 
                                              Filter(is.numeric,df)),
                               varSelectInput(inputId = "aux_var_uni",
                                              "Select Auxiliary Variable (Factor):",
                                              Filter(is.factor,df))
                          ),
                          mainPanel(
                              tabsetPanel(
                                 tabPanel("Plot", plotOutput("plot_uni")), 
                                 tabPanel("Summary Table", tableOutput("summary_uni"))
                               )
                             )
                           )
                  ),
                  tabPanel("Multivariate Summary",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(label="Select Plot Type",inputId = "plot_type2",
                                      choices = c("Scatter Plot"=1, "Biplot"=2,
                                                  "BoxPlot"=3), selected = 1),
                          varSelectInput(inputId = "var_multi_plot1",
                                         "Select Response Variable:", Filter(is.numeric,df)),
                          varSelectizeInput(inputId = "var_multi_plot2",
                                            "Select Independent Variable:",
                                            Filter(is.numeric,df)),
                          varSelectInput(inputId = "aux_var_multi",
                                         "Select Auxiliary Variable (Factor):",
                                         Filter(is.factor,df))
                        ),
                        mainPanel(
                          plotOutput("plot_multi")
                        )
                      )
                  )
              )
           ),
           tabPanel("Modeling",
              navbarPage("Modeling Features",
                  tabPanel("Modeling Info",
                      fluidRow(
                        column(6,
                            includeMarkdown('about.md')
                        ),
                        column(3,
                          img(class="img-polaroid",
                            src=paste0("http://upload.wikimedia.org/",
                                "wikipedia/commons/9/92/",
                                "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                            tags$small(
                                "Source: Photographed at the Bay State Antique ",
                                "Automobile Club's July 10, 2005 show at the ",
                                "Endicott Estate in Dedham, MA by ",
                                a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                "User:Sfoskett")
                            )
                        )
                      )
                 ),
                 tabPanel("Model Fitting", 
                    navlistPanel(
                      tabPanel("Data Splitting", 
                          sidebarLayout(
                            sidebarPanel(
                              numericInput(inputId = "split",label = "Splitting Proportion",
                                           value = 0.7, min = 0.1, max = 0.9)
                            ),
                            mainPanel(
                              plotOutput("split_plot")
                            )
                          )
                      ),
                      tabPanel("Model Settings", 
                        tabsetPanel(
                          tabPanel(title = "Multiple Linear Regression"),
                          tabPanel(title = "Regression Tree"),
                          tabPanel(title = "Random Forest")
                          
                        )
                      )
                    )
                    
                    
                ),
                 tabPanel("Prediction",plotOutput("table3")),
               )
           ),
           tabPanel("Data",
             tabsetPanel(
               tabPanel("Filter", plotOutput("plot33")),
               tabPanel("download",plotOutput("plot55"))
             )
           ),
           navbarMenu(title="More",
                      tabPanel("Data Science Blog", icon = icon("blog"),
                               verbatimTextOutput("Link")),
                      tabPanel("GitHub Page", icon = icon("github"),
                               verbatimTextOutput("test2")),
                      tabPanel("Contact", icon = icon("envelope"),
                               verbatimTextOutput("Contact"))
           )
)



