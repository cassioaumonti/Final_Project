
library(shiny)
library(shinythemes)
library(tidyverse)
library(GGally)

df = iris
var_init = names(df)

navbarPage("Monti's App", theme = shinytheme("flatly"),
           tabPanel(title="About", icon = icon("house"),
              fluidRow(
                  column(6,
                    h3("Welcome to Monti's App", style="margin-top:0px;"),
                    br(),
                    includeMarkdown("about.md")
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
                            selectInput(label="Select Summary Type", inputId = "summ_type",
                                        choices=c("Contingency Table"=1,
                                                  "Continuous Summary"=2),selected = 2),
                               selectInput(label="Select Plot Type",inputId = "plot_type",
                                    choices = c("Empirical CDF"=1, "Histogram"=2,
                                    "BoxPlot"=3, "Scatter"=4), selected = 2),
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
                                 tabPanel("Summary Table", verbatimTextOutput("summary_uni"))
                               )
                             )
                           )
                  ),
                  tabPanel("Multivariate Summary",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(label="Select Plot Type",inputId = "plot_type2",
                                      choices = c("Scatter Plot"=1, "Biplot"=2,
                                                  "Combo Plot"=3), selected = 1),
                          varSelectInput(inputId = "var_multi_plot1",
                                         "Select Response Variable:", Filter(is.numeric,df),
                                         selected = var_init[1]),
                          varSelectizeInput(inputId = "var_multi_plot2",
                                            "Select Independent Variable:",
                                            Filter(is.numeric,df),
                                            selected = var_init[2]),
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
                            includeMarkdown('model_info.md')
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
                          tabPanel(title = "Multiple Linear Regression",
                             sidebarLayout(
                               sidebarPanel(
                                 checkboxGroupInput(inputId = "vars_mod1",
                                     label = "Select the Predictor Variables",
                                     choices = names(Filter(is.numeric,df[,-1]))
                                  ),
                                 checkboxInput(inputId = "prepros1",
                                               label = "Standardize Data?",
                                               value = TRUE)
                               ),
                               mainPanel(
                                 tabPanel(title = "Training Error Plot",
                                    plotOutput("train_mod1")
                                 ),
                                 tabPanel(title = "Training Error Summary",
                                    verbatimTextOutput("summary_mod1")
                                 )
                               )
                             )
                             
                          ),
                          tabPanel(title = "Regression Tree",
                                   sidebarLayout(
                                     sidebarPanel(
                                       checkboxGroupInput(inputId = "vars_mod2",
                                            label = "Select the Predictor Variables",
                                            choices = names(Filter(is.numeric,df[,-1]))
                                       ),
                                       checkboxInput(inputId = "prepros2",
                                                     label = "Standardize Data?",
                                                     value = TRUE),
                                       numericInput(inputId = "tun1_mod2",
                                                    label = "Tuning Parameter 1",
                                                    value = 0.7, min = 0.1, max = 0.9)
                                     ),
                                     mainPanel(
                                       tabPanel(title = "Training Error Plot",
                                                plotOutput("train_mod2")
                                       ),
                                       tabPanel(title = "Training Error Summary",
                                                verbatimTextOutput("summary_mod2")
                                       )
                                     )
                                   )
                          ),
                          tabPanel(title = "Random Forest",
                                   sidebarLayout(
                                     sidebarPanel(
                                       checkboxGroupInput(inputId = "vars_mod3",
                                                          label = "Select the Predictor Variables",
                                                          choices = names(Filter(is.numeric,df[,-1]))
                                       ),
                                       checkboxInput(inputId = "prepros3",
                                                     label = "Standardize Data?",
                                                     value = TRUE),
                                       numericInput(inputId = "tun1_mod3",
                                                    label = "Max Value for Tuning Parameter",
                                                    value = 0.7, min = 0, max = 100),
                                       numericInput(inputId = "tun2_mod3",
                                                    label = "Min Value for Tuning Parameter",
                                                    value = -0.7, min = -100, max = 0)
                                     ),
                                     mainPanel(
                                       tabPanel(title = "Training Error Plot",
                                                plotOutput("train_mod3")
                                       ),
                                       tabPanel(title = "Training Error Summary",
                                                verbatimTextOutput("summary_mod3")
                                       )
                                     )
                                   )
                          )
                        ),
                        actionButton(inputId = "run_mods",
                                     label = "Run Models")
                      )
                    )
                ),
                 tabPanel("Prediction",plotOutput("table3")),
               )
           ),
           tabPanel("Data",
               DT::dataTableOutput("dynamic_table")
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



