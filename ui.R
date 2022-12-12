
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(GGally)
library(shinyWidgets)
library(caret)
library(DT)

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
                                         label="More information about the data",
                                         icon=icon('circle-question'),
                                         class="down",
                                        onclick= "window.open('https://archive-beta.ics.uci.edu/dataset/53/iris')"),
                         br(),
                         img(class="img-polaroid",
                             src="iris.jpg"),
                         tags$small(
                           "Source: Famous data set from Fisher, 1936.",
                           a(href="https://archive.ics.uci.edu/ml/datasets/iris",
                             "User:CassioMonti")
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
                      withMathJax(),
                      fluidRow(
                        column(6,
                               h1("Multiple Linear Regression"),
                               box(width=12,
                                 helpText("Linear regression is a famous modeling technique which attempts to capture the average response of a particular dependent variable of interest via a straigh line. In the framework of a multiple linear regression, the expected response can be modeled as:"),
                                 helpText("$$E(Y|X_1,\\cdots,X_p)=\\beta_0+\\beta_1X_1+\\cdots+\\beta_pX_p$$"),
                                 helpText("Where X's are the predictor variables, Y is the response variable and \\(\\beta\\)'s are the parameters to be estimated from the data."),
                                 helpText("The multiple linear regression model assumes the form below when the parameters are estimated using the Least Squares Method:"),
                                 helpText("$$\\hat{Y}=\\hat{\\beta_0}+\\hat{\\beta_1}X_1+\\cdots+\\hat{\\beta_p}X_p$$"),
                                 helpText("The general matricial form of the Least Squares Methods can be see below:"),
                                 helpText("$$\\hat{\\beta}=(X^TX)^{-1}X^TY$$"),
                                 helpText("Where \\(X^T\\) is the transposed design matrix of predictors, \\(X\\) is the design matrix of predictors, and \\(Y\\) is the response vector."),
                                 helpText("The major benefit of linear regression is that the coefficients are of ease interpretation and it is relativelly simple to estimate them when the response follows the normal distribution, mainly because the coefficients present a closed form of estimation, as shown above.
                                          When the response variable follows the normal distibution, the Gauss-Markov Theorem says that the Least Squares Method is similar to the Maximum Likelihood Estimator (MLE), which is an important and reliable estimator since it considers the conditional ditributions of the data. Besides, MLE has some nice asymptotic properties."),
                                 helpText("The main drawback of linear regression is the assumptions one must make in order to use all the theory behind the method. The assumptions are:"),
                                 helpText("$$\\epsilon \\overset{iid}{\\sim} N(0,1)$$"),
                                 helpText("In other words, the errors should be independent and identically distributed, following normal distribution with mean zero and variance 1. In real applications, this setting is not always the case, so more sofisticated models are necessary.")
                                )
                        ),
                        column(6,
                          h1("Tree Based Methods"),
                          h2("Regression Tree & Random Forest"),
                          box(width=12,
                                 helpText("The controls for the app are located to the left and the visualizations are available on the right."),
                                   
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
                              h2("Distribution of the Selected Split for Sepal.Length"),
                              plotOutput("split_plot")
                            )
                          )
                      ),
                      tabPanel("Model Settings",
                        titlePanel("Select the Predictors for each model"),
                        sidebarLayout(
                          sidebarPanel(
                              radioButtons(inputId = "rd_but",
                                      label = "Have you chosen the desired predictors for all models?",
                                      choices = c("Yes", "No"), selected = "No"),
                              conditionalPanel(condition = "input.rd_but == 'Yes'",
                                  actionButton(inputId = "run_mods",
                                               label = "Run Models")
                              )
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(title = "Multiple Linear Regression",
                                         sidebarLayout(
                                           sidebarPanel(
                                             checkboxGroupInput(inputId = "vars_mod1",
                                                                label = "Select the Predictor Variables",
                                                                choices = names(df[,-c(1,5)])
                                             )
                                           ),
                                           mainPanel(
                                             tabsetPanel(
                                               tabPanel(title = "Training Error Plot",
                                                        plotOutput("train_mod1")
                                               ),
                                               tabPanel(title = "Training Error Summary",
                                                        verbatimTextOutput("summary_mod1")
                                               )
                                             )
                                           )
                                         )
                                ),
                                tabPanel(title = "Regression Tree",
                                         sidebarLayout(
                                           sidebarPanel(
                                             checkboxGroupInput(inputId = "vars_mod2",
                                                                label = "Select the Predictor Variables",
                                                                choices = names(df[,-c(1,5)])
                                             )
                                           ),
                                           mainPanel(
                                             tabsetPanel(
                                               tabPanel(title = "Training Error Plot",
                                                        plotOutput("train_mod2")
                                               ),
                                               tabPanel(title = "Training Error Summary",
                                                        verbatimTextOutput("summary_mod2")
                                               )
                                             )
                                           )
                                         )
                                ),
                                tabPanel(title = "Random Forest",
                                         sidebarLayout(
                                           sidebarPanel(
                                             checkboxGroupInput(inputId = "vars_mod3",
                                                                label = "Select the Predictor Variables",
                                                                choices = names(df[,-c(1,5)])
                                             )
                                           ),
                                           mainPanel(
                                             tabsetPanel(
                                               tabPanel(title = "Training Error Plot",
                                                        plotOutput("train_mod3")
                                               ),
                                               tabPanel(title = "Training Error Summary",
                                                        verbatimTextOutput("summary_mod3")
                                               )
                                             )
                                           )
                                         )
                                )
                            )
                          )
                          
                        )
                      ),
                      tabPanel("Test Set Error Metrics",
                               tabsetPanel(
                                 tabPanel("Multiple Linear Regression",
                                      verbatimTextOutput("test_error_mlr_summ")
                                      
                                 ),
                                 tabPanel("Regression Tree",
                                      verbatimTextOutput("test_error_rt_summ")
                                 ),
                                 tabPanel("Random Forest",
                                      verbatimTextOutput("test_error_rf_summ")
                                 )
                               )
                      ),
                      tabPanel("Best Model",
                          verbatimTextOutput("best_model_choice"),
                          radioButtons(inputId = "choose",
                                       label = "Choose YOUR best model below",
                                       choices = c("Multiple Linear Regression"=1,
                                                   "Regression Tree"=2,
                                                   "Random Forest"=3),
                                       selected = 4),
                          actionButton(inputId = "sub_but",
                                       label = "Submit Best Model's Choice"),
                          useShinyjs(),
                          conditionalPanel(condition = "input.sub_but==1",
                                           uiOutput("text")
                                           )
                      )
                    )
                ),
                 tabPanel("Prediction",
                    sidebarLayout(
                      sidebarPanel(
                        useShinyjs(),
                        actionButton(inputId = "reset_preds",
                                     label = "Reset Predictor Variables"),
                        numericInput(inputId = "Sepal.Width",
                            label = "Sepal.Width",
                            value = 0),
                        numericInput(inputId = "Petal.Length",
                            label = "Petal.Length",
                            value = 0),
                        numericInput(inputId = "Petal.Width",
                            label = "Petal.Width",
                            value = 0),
                        # selectInput(inputId = "Species",
                        #     label = "Species",
                        #     choices = list("setosa"=1,"versicolor"=2,"virginica"=3),
                        #     selected = 1),
                        
                        actionButton(inputId = "run_pred",
                                   label = "Predict")
                        ),
                      mainPanel(
                          h3("The predicted value is:"),
                          verbatimTextOutput("text_pred")
                        )
                      )
                    )
                 )
           ),
           tabPanel("Data",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("type_select", "What do you want to select?",
                                     c("Rows" = "Rows",
                                       "Columns" = "Columns")),
                        conditionalPanel(
                          condition = "input.type_select == 'Columns'",
                          uiOutput("picker"),
                          actionButton("view", "View Selection")        
                        )
                        
                      ),
                      mainPanel(
                        DT::dataTableOutput("table"),
                      )
                    )
           )
          # ,
          #  navbarMenu(title="More",
          #             tabPanel("Data Science Blog", icon = icon("blog"),
          #                      verbatimTextOutput("Link")),
          #             tabPanel("GitHub Page", icon = icon("github"),
          #                      verbatimTextOutput("test2")),
          #             tabPanel("Contact", icon = icon("envelope"),
          #                      verbatimTextOutput("Contact"))
          #  )
)



