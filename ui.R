
library(shiny)
library(shinythemes)

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
                               # Inputs excluded for brevity
                          ),
                          mainPanel(
                              tabsetPanel(
                                 tabPanel("Plot", plotOutput("plot")), 
                                 tabPanel("Summary", verbatimTextOutput("summary")), 
                                 tabPanel("Table", tableOutput("table"))
                               )
                             )
                           )
                  ),
                  tabPanel("Multivariate Summary",
                      plotOutput("plot_summary")
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
                 tabPanel("Model Fitting", plotOutput("table4")),
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



