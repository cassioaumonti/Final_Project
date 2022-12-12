# Final Project by Cassio Monti

This shiny app is a requirement from ST558 as final project.

The purpose of this app is to provide a basic exploratory data analysis (EDA) and modeling using a multiple linear regression, regression tree, and random forest models. The app has guidance tabs that will lead the user from the general information about the app, data set, and models to the final predictions made on the best model chosen by the user dynamically.

In order to run this app, the following packages are required:

```{r}

library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(GGally)
library(shinyWidgets)
library(caret)
library(DT)

```

RStudeo has a nice feature that allows the user to run the shiny app through GitHub via the following code:

```{r}
shiny::runGitHub('Final_Project','cassioaumonti', ref="main")
```

The code below checks if the user has the required packages to run the app and install the ones that are not installed in their local machine. Lastly, the code run the previous chunk and calls the shiny app from GitHub. Pretty nice, isn't it?!

```{r}

list.of.packages <- c("shiny","shinythemes","tidyverse","GGally","shinyWidgets","caret","DT","shinyjs","shinydashboard")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

shiny::runGitHub('Final_Project','cassioaumonti', ref="main")
```
