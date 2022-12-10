# Final_Project

List of used packages:

```{r}

library(shiny)
library(shinythemes)
library(tidyverse)
library(GGally)
library(shinyWidgets)
library(caret)
library(DT)

```

Code to run the shiny app through GitHub

```{r}
shiny::runGitHub('Final_Project','cassioaumonti', ref="main")
```

Run the code below and all required packages will be installed if they are not already in your local machine and the app will automatically be opened.
```{r}

list.of.packages <- c("shiny","shinythemes","tidyverse","GGally","shinyWidgets","caret","DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

shiny::runGitHub('Final_Project','cassioaumonti', ref="main")
```
