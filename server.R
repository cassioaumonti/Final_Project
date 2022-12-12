
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot_uni <- renderPlot({
      
      if(input$plot_type == 1){
        ggplot(df) + stat_ecdf(aes(x = get(input$var_uni_plot))) + 
          labs(x = input$var_uni_plot, y = "Probability")
        
      }else if(input$plot_type == 2){
        ggplot(df) + geom_histogram(aes(x = get(input$var_uni_plot), 
                                fill = as.factor(get(input$aux_var_uni)))) + 
          labs(x = input$var_uni_plot, y = "Frequency") + 
          scale_fill_discrete(input$aux_var_uni)
        
      }else if(input$plot_type == 3){
        ggplot(df) + geom_boxplot(aes(y = get(input$var_uni_plot),
                                fill = as.factor(get(input$aux_var_uni)))) + 
          labs(x = input$var_uni_plot, y=" ") + scale_fill_discrete(input$aux_var_uni)
        
      }else{
        ggplot(df) + geom_point(aes(y = get(input$var_uni_plot),
                                    x = 1:nrow(df), color = get(input$aux_var_uni))) + 
          labs(x = " ", y = input$var_uni_plot) + scale_color_discrete(input$aux_var_uni)
      }
      
    })

    output$summary_uni <- renderPrint({
      
      v = df[,as.character(input$var_uni_plot)]
      a = df[,as.character(input$aux_var_uni)]
      
      cutoff = cut(v, 
                   breaks = c(min(v), 
                              mean(v),
                              max(v)),
                   labels = c(paste0("(",round(min(v),2),
                                     ", ",round(mean(v),2),
                                     "]"),
                              paste0("(",round(mean(v),2),
                                     ", ",round(max(v),2),
                                     "]"))
      )

      if(input$summ_type == 1){
        
        table(as.factor(a), cutoff)
        
      }else{
        
        f = data.frame(v=v,a=a)
        
        f %>% 
          group_by(a) %>%
          summarise(Average = mean(v), 
                    stdDev = sd(v), 
                    Min = min(v), 
                    Max = max(v))
        
      }
      
    })
    
    output$plot_multi <- renderPlot({
      
      if(input$plot_type2 == 1){
        ggplot(df) + geom_point(aes(y = get(input$var_multi_plot1),
                                    x = get(input$var_multi_plot2),
                                    color = as.factor(get(input$aux_var_multi)))) +
          labs(x = input$var_multi_plot2, y = input$var_multi_plot1) + 
          scale_color_discrete(input$aux_var_multi)
        
      }else if(input$plot_type2 == 2){
        
        pcr = prcomp(Filter(is.numeric,df[,-1]), center = T, scale. = T)
        pc_dir = as.data.frame(pcr$rotation)
        pc_df = data.frame(pcr$x)
        
        ggplot(pc_dir)+
          geom_point(data = pc_df, mapping = aes(x=PC1, y=PC2))+
          geom_segment(aes(x = 0, y = 0, yend = 50 * PC2, xend = 50 * PC1))+
          geom_label(mapping = aes(x = 51 * PC1, y = 51 * PC2, label = row.names(pc_dir)))
        
      }else{
        
        ggpairs(df)
        
      }
      
    })
    
    dat_split <- reactive({
      
      splt = input$split
      # vr = as.character(input$var_resp)
      
      set.seed(555)
      
      trainIndex <- createDataPartition(df[,1], p = splt, list = FALSE)
      trainIndex
    })
    
    output$split_plot <- renderPlot({
      
      trainIndex = dat_split()
      df2 = df
      df2$id = 0
      df2$id[trainIndex]=1
      df2$id = as.factor(df2$id)
      
      ggplot(df2, aes(color = id)) + 
        geom_point(aes(y = Sepal.Length, x = 1:nrow(df2))) + 
        scale_color_discrete(name="Split", labels= c("Test","Train"))+
        labs(x = " ", y = "Sepal.Length")
      
    })
    
    # multiple linear regression eventReactive
    btn_run <- eventReactive(input$run_mods,{
        
       trainIndex = dat_split()
       dfTrain <- df[trainIndex, ]
       dfTest <- df[-trainIndex, ]
        
        vars1=as.character(input$vars_mod1)
        f1 = as.formula(paste0("Sepal.Length~",paste0(vars1, collapse = "+")))
        mlr = lm(f1, data = dfTrain)
        mlr_test = predict(mlr, newdata = dfTest)
        
        vars2=as.character(input$vars_mod2)
        f2 = as.formula(paste0("Sepal.Length~",paste0(vars2, collapse = "+")))
        rt = train(f2, data = dfTrain,
                   method = "rpart",
                   tuneGrid = data.frame(cp = seq(0,1,0.1)),
                   trControl = trainControl(method = "cv", number = 5))
        rt_test = predict(rt, newdata = dfTest)
        
        vars3=as.character(input$vars_mod3)
        f3 = as.formula(paste0("Sepal.Length~",paste0(vars3, collapse = "+")))
        rf = train(f3, data = dfTrain,
                   method = "rf",
                   tuneGrid = data.frame(mtry = 1:5),
                   trControl = trainControl(method = "cv", number = 5))
        rf_test = predict(rf, newdata = dfTest)
        
        list(mlr = mlr, rt = rt, rf = rf, mlr_test = mlr_test, 
             rt_test = rt_test, rf_test = rf_test)
    })
    
    output$train_mod1 <- renderPlot({
      
      mlr = btn_run()$mlr
      
      ggplot(mlr, aes(x=.fitted, y=.resid)) + 
        geom_point() + 
        geom_hline(yintercept = 0)
      
    })
    
    output$summary_mod1 <- renderPrint({
      
      summary(btn_run()$mlr)
      
    })
    
    # regression tree
    output$train_mod2 <- renderPlot({
      
      plot(btn_run()$rt)
      
    })
    
    output$summary_mod2 <- renderPrint({
      
      rt = btn_run()$rt
      
      rt$results
      
    })
    
    # random forest
    output$train_mod3 <- renderPlot({
      
      gbmImp = varImp(btn_run()$rf)
      plot(gbmImp)
      
    })
    
    output$summary_mod3 <- renderPrint({
      
      rf = btn_run()$rf
      
      rf$results
      
    })
    
    # test errors
    errors <- reactive({
      
      trainIndex = dat_split()
      dfTest <- df[-trainIndex, ]
      
      mlr = postResample(btn_run()$mlr_test, dfTest$Sepal.Length)
      rt = postResample(btn_run()$rt_test, dfTest$Sepal.Length)
      rf = postResample(btn_run()$rf_test, dfTest$Sepal.Length)
      
      list(mlr = mlr, rt = rt, rf= rf)
      
    })
    
    output$test_error_mlr_summ <- renderPrint({
      
      errors()$mlr
      
    })
    
    output$test_error_rt_summ <- renderPrint({
      
      errors()$rt
      
    })
    
    output$test_error_rf_summ <- renderPrint({
      
      errors()$rf
      
    })
    
    # best model
    output$best_model_choice <- renderPrint({

      bestMethod = function(x){
        
        bestm = which.min(lapply(1:length(x), function(i) x[[i]][1]))
        
        out = switch(bestm,
                     "Multiple Linear Regression",
                     "Regression Tree",
                     "Random Forest")
        
        return(out)
        
      }
      
      tb = data.frame(mlr = errors()$mlr, rt = errors()$rt, 
                      rf = errors()$rf)
      
      paste0("The best model by the RMSE is: ", bestMethod(tb))
      
    })
    
    observeEvent(input$reset_preds, {
      show("Sepal.Width")
      show("Petal.Length")
      show("Petal.Width")
      # show("Species")
    })
    
    observeEvent(input$sub_but, {
      
      if(input$choose == 1){
       
        varnames = as.character(input$vars_mod1)

      }else if(input$choose == 2){

        varnames = as.character(input$vars_mod2)

      }else{

        varnames = as.character(input$vars_mod3)

      }

      out_vars = names(df[!(names(df) %in% varnames)])
      
      if("Sepal.Width" %in% out_vars){
        hide("Sepal.Width")
      }
      
      if("Petal.Length" %in% out_vars){
        # toggle("Petal.Length")
        hide("Petal.Length")
      }
      
      
      if("Petal.Width" %in% out_vars){
        # toggle("Petal.Width")
        hide("Petal.Width")
      }
      
      
      # if("Species" %in% out_vars){
      #   # toggle("Species")
      #   hide("Species")
      #   }
      
  })
    
    output$text <- renderUI({
      h3("Your choice has been recorded!")
    })
    
    # prediction:
    prediction <- reactive({
      
      dat_fr = data.frame(Sepal.Width = input$Sepal.Width,
                          Petal.Length = input$Petal.Length,
                          Petal.Width=input$Petal.Width)
      
      if(input$choose == 1){
        
        varnames = as.character(input$vars_mod1)
        mlr = btn_run()$mlr
        pred = predict(mlr, newdata = dat_fr[varnames])
        
      }else if(input$choose == 2){
        
        varnames = as.character(input$vars_mod2)
        rt = btn_run()$rt
        pred = predict(rt, newdata = dat_fr[varnames])

      }else{
        
        varnames = as.character(input$vars_mod3)
        rf = btn_run()$rf
        pred = predict(rf, newdata = dat_fr[varnames])
        
        
      }
      
      pred
      
    })
    
    btn_pred <- eventReactive(input$run_pred, {
      
      round(prediction(),4)
      
    })
    
    output$text_pred <- renderPrint({
      
      btn_pred()
      
    })
    
    # data page:
    output$picker <- renderUI({
      pickerInput(inputId = 'pick', 
                  label = 'Choose', 
                  choices = colnames(df),
                  options = list(`actions-box` = TRUE),multiple = T)
    })
    
    datasetInput <- eventReactive(input$view,{
      
      datasetInput <- df %>% 
        select(input$pick)
      
      return(datasetInput)
      
    })
    
    output$table <- DT::renderDataTable(server = FALSE,{
      if(input$type_select == "Rows"){
        datatable(
          df,
          selection = "none",
          filter="top", 
          rownames = FALSE,
          extensions = c("Buttons", "Select"),
          
          options = list(
            select = TRUE,
            dom = 'Blfrtip',
            buttons =
              list('copy', list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = "File", title = NULL,
                       exportOptions = list(modifier = list(selected = TRUE))),
                  list(extend = 'excel', filename = "File", title = NULL,
                       exportOptions = list(modifier = list(selected = TRUE)))),
                text = 'Download'
              ))
          ),
          class = "display"
        )
      }else{
        datatable(
          datasetInput(),
          filter="top", 
          rownames = FALSE,
          extensions = 'Buttons',
          
          options = list(
            dom = 'Blfrtip',
            buttons =
              list('copy', 'print', list(
                extend = 'collection',
                buttons = list(
                  list(extend = 'csv', filename = "File", title = NULL),
                  list(extend = 'excel', filename = "File", title = NULL)),
                text = 'Download'
              ))
          ),
          class = "display"
        )
      }
    })
    
})
