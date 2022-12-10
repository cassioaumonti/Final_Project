
library(shiny)
library(shinythemes)
library(tidyverse)
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
    
    output$split_plot <- renderPlot({
      
      splt = input$split
      vr = as.character(input$var_resp)
      
      set.seed(555)
      
      trainIndex <- createDataPartition(df[,vr], p = splt, list = FALSE)
      df2 = df
      df2$id = 0
      df2$id[trainIndex]=1
      df2$id = as.factor(df2$id)
      
      ggplot(df2, aes(color = id)) + 
        geom_point(aes(y = get(input$var_resp), x = 1:nrow(df2))) + 
        scale_color_discrete(name="Split", labels= c("Test","Train"))+
        labs(x = " ", y = input$var_resp)
      
    })
    
    # multiple linear regression
    btn_run1 <- eventReactive(input$run_mods,{
      
      vars1=as.character(input$vars_mod1)
      f = as.formula(paste0("Sepal.Length~",paste0(vars1, collapse = "+")))
      lm(f, data =df)
      
    })
    
    output$train_mod1 <- renderPlot({
      
      mlr = btn_run1()
      
      ggplot(mlr, aes(x=.fitted, y=.resid)) + 
        geom_point() + 
        geom_hline(yintercept = 0)
      
    })
    
    output$summary_mod1 <- renderPrint({
      
      summary(btn_run1())
      
    })
    
    # regression tree
    btn_run2 <- eventReactive(input$run_mods,{
      
      vars2=as.character(input$vars_mod2)
      f = as.formula(paste0("Sepal.Length~",paste0(vars2, collapse = "+")))
      rt = train(f, data =df,
                 method = "rpart",
                 tuneGrid = data.frame(cp = seq(0,1,0.1)),
                 trControl = trainControl(method = "cv", number = 5))
    })
    
    output$train_mod2 <- renderPlot({
      
      plot(btn_run2())
      
    })
    
    output$summary_mod2 <- renderPrint({
      
      rt = btn_run2()
      
      rt$results
      
    })
    
    output$btn <- 
    
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
