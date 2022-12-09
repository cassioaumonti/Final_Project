
library(shiny)

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
    
})
