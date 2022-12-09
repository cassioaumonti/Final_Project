
library(shiny)
library(GGally)

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
    
})
