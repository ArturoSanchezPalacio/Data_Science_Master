library(shiny)
library(mvtnorm)
library(plotly)
library(ggplot2)

shinyServer(function(input,output){
  
  grafica <- reactiveValues(muestra = ggplot(), salida = ggplot()) 
  
  observeEvent(input$generar, {
    nbiv <- as.data.frame(rmvnorm(input$tamano, mean = c(0 ,0), 
                                  sigma = matrix(c(1,input$correl,input$correl,1), ncol = 2)))
    grafica$muestra <- (ggplot(data = nbiv, aes(nbiv[,1], nbiv[,2])) + geom_point())
    grafica$salida <- (ggplot(data = nbiv, aes(nbiv[,1], nbiv[,2])) + geom_point()) #Duplico esto porque así cuando añado modelos no se van solapando y el usuario puede probar distintos modelos sobre una misma muestra sin solapamiento.
  })
  
  observeEvent(input$construir, {
    grado <- isolate(input$orden)
    grafica$salida <- grafica$muestra + stat_smooth(method = "lm", formula = y ~ poly(x,grado))
  })
  
  output$grafica <- renderPlotly({
    
    ggplotly(grafica$salida)
    
  })

})


