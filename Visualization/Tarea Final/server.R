#FINAL ASSIGNMENT. FUNCTION EXPLORER

#Arturo Sánchez Palacio

#Date: 19/II/19

#Añadimos la biblioteca en ambos ficheros por si acaso:

library(shiny)
library(ggplot2)
library(mosaicCalc)
library(mosaic)
library(plotly)

# Una idea es meter la función en reactivos y los límites. El dataframe ya fuera para que varíe. Contar número de variables (n//2+1).
#Pintarlas y dejar la última libre.

shinyServer(function(input, output) {
  
  grafica <- reactiveValues(muestra = ggplot(), multidim = ggplotly(ggplot()), E = makeFun(0~x&y&z), 
                            valores = seq(0, 10, length.out = 100000), contador = 0 ) 
  
  observeEvent(input$representa, {
    expre <- paste(input$funcion, '~', input$variables)
    grafica$E <- makeFun(eval(parse(text = expre)))
    valores <- seq(input$inf, input$sup, length.out = 100000)
    data <- data.frame(valores, grafica$E(valores))
    grafica$muestra <- ggplot(data, aes(x = data[,1], y = data[,2])) + geom_line() + labs(x = "Eje X", y = "Eje Y")
  })
  
  output$instrucc1 <- renderPrint("El siguiente explorador se divide en dos pestañas. La primera titulada 'Funciones Univariantes' se emplea para la representación de funciones con un único argumento. Se permite elegir el rango de valores que toma dicho argumento. La función se introduce como argumento pudiendo emplerase cualquier variable. En la caja de variables se debe introducir el nombre de la variables sin espacios ni símbolos especiales. Por último basta apretar el botón de representar función.")
  output$instrucc2 <- renderPrint("La segunda es empleada para funciones con más de dos dimensiones. Para ello se introduce la función y las variables separadas por '&' sin espacios. Una vez hecho esto si se pulsa play en el slider se pueden ver las variaciones de las funciones.")
  output$nota1 <- renderText( " \n Los siguientes valores determinan el intervalo en el que se graficara la función:")
  output$nota2 <- renderText( " \n Los siguientes valores determinan el intervalo en el que se graficara la función:")
  
  output$univ <- renderPlot({
    grafica$muestra
  })
  
  observeEvent(input$representa1, {
    expre <- paste(input$funcion1, '~', input$variables1)
    grafica$E <- makeFun(eval(parse(text = expre)))
    grafica$valores <- seq(input$inf1, input$sup1, length.out = 1000)
  })
  
  observeEvent(input$dibujar, {
    valores <- seq(input$inf1, input$sup1, length.out = 1000)
    data <- data.frame(valores, grafica$E(valores,valores))
    grafica$multidim <- plot_ly(a = ~ as.matrix(data)) %>% add_surface()
  })
  

  
  
  
  output$mult <- renderPlotly({
    data <- data.frame(grafica$valores, grafica$E(grafica$valores, grafica$valores, grafica$valores * input$var1))
    grafica$multidim <- plot_ly(z = ~ as.matrix(data)) %>% add_surface()
    grafica$multidim
  })
  
  
})
