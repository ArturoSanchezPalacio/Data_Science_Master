#PRÁCTICA 4. SHINY PARA KMEANS Y MODELOS POLINOMIALES USANDO EL INTERFAZ DASHBOARD

#Arturo Sánchez Palacio
#Fecha : 26/XI/18

#Añadimos la biblioteca en ambos ficheros por si acaso:

library(shiny)
library(ggplot2)
library(MASS)
library(magrittr)
library(dplyr)


shinyServer(function(input, output) {
  
  output$htmlgif <- renderUI({
    tags$img( src = "https://lh3.googleusercontent.com/-cH0BtD0-Wx8/WJDivFwTaGI/AAAAAAAAA9w/WcQZ8IvbXoUBiV8yDP_v_-t0hVgTvb4oACJoC/w1000-h648-rw/616.gif")
  })
  
  output$modelos <- renderUI({
    if (input$opcion == "poly") {
      box(h4("Parámetros modelo polinomial"),
          numericInput("tamano", label = "Introduzca el tamaño de la muestra:", value = 0, min = 1),
          numericInput("correl", label = "Introduzca el valor de la correlación (entre -1 y 1):", value = 0, min = -1,
                       max = 1, step = 0.1),
          actionButton("generar", label = "Generar muestra"),
          numericInput("orden", label = "Introduza el orden del polinomio del modelo:", value = 1, min = 1, step = 1),
          actionButton("construir", label = "Añadir modelo")
      )
      
    } else {
      box( h4("Parámetros modelo KMeans"), selectInput("varx", "Variable x", choices = c("Sepal.Length","Sepal.Width", "Petal.Length","Petal.Width")),
           selectInput("vary", "Variable y", choices = c("Sepal.Length","Sepal.Width","Petal.Length", "Petal.Width")),
           numericInput("nclusters", label = "Número de grupos", min = 1, step = 1, value = 3),
           actionButton("run", "Ejecutar"))
    }
  })
  
  react <- reactiveValues()
  
  observeEvent(input$generar, {
    datos <- as.data.frame(mvrnorm(input$tamano, c(0,0),
                                   Sigma = matrix(c(1, input$correl, input$correl, 1), nrow = 2)))
    colnames(datos) <- c("x", "y")
    react$grafica <- ggplot(datos, aes(x = x, y = y)) + geom_point()
  })
  
  
  observeEvent(input$construir, {
    
    req(react$grafica)
    orden <- input$orden
    react$grafica <- react$grafica + geom_smooth(method = "lm", formula = y ~ poly(x, orden))
  })
  
  
  miModelo <- reactive({
    input$run
    iris %>%
      select_(isolate(input$varx), isolate(input$vary)) %>%
      kmeans(isolate(input$nclusters))
  })
  
  observeEvent(input$run, {
    react$grafica <- ggplot(iris, aes_string(x = isolate(input$varx),
                                             y = isolate(input$vary),
                                             color = as.factor(miModelo()$cluster))) +
      geom_point()
  })
  
  
  output$modelo <- renderPlot({
    react$grafica
  })
  
})
