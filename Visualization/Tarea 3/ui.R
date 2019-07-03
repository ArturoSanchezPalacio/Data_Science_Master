library(shiny)
library(plotly)
shinyUI(
  fluidPage(
    
    titlePanel("Práctica 3. Arturo Sánchez Palacio"),
    
    sidebarLayout(
      
    sidebarPanel(
      numericInput("tamano", label = "Introduzca el tamaño de la muestra:", value = 1, min = 1),
      numericInput("correl", label = "Introduzca el valor de la correlación (entre -1 y 1):", value = 0, min = -1,
                   max = 1, step = 0.1),
      actionButton("generar", label = "Generar muestra"),
      numericInput("orden", label = "Introduza el orden del polinomio del modelo:", value = 1, min = 1, step = 1),
      actionButton("construir", label = "Añadir modelo")
    ),
    mainPanel(
      plotlyOutput('grafica')
    )
    )
  )
)