#PRÁCTICA 2. GRÁFICA DE DISPERSIÓN DE MTCARS

##Arturo Sánchez Palacio

#Las bibliotecas que se utilizarán son:
library(shiny)
library(plotly)


#De esta manera me ahorro escribir los nombres:
nombres <- names(mtcars)

ui <- fluidPage(
  
  headerPanel("Práctica 2. Arturo Sánchez Palacio"),
  sidebarPanel(
    selectInput('x', 'Eje de abscisas', choices = nombres, selected = "mpg"),
    selectInput('y', 'Eje de ordenadas', choices = nombres, selected = "cyl"),
    
    verbatimTextOutput("salida"),
    
    checkboxGroupInput('facet_fila', 'Facet por filas', c(None = '.', nombres), selected = "vs" ),
    checkboxGroupInput('facet_col', 'Facet por columnas', c(None = '.', nombres), selected="am")
  ),
  
  mainPanel(
    
    plotlyOutput('grafica')
  )
)

server <- function(input, output) {
  output$salida <-renderText({
    "En el 'faceteo' es necesario marcar al menos uno de los checkbox, si no se desea ningún tipo de 'faceteo' marcar 'none' en ambas"
  })
  
  output$grafica <- renderPlotly({
    
    # Construyo la gráfica (sin facet) y la almaceno en un objeto p:
    p <- ggplot(mtcars, aes_string(x = input$x, y = input$y)) + 
      geom_point()
    
    
    facets <- paste(input$facet_fila, '~', input$facet_col) #Construyo el facet.
    if (facets != '. ~ .') p <- p + facet_grid(facets) #Si no es el vacío lo sumo al plot, si no el facet no se suma y se construye la grafica sin facet.
    
    ggplotly(p)
    
  })
  
}

shinyApp(ui, server)