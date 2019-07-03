#PRÁCTICA 4. SHINY PARA KMEANS Y MODELOS POLINOMIALES USANDO EL INTERFAZ DASHBOARD

#Arturo Sánchez Palacio
#Fecha : 26/XI/18

#Añadimos la biblioteca en ambos ficheros por si acaso:


library(shiny)
library(shinydashboard)
library(ggplot2)
library(MASS)
library(magrittr)
library(dplyr)


shinyUI(
  dashboardPage( skin = "purple",
    dashboardHeader(title = "Práctica 4"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("GIF", tabName = "dashgif"),
        menuItem("Modelo", tabName = "modelo")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashgif", uiOutput("htmlgif")),
        tabItem(tabName = "modelo",
                fluidRow(
                  box(
                    width = 8, solidHeader = TRUE,
                    title = "Gráfica",
                    plotOutput("modelo")
                  ),
                  box(
                    width = 4,
                    title = "Opciones",
                    selectInput("opcion", "Opciones:", c("Polinomial" = "poly","KMeans" = "km")),
                    uiOutput("modelos") 
                  )
                ))
      )
    )
  )
)
