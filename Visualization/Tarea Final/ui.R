#FINAL ASSIGNMENT. FUNCTION EXPLORER

#Arturo Sánchez Palacio

#Date: 19/II/19

# We will be using the following libraries:


library(shiny)
library(shinydashboard)
library(ggplot2)
library(MASS)
library(magrittr)
library(dplyr)
library(mosaicCalc)
library(mosaic)
library(plotly)

shinyUI(
  dashboardPage( skin = "purple",
    dashboardHeader(title = "Function Explorer"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Instrucciones", tabName = "instru"),
        menuItem("Funciones univariantes", tabName = "uni"),
        menuItem("Funciones multivariantes", tabName = "multi")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "instru", fluidRow(
          box(
            width = 8, solidHeader = TRUE,
            title = "Instrucciones de uso",
            verbatimTextOutput("instrucc1"),
            verbatimTextOutput("instrucc2")
          )
        )),
        tabItem(tabName = "uni", fluidRow(
          box(
            width = 8, solidHeader = TRUE,
            title = "Gráfica de la función",
            plotOutput("univ")
          ),
          box(
            width = 4, 
            textInput("funcion", label = h3("Introduzca una función:"), value = '3 * x'),
            textInput("variables", label = h3("Introduzca las variables separadas por comas:"), value = 'x'),
            textOutput("nota1"),
            numericInput("inf", label = h3("Rango inferior"), value = -10),
            numericInput("sup", label = h3("Rango superior"), value = 10),
            actionButton("representa", label = "Representar función")
          ),
          box(
            width = 8,
            title = "Información"
          )
        )),
        tabItem(tabName = "multi", fluidRow(
          box(
            width = 8, solidHeader = TRUE,
            title = "Gráfica de la función",
            plotlyOutput("mult")
          ),
          box(
            width = 4,
            title = "Comandos para la navegación:",
            sliderInput("var1", label = h3("Primera variable"), min = -10, max = 10, value = 0, step = 0.5, animate = TRUE),
            sliderInput("var2", label = h3("Segunda variable"), min = -10, max = 10, value = 0, step = 0.5, animate = TRUE)
          ),
          box(
            width = 8, 
            textInput("funcion1", label = h3("Introduzca una función:"), value = '3 * x * y'),
            textInput("variables1", label = h3("Introduzca las variables separadas por comas:"), value = 'x&y'),
            textOutput("nota2"),
            numericInput("inf1", label = h3("Rango inferior"), value = -10),
            numericInput("sup1", label = h3("Rango superior"), value = 10),
            actionButton("representa1", label = "Representar función"),
            actionButton("dibujar", label = "Dibujar")
          ),
          box(
            width = 4,
            title = "Información"
          )
        ))
      )
    )
  )
)
