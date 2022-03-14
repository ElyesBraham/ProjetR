library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyMatrix)
source("C:/Users/33645/Documents/R/ProjetR/generation_grille.R")
source("C:/Users/33645/Documents/R/ProjetR/fonctions.R")
source("C:/Users/33645/Documents/R/ProjetR/solver.R")

##########################################################
###################### UI.R #########################
##########################################################

if (interactive()) {

  ui <- fluidPage(

    titlePanel("Sudoku"),

    dashboardPage(
      dashboardHeader(),
      dashboardSidebar(
        sidebarMenu(
          radioButtons(inputId="radio",  #radio pour ne choisir qu'une difficulté à la fois
                       label ="Difficulté",
                       choices = c("Facile" = "easy",
                                   "Moyen" = "medium",
                                   "Difficile" = "hard"),
                       selected = "easy"),
          hr(),

          actionButton(inputId="load", label="Générer un nouveau Sudoku", icon = icon("cog")),
          hr(),
          actionButton(inputId="solve", label="Solution", icon = icon("check")),
          hr(),

          menuItem("", icon = icon(""))

        )),
      dashboardBody(  theme = shinytheme('superhero'),
                      selectInput('grille',label = '', choices = c(9)),
                      actionButton("add", "Jouer !"),
                      uiOutput("grid"),

                      mainPanel(
                        plotOutput("plot_sudoku"),
                        plotOutput("plot_solve_sudoku")

                      )

      )))



  ##########################################################
  ###################### Server.R #########################
  ##########################################################


  server <- function(input, output, session) {

    # Création de la grille 9x9
    observe({
      if(!is.null(input$add)) {
        m = reactive({ matrix('',
                              ncol = as.numeric(input$grille),
                              nrow = as.numeric(input$grille)) })
        output$grid <- renderUI({
          div(matrixInput(inputId = "newGrid", value = m()),)
        })
      }
    })

    #Générer un nouveau Sudoku avec les valeurs manquantes
    observeEvent(input$load,{
      sudoku <- grille2(grille)
      output$plot_sudoku <- renderPlot({plot(sudoku)})
    })


    #Générer la solution du Sudoku
    observeEvent(input$solve,{
      solution_sudoku <- sudoku_solver(grille) # fonction a changer
      output$plot_solve_sudoku <- renderPlot({plot(solution_sudoku)})
    })



  }
  shinyApp(ui, server)
}
