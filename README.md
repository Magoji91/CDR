# CDR
BASIC TABLE EXERCISE

##load data
getwd()
setwd("/Users/smsanda/Documents/")
library(readxl)
AIT <- read_excel("AIT.xlsx", sheet = "Plan1")

## app.R ##
library(shinydashboard)
library(shiny)
library(threejs)

options(shiny.launch.browser=F, shiny.minified=F, shiny.port = 4601)

ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "Testing dynamic sidebar bookmarking"),
    dashboardSidebar(
      uiOutput("sidebarControls"),
      sidebarMenuOutput("menu")
    ),
    dashboardBody(
      tabItems(
        tabItem("front",
                h3("Click through the different tabs to see different content")
        ),
        tabItem("models1",
                h3("Here's a linear model"),
                verbatimTextOutput("models1")
        ),
        tabItem("models2",
                h3("Here's a logistic regression"),
                verbatimTextOutput("models2")
        ),
        tabItem("plots1",
                h3("Here's a 2D plot"),
                plotOutput("plots1")
        ),
        tabItem("plots2",
                h3("Here's a 3D plot"),
                scatterplotThreeOutput('plots2')
        ),
        tabItem("tables",
                h3("Here's a table"),
                tableOutput("tbl")
        )
      )
    )
  )
}


server <- function(input, output, session) {
  output$menu <- renderMenu({
    sidebarMenu(id = "smenu",
                menuItem("Frontpage", tabName = "front"),
                menuItem("Models",
                         menuSubItem("Linear model", "models1"),
                         menuSubItem("Logistic regression", "models2")
                ),
                menuItem("Plots",
                         helpText("This is help text"),
                         menuSubItem("Scatterplot", "plots1"),
                         menuSubItem("3D graph", "plots2")
                ),
                menuItem("Tables", tabName = "tables")
    )
  })
  output$sidebarControls <- renderUI({
    req(input$smenu)
    if (input$smenu %in% c("models1", "models2", "plots1")) {
      tagList(
        selectInput("xaxis", "X axis", names(AIT), selected = input$xaxis),
        selectInput("yaxis", "Y axis", names(AIT), selected = input$yaxis)
      )
    } else if (input$smenu == "plots2") {
      tagList(
        selectInput("xaxis", "X axis", names(AIT), selected = input$xaxis),
        selectInput("yaxis", "Y axis", names(AIT), selected = input$yaxis),
        selectInput("zaxis", "Z axis", names(AIT), selected = input$zaxis)
      )
    } else NULL
  })
  
  formula <- reactive({
    req(input$yaxis, input$xaxis)
    as.formula(paste(input$yaxis, input$xaxis, sep = " ~ "))
  })
  
  output$models1 <- renderPrint({
    summary(glm(formula(), data = AIT), family = "linear")
  })
  
  output$models2 <- renderPrint({
    summary(glm(formula(), data = AIT), family = "binomial")
  })
  
  output$plots1 <- renderPlot({
    plot(formula(), data = AIT)
  })
  
  output$plots2 <- renderScatterplotThree({
    x <- AIT[[input$xaxis]]
    y <- AIT[[input$yaxis]]
    z <- AIT[[input$zaxis]]
    scatterplot3js(x, y, z)
  })
  
  output$tbl <- renderTable({
    AIT
  })
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
}

enableBookmarking("url")
shinyApp(ui, server)
