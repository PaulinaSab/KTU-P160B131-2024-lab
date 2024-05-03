library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(title = "KTU Lab"),
  dashboardSidebar(
    selectizeInput("kodas",
                   "Įveskite įmones pavadinimą",
                   choices = NULL)
    
  ),
  dashboardBody(
    fluidRow(
      style = "padding: 0px 10px 10px 10px;",
      h2(textOutput("company_name"), style = "text-align: center; font-size: 24px; color: black; font-weight: bold; margin: 0px;")
    ),
    fluidRow(
      uiOutput("square1"),
      uiOutput("square2"),
      tableOutput("table"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  data <- readRDS("../data/data.rds")
  updateSelectizeInput(session, "kodas", choices = data$name, server = TRUE)
  
  
  output$company_name <- renderText({
    company_name <- as.character(head(data[data$name == input$kodas, "name"], 1))
    company_name
  })
  
  output$square1 <- renderUI({
    filtered <- data %>%
      group_by(name) %>%
      summarise(dsk = sum(avgWage)) %>%
      filter(name == input$kodas)
    year <- filtered$dsk
    div(style = "background-color: black;
              width: 400px; 
              height: 100px; 
              margin: 10px;
              float:left
              border-radius: 0%;
              color: white;
              font-weight: bold;
              font-size: 30px; 
              text-align: center;
              padding: 10px;,
              display: inline-block;",  year, " €",  
        br(),
        span(style = "font-size: 16px; color: white;", "Faktinis metinis atlyginimas")  
    )
    
  })
  
  output$square2 <- renderUI({
    company_code <- as.character(head(data[data$name == input$kodas, "code"], 1))
    div(style = "background-color: purple; 
                   width: 400px; 
                   height: 100px; 
                   margin: 10px;
                   float:rigth; 
                   margin: 10px; 
                   border-radius: 0%;  
                   color: white;  
                   font-weight: bold; 
                   font-size: 30px;  
                   text-align: center;  
                   padding: 10px;,
                   display: inline-block;", company_code,  
        br(),          
        span(style = "font-size: 16px; color: white;", "Įmonės kodas"))
  })
  
  output$table <- renderTable({
    data %>%
      filter(name == input$kodas) %>%
      group_by(`Įmonės pavadinimas` =name) %>%
      summarise(`Minimalus mėnesio atlyginimas` = ifelse(all(is.na(avgWage)), 0, min(avgWage, na.rm = TRUE)),
                `Maksimalus mėnesio atlyginimas` = ifelse(all(is.na(avgWage)), 0, max(avgWage, na.rm = TRUE)),
                `Vidutinis mėnesio atlyginimas` = ifelse(all(is.na(avgWage)), 0, mean(avgWage, na.rm = TRUE)))
  })
  
  output$plot <- renderPlot(
    data %>%
      filter(name == input$kodas) %>%
      ggplot(aes(x = ym(`month`), y = avgWage)) +
      labs(main = "Atlyginimo dinamika",
           x = "Mėnesiai",
           y = "Atlyginimas") +
      geom_line(color = "purple", legend = FALSE, size = 1) +
      geom_point(color = "black", size = 3, shape = 19) +
      theme_classic()
  )
  
}

shinyApp(ui, server)
