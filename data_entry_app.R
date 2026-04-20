setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# library ----
library(shiny)
library(lubridate)
library(DT)
library(readr)

species <- 
  c("Quercus coccinea", "Oxydendrum arboreum", "Liquidambar styraciflua", "Acer rubrum", "Sassafras albidum", "Quercus alba", "Nyssa sylvatica")
# Function for saving data to a CSV file ----
log_line <- function(newdata, filename = 'app_data.csv'){
  (dt <- Sys.time() %>% round %>% as.character)
  (newline <- c(dt, newdata) %>% paste(collapse=',') %>% paste0('\n'))
  cat(newline, file=filename, append=TRUE)
  print('Data stored!')
}

# UI ----
ui <- fluidPage(
  titlePanel(h4("Data entry app")),
  tabsetPanel(
    ## tree tab ----
    tabPanel(h5("Trees"), 
             fluidRow(
               ### plot number ----
               column(3, selectInput(inputId = "plot",
                                     label = "plot number",
                                     multiple = FALSE,
                                     choices = 1:99,
                                     selected = "1"
                                     )
                      ),
               ### DBH ----
               column(3, numericInput(inputId = "DBH",
                                   label = "DBH(in)",
                                   value = "",
                                   width = "20%"
                                   )
                      ),
               ### height ----
               column(3, numericInput(inputId = "height",
                                   label = "height(ft)",
                                   value = "",
                                   width = "20%"
                                   )
                      ),
               ### species ----
               column(3, selectInput(inputId = "species",
                                     label = "species",
                                     multiple = FALSE,
                                     choices = c(species, ""),
                                     selected = ""
                                     )
                      )
             ),
             fluidRow(column(2),
                      ### Save button ----
                      column(8, actionButton('save_tree',
                                             h2('Save'),
                                             width='100%')),
                      column(2)
                      ),
             ### table ----
             fluidRow(column(12, DTOutput("tree_dt")
                             )
                      )
    ),
    ## Arondinaria tab ----
    tabPanel(h5("Arondinaria"), 
             fluidRow(
               ### plot number ----
               column(3, selectInput(inputId = "plot",
                                     label = "plot number",
                                     multiple = FALSE,
                                     choices = 1:99,
                                     selected = "1"
                                     )
                      ),
               ### area ----
               column(2, numericInput(inputId = "L",
                                   label = "DALT(ft)",
                                   value = " ",
                                   width = "50%"
                                   )
                      ),
               column(2, numericInput(inputId = "S",
                                   label = "DAST(ft)",
                                   value = " ",
                                   width = "50%"
                                   )
                      ),
               column(2, textOutput(outputId = "area"
                                    )
                      )
               ),
             ### save button ----
             fluidRow(
               column(2),
               column(8,actionButton("save_Ar",
                                     h2("Save"),
                                     width = "100%")),
               column(2)
             ),
             ### table ----
             fluidRow(
               column(12, DTOutput("Ar_data"))
             )
  )
)
)
# Server ----
server <- function(input, output) {
  rv <- reactiveValues()
  rv$tree_df <- read.csv('tree_data.csv', header=FALSE)
  rv$Ar_df <- read.csv('Ar_data.csv', header=FALSE)
  
  # tree data table
  output$tree_dt <- renderDT({
    tree_df <- read.csv('tree_data.csv')
    rv$tree_df
  })
  
  # tree Save button ----
  observeEvent(input$save_tree, {
    newdata <- c(input$plot, input$DBH, input$height, input$species)
    log_line(newdata, "tree_data.csv")
    showNotification("Save successful!")
  })
  
  # area (calculate area as a reactive value) ---- 
  area_value <- reactive({
    round(pi * (((input$L + input$S) / 4)^2), digits = 2)
  })
  
  output$area <- renderText({
    paste(area_value(), "ft^2")
  })
  
  # Arondinaria Save button ----
  observeEvent(input$save_Ar, {
    newdata <- c(input$plot, area_value())
    log_line(newdata, "Ar_data.csv")
    showNotification("Save successful!")
  })
  
  # Arondinaria data table
  output$Ar_data <- renderDT({
    Ar_df <- read.csv('Ar_data.csv')
    rv$Ar_df
  })
  
}

# run app ----
shinyApp(ui, server)
