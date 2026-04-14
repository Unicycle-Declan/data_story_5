setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# library ----
library(shiny)
library(lubridate)
library(DT)

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
    tabPanel(h5("trees"), 
             fluidRow(
               ## plot number ----
               column(3, selectInput(inputId = "plot",
                                     label = "plot number",
                                     multiple = FALSE,
                                     choices = 1:99,
                                     selected = "1"
                                     )
                      ),
               ## DBH ----
               column(3, numericInput(inputId = "DBH",
                                   label = "DBH(in)",
                                   value = "",
                                   width = "20%"
                                   )
                      ),
               ## height ----
               column(3, numericInput(inputId = "height",
                                   label = "height(ft)",
                                   value = "",
                                   width = "20%"
                                   )
                      ),
               ## species ----
               column(3, selectInput(inputId = "species",
                                     label = "species",
                                     multiple = FALSE,
                                     choices = c(species, ""),
                                     selected = ""
                                     )
                      )
             ),
             fluidRow(column(2),
                      ## Save button ----
                      column(8, actionButton('save',
                                             h2('Save!'),
                                             width='100%')),
                      column(2)
                      )
    ),
    tabPanel(h5("Arondinaria"), 
             fluidRow(
               ## plot number ----
               column(3, selectInput(inputId = "plot",
                                     label = "plot number",
                                     multiple = FALSE,
                                     choices = 1:99,
                                     selected = "1"
                                     )
                      ),
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
               )
  )
)
)
# Server ----
server <- function(input, output) {
  
  # Save button ----
  observeEvent(input$save, {
    newdata <- c(input$text, input$select, input$radio)
    log_line(newdata)
    showNotification("Save successful!")
  })
  
}

# run app ----
shinyApp(ui, server)