setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# library ----
library(shiny)
library(lubridate)
library(DT)

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
  br(),
  fluidRow(
    # Example input: manual text entry
    column(4, textInput('text',
                        label='Type here',
                        value='typing placeholder',
                        width = '95%')),
    
    # Example input: selecting pre-canned options
    column(4, selectInput('select',
                          label='Select here',
                          choices = paste('select', 1:4),
                          width='95%')),
    
    # Example input: toggling between options
    column(4, radioButtons('radio',
                           label='Toggle here',
                           choices = paste('toggle', 1:4),
                           inline = TRUE,
                           width='95%'))),
  br(),
  br(),
  fluidRow(column(2),
           # Save button!
           column(8, actionButton('save',
                                  h2('Save!'),
                                  width='100%')),
           column(2))
)

# Server ----
server <- function(input, output) {
  
  # Save button ================================================================
  observeEvent(input$save, {
    newdata <- c(input$text, input$select, input$radio)
    log_line(newdata)
    showNotification("Save successful!")
  })
  #=============================================================================
  
}

################################################################################
################################################################################

shinyApp(ui, server)