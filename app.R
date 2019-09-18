library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # header
                headerPanel("Test data input"),
                
                sidebarLayout(
                  # sidebar for form
                  sidebarPanel(
                    h3("Information",""),
                    textInput("syn_pat_id", "Synthetic Participant ID",""),
                    textInput("ncit_code", "NCIt code",""),
                    textInput("relational_str", "Relational",""),
                    textInput("obs_value", "Observation Value",""),
                    actionButton("addEntry", "Add New Data"),
                    helpText("Click to insert new data ")
                    
                  ),
                  
                  # output for viewing
                  mainPanel(
                    
                    DT::dataTableOutput("userEnteredData"),
                    actionButton("deleteSelectedRows", "Delete Selected Rows")
                    
                  )   
                )
)

server <- function(input, output) {

  valuesToAdd <- reactiveValues()

  #
  # Process the input from the data entry fields into new_data element of the valuesToAdd reactive
  #
  
  observeEvent(input$addEntry, {
    new_data_entered <- data.frame(
      syn_pat_id = input$syn_pat_id,
      ncit_code = input$ncit_code,
      relational_str = input$relational_str,
      obs_value = input$obs_value,
      stringsAsFactors = FALSE
    )
    print(new_data_entered)
    
    if (is.null(valuesToAdd[["new_data"]])) {
      print("First data row in the dataframe")
      valuesToAdd$new_data <- new_data_entered
    } else {
      print("adding to existing dataframe")
      valuesToAdd$new_data <-
        rbind(valuesToAdd$new_data, new_data_entered)
    }
  })
  
  #
  # Process the delete button click, deleting the selected rows (if any) from the dataframe
  #
  observeEvent(input$deleteSelectedRows,
    {
    if (!is.null(input$userEnteredData_rows_selected) & !is.null(valuesToAdd[["new_data"]])) {
      valuesToAdd$new_data <- valuesToAdd$new_data[-(input$userEnteredData_rows_selected),]
    }
    
  })

  # output as data table      
  output$userEnteredData <- DT::renderDataTable(valuesToAdd$new_data)

  
}


shinyApp(ui = ui, server = server)
