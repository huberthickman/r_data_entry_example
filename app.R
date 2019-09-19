library(shiny)
library(shinythemes)
library(DT)

ui <- fluidPage(theme = shinytheme("sandstone"),
                
                # header
                headerPanel("Test data input"),
                
                sidebarLayout(
                  # sidebar for form
                  sidebarPanel(
                    textInput("participant_id", "Participant ID",""),
                    textInput("ncit_code", "NCIt code",""),
                    textInput("relational_str", "Relational",""),
                    textInput("obs_value", "Observation Value",""),
                    actionButton("addEntry", "Add New Data"),
                    helpText("Click to insert new data ")
                    
                  ),
                  
                  # output for viewing
                  mainPanel(
                    
                    DTOutput("userEnteredData"),
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
      participant_id = input$participant_id,
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
                 if (!is.null(input$userEnteredData_rows_selected) &
                     !is.null(valuesToAdd[["new_data"]])) {
                   valuesToAdd$new_data <-
                     valuesToAdd$new_data[-(input$userEnteredData_rows_selected), ]
                 }
               })
    
  # 
  # Process the editing of a cell.  Get the new value and place it in the reactive dataframe
  #
  observeEvent(input$userEnteredData_cell_edit,
  {
    print("cell edit")
    info = input$userEnteredData_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    valuesToAdd$new_data[i,j] = v
  })
  
  # output as data table      
  output$userEnteredData <- renderDT(valuesToAdd$new_data, editable=TRUE)

  
}


shinyApp(ui = ui, server = server)
