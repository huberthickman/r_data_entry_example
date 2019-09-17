library(shiny)
library(ggplot2)
library(xlsx)
#library(xlsxjars)
#library(rJava)
library(shinythemes)
library(rhandsontable)

# Define UI -----------
# ---------------------


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
                    helpText("Click to insert new data "),
                    br(),

                    downloadButton("downloadData", "Download"),
                    helpText("Click for download the data (.csv) ")
                  ),
                  
                  # output for viewing
                  mainPanel(
                    
                    DT::dataTableOutput("newDataRow")
                   # br(),
                   # rHandsontableOutput("newDataRow")

                    
                  )   
                )
)


# Define server logic ------
# --------------------------

server <- function(input, output) {
  input_df <- data.frame(syn_pat_id = character(0), 
                   ncit_code = character(0), 
                   relational_str = character(0), 
                   obs_value = character(0),check.names = FALSE , stringsAsFactors = FALSE  )
  
  valuesToAdd <- reactiveValues()
  #valuesToAdd$new_data <- data.frame(syn_pat_id = character(0), 
  ##                                   ncit_code = character(0), 
  #                                   relational_str = character(0), 
  #                                   obs_value = character(0),check.names = FALSE , stringsAsFactors = FALSE  )
  
  # process the textinput
  new_data_table <- observeEvent(input$addEntry,{  
    
    
    # creating table
    
    new_data_entered <- data.frame(syn_pat_id = input$syn_pat_id, 
                          ncit_code = input$ncit_code, 
                          relational_str = input$relational_str, 
                          obs_value = input$obs_value, 
                          stringsAsFactors = FALSE)
    print(new_data_entered)     
                  
   if (is.null(valuesToAdd[["new_data"]])) {
     print("NULL valuesToAdd dataframe")
     
     valuesToAdd$new_data <- new_data_entered
   } else {
     print("adding to existing dataframe")
     valuesToAdd$new_data <- rbind(valuesToAdd$new_data, new_data_entered)
     
   }
    
    #print(valuesToAdd$newData)
    
   # browser()
    #print(new_data_entered)
    #input_df <- rbind(input_df, new_data_table())
    #print(input_df)
    
    return(new_data_entered)
  })
  
  # process the text file and download

  
  # add new row (?)
  

  
  

  
  # output as data table      
  output$newDataRow <- DT::renderDataTable(
    valuesToAdd$new_data
  )
  output$hot1 <- renderRHandsontable(input_df)

  # download the file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("DTI", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(mytable2(), file, row.names = FALSE)
    }
  )
  
}

# Run the app ----------
# ----------------------

shinyApp(ui = ui, server = server)
