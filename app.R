# App for submitting data from an excel file to a MySQL database.
# create insert statements for MySQL

library(shiny)

################################################################################
#### Functions 
################################################################################

# check entry
check_entry <- function(entry){
  # double single quotes for mysql syntax
  entry <- gsub("'", "''",entry, useBytes = TRUE)
  # remove leading and trailing whitespace
  entry<-trimws(entry)
  # don't send empty string and replace NA with NULL
  if(entry=="" | is.na(entry)){
    entry <- NULL
  }
  return(entry)
}

# generate mysql statement
generate_insert <- function(table, column, value){
  x <- paste0("INSERT INTO ",table," (",column,") VALUES ('",value, "')")
  # remove quotes if value is NULL
  x <- gsub("'NULL'", "NULL",x, useBytes = TRUE)
  return(x)
}

# row of df to msql insert
row_to_insert <- function(row,table,column) {
  entry <- lapply(row,check_entry)
  values <- paste(entry, collapse = "','")
  insert <- generate_insert(table,column, values)
  # end with ;
  return(paste0(insert,";"))
}


################################################################################

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("CSV to SQL statements"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput(inputId="db_name", 
                label = p("Enter the database name:"), 
                value = ""),
      textInput(inputId="table", 
                label = p("Enter the table to create inserts for:"), 
                value = ""),
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h3("Preview of the CSV file:"),
      p("check your data, the column names have to be identical to the MySQL columns."),
      # Output: Data file ----
      tableOutput("contents"),
      h3("Preview of the SQL statements:"),
      tableOutput("statements"),
      h3("Download the SQL statements:"),
      downloadButton("download_button", label = "Download")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  datafile <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = TRUE,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    return(df)
  })
  
  output$contents <- renderTable({
    if(input$disp == "head") {
      return(head(datafile()))
    }
    else {
      return(datafile())
    }
  })
  
  # id_Author`, `first_name`, `last_name` ,...
  col_names <- reactive({
    col <- paste(colnames(datafile()),collapse="`, `") 
    return(paste0("`",col, "`"))
    })
  
  # INSERT INTO `db`.`table` (`id_Author`, ... ) VALUES 
  table_name <- reactive({
    paste0("`", input$db_name, "`.`", input$table,"`")
    })
  
  sql_inserts <- reactive({
    apply(datafile(),1,row_to_insert, table=table_name(), column= col_names())
  })
  
  output$statements <- renderTable({
    if(input$disp == "head") {
      return(head(sql_inserts()))
    }
    else {
      return(sql_inserts())
    }
  })
  
  # download SQL lines
  output$download_button <- downloadHandler(
    filename = function(){
      paste0("insert_",input$db_name,"_",input$table,".sql")
    },
    content = function(file) {
      writeLines(c("START TRANSACTION;", paste0("USE `",input$db_name,"` ;"),
                   sql_inserts(), "COMMIT;"), file)
    }
  )
  
  # stop when app is closed
  session$onSessionEnded(function(session){
    stopApp()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
