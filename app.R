library(shiny)
library(shinyFiles)
source("CSVprocess_function.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      shinyDirButton("dir", "Input directory", "Upload"),
      verbatimTextOutput("dir", placeholder = TRUE), 
     
      # Horizontal line ----
      tags$hr(),
      textInput("csvname", label="Assign name to result .csv", value="Example.csv"),
      h4("Process selected files to final .csv"),
      actionButton("generate_csv", "Let's go"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Button
      downloadButton("downloadData", "Download ChroMo .csv")
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      tableOutput("finalcsv")

    ),
  ),
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = '.'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  global <- reactiveValues(datapath = normalizePath(getwd()))
  total <- NULL
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 
                 global$datapath <-
                   normalizePath(file.path(getwd(), paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep)))
                 
                 global$completed_path <- paste(global$datapath, "\\", sep="")
                 
                 # output$downloadData <- downloadHandler(
                 #   filename = function() {
                 #     paste("hola", ".csv", sep = "")
                 #   },
                 #   content = function(file) {
                 #     total <- csv_process(completed_path,"hi","hi3.csv")
                 #     print(total)
                 #     print("<-total")
                 #     write.csv(total, file, row.names = FALSE)
                 #   }
                 #   
                 # )
               })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {input$generate_csv},
               handlerExpr = {
               print("hi")
               print(global$completed_path)
               
               output$finalcsv <- renderTable({
                 total <- csv_process(global$completed_path,"hi","hi3.csv")
                 return(total)
               })
               
               # output$downloadData <- downloadHandler(
               #      filename = function() {
               #        paste("hola", ".csv", sep = "")
               #      },
               #      content = function(file) {
               #        total <- csv_process(global$completed_path,"hi","hi3.csv")
               #        print(total)
               #        print("<-total")
               #        write.csv(total, file, row.names = FALSE)
               #      }
               #    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("hola", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(total, file, row.names = FALSE)
    }
  )
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$dir)
    files <- list.files(paste(global$datapath,"\\",sep=""))
    df <- data.frame(files)
    return(df)
    
  })
  
  output$table <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)