# Required package loading =================================================================
library(shiny)
library(readxl)
library(DT)
library(data.table)
library(shinyWidgets)
library(LogRegSISEM2)
library(shinyjs)


# Maximum upload size definition (in Mo) ===================================================
max_mega_octets <- 100
options(shiny.maxRequestSize = max_mega_octets*1024^2)


# FONCTION A PACKAGER DANS LE REPOS GH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# Fonction pour traiter les valeurs manquantes ----------------------------------------------
handle_missing_values <- function(dataset, NA_process) {
  if (NA_process == "Drop NA") {
    cleaned_dataset <- na.omit(dataset)
  } else if (NA_process == "Mean value") {
    cleaned_dataset <- dataset
    for (col in names(cleaned_dataset)) {
      if (is.numeric(cleaned_dataset[[col]])) {
        mean_value <- mean(cleaned_dataset[[col]], na.rm = TRUE)
        cleaned_dataset[[col]][is.na(cleaned_dataset[[col]])] <- mean_value
      }
    }
  } else if (NA_process == "Median value") {
    cleaned_dataset <- dataset
    for (col in names(cleaned_dataset)) {
      if (is.numeric(cleaned_dataset[[col]])) {
        median_value <- median(cleaned_dataset[[col]], na.rm = TRUE)
        cleaned_dataset[[col]][is.na(cleaned_dataset[[col]])] <- median_value
      }
    }
  } else {
    cleaned_dataset <- dataset
  }
  return(cleaned_dataset)
}
# ----------------------------------------------------------------------------------------

# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# User interface ==========================================================================
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Multimodal logistic regression"),
  
  # Sidebarpanel output --------------------------------------------------------------
  
  sidebarLayout(
    
    sidebarPanel(
      
      div(
        style = "border: 2px solid orange; 
                 border-radius: 5px; 
                 padding: 15px; 
                 background-color: #FFEBCC; 
                 margin-bottom: 10px;",
        
        # File upload
        fileInput("file", "Upload a dataset (.csv, .xlsx, ou .data file)",
                  accept = c(".csv", ".xlsx",".data")),
        
        # Separator selection (.csv files)
        selectInput("sep", "Choose a separator (if .csv)", 
                    choices = c(",", ";", "tabulation"="\t"),
                    selected = ",")
        ),
      
      # Target variable selection
      tags$div(
        style = "border: 2px solid #28a745; 
          border-radius: 5px; 
          padding: 10px; 
          background-color: #AFF5B0; 
          color: #333;",
        tags$h4("Selected target variable :"),
        uiOutput("target_variable_selector")  # widget for var. selection
      ),
      
      # NA cells processing
      selectInput_wrapper <- div(  
        style = "border: 2px solid blue; 
           border-radius: 5px; 
           background-color: #A7C6ED;
           padding: 10px; 
           margin: 10px 0;",
        selectInput(inputId = "NA_process",
                    label = "Choose a NA processing method",
                    choices = c("Drop NA","Mean value","Median value"),
                    selected = "Drop Na")
      )
      
    ),
    
    # Mainpanel output - 3 tabs -----------------------------------------------
    mainPanel(
      tabsetPanel(
        # Initial dataset output
        tabPanel(title = "Initial data",
          DTOutput("table"),  # Dataset output as table
          DTOutput("cleaned_table") # NA cleaned dataset output as table
        ),
        
        #Encoded dataset output
        tabPanel(title = "Encoded dataset",
                 verbatimTextOutput("encoding_summary")
                 
        ),
        
        #Logistic regression results
        tabPanel(title = "Log. regression results")
      )
    )
  )
)

# Server =======================================================================
server <- function(input, output,session) {
  
  # Shinyjs package loading
  observe({
    useShinyjs()
  })
  
  
# Preprocessing (before encoding) ----------------------------------------------
  
  # File reading according to extension
  dataset <- reactive({
    req(input$file)  # File selection check
    req(input$sep)   # Separator choice check
    file <- input$file
    
    # Extension checking and file reading
    ext <- tools::file_ext(file$name)
    if (ext == "csv") {
      read.csv(file$datapath, sep=input$sep,header=TRUE)
    } else if (ext == "xlsx") {
      read_excel(file$datapath)
    } else if (ext == "data") {
      fread(file$datapath)
    } else {
      showNotification("Unsupported file type", type = "error")
      return(NULL)
    }
  })
  
  # Initial dataset output as a table
  output$table <- renderDT({
    req(dataset())
    datatable(
      dataset(),
      class='cell-border stripe', 
      options = list(pageLength = 5, scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "font-size: 20px; color: #007bff; font-weight: bold; padding: 10px; background-color: #f8f9fa; text-align: left;",
        "Initial dataset")
      )
  })

  # Target variable selection
  output$target_variable_selector <- renderUI({
    vars <- colnames(dataset())
    selectInput("target_variable", "Sélectionnez une variable cible :",
                  choices = vars, selected = vars[1])
    }
  )
  
  # Reactive NA processing
  cleaned_dataset <- reactive({
    req(dataset())           # Check that dataset is loaded
    req(input$NA_process)    # Check that method is selected
    handle_missing_values(dataset = dataset(), NA_process = input$NA_process)
  })
  
  # Dataset output with no NAs
  output$cleaned_table <- renderDT({
    req(cleaned_dataset())
    datatable(
      cleaned_dataset(),
      class = 'cell-border stripe', 
      options = list(pageLength = 5, scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "font-size: 20px; color: #007bff; font-weight: bold; padding: 10px; background-color: #f8f9fa; text-align: left;",
        "Cleaned data (NA removed)")
    )
  })
  
}



# ===========================================================================================

# Lancer l'application
shinyApp(ui = ui, server = server)
