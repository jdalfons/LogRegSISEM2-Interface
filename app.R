# Required package loading =================================================================
library(shiny)
library(readxl)
library(DT)
library(data.table)
library(shinyWidgets)
library(LogRegSISEM2)


# Maximum upload size definition (in Mo) ===================================================
max_mega_octets <- 100
options(shiny.maxRequestSize = max_mega_octets*1024^2)



# NA values processing --------------------------------------------------------
handle_missing_values <- function(dataset, NA_process) {
  if (NA_process == "Drop NA") {
    cleaned_dataset <- na.omit(dataset)
  } else if (NA_process == "Mean value") {
    cleaned_dataset <- dataset
    for (col in names(cleaned_dataset)) {
      if (is.numeric(cleaned_dataset[[col]])) {
        mean_value <- round(mean(cleaned_dataset[[col]], na.rm = TRUE), 1)
        cleaned_dataset[[col]][is.na(cleaned_dataset[[col]])] <- mean_value
      }
    }
  } else if (NA_process == "Median value") {
    cleaned_dataset <- dataset
    for (col in names(cleaned_dataset)) {
      if (is.numeric(cleaned_dataset[[col]])) {
        median_value <- round(median(cleaned_dataset[[col]], na.rm = TRUE), 1)
        cleaned_dataset[[col]][is.na(cleaned_dataset[[col]])] <- median_value
      }
    }
  } else {
    cleaned_dataset <- dataset
  }
  return(cleaned_dataset)
}
# -----------------------------------------------------------------------------


# User interface ==========================================================================
ui <- fluidPage(
  # useShinyjs(),
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

        #Encoding methods
        tabPanel(title = "Encoding methods",
                 actionButton("apply_encoding", "Apply encoding",
                              style = "margin-bottom: 15px; background-color: #007bff; color: white; border: none; padding: 10px 20px; border-radius: 5px;"),
                 uiOutput("encoding_widgets")  # Dynamically generated widgets
        ),

        #Encoded dataset output
        tabPanel(title = "Encoded dataset",
                 DTOutput("processed_table")
        ),

        #Logistic regression results
        tabPanel(title = "Log. regression results",
                 actionButton("run_logreg", "Run logistic regression", 
                              style = "margin-bottom: 15px; background-color: #007bff; color: white; border: none; padding: 10px 20px; border-radius: 5px;"),
                 
                 # Text input widgets for learning rate, iterations, and lambda
                 textInput("learning_rate", "Learning rate:", value = "0.01", width = "100%"),
                 textInput("iterations", "Number of iterations:", value = "1000", width = "100%"),
                 
                 
                 h3("Model Summary"),
                 verbatimTextOutput("model_summary"),
                 h3("Predictions and Probabilities"),
                 DTOutput("predictions_table")
        )
      )
    )
  )
)

# Server =======================================================================
server <- function(input, output,session) {

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
    selectInput("target_variable", "Select a target variable :",
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

  # ======== CATEGORICAL VARIABLE ENCODING ==========================================================

# Dynamic widget for encoding
output$encoding_widgets <- renderUI({
  req(cleaned_dataset())
  columns <- setdiff(colnames(cleaned_dataset()), input$target_variable) # Exclude target variable
  # Filter only categorical columns
  categorical_columns <- columns[sapply(cleaned_dataset()[, columns, drop = FALSE], function(col) is.factor(col) || is.character(col))]
  # Generate one `selectInput` per categorical column
  lapply(categorical_columns, function(column) {
    selectInput(
      inputId = paste0("encoding_", column),
      label = paste("Encoding method for", column),
      choices = c("label", "one_hot", "frequency"),
      selected = "label"
    )
  })
})

  # Creation of an `encoding_dict` from user selection
  encoding_dict <- reactive({
    req(cleaned_dataset())
    columns <- colnames(cleaned_dataset())
    # Check if user inputs exists
    all_inputs_ready <- all(sapply(columns, function(column) {
      !is.null(input[[paste0("encoding_", column)]])
    }))
    # if (!all_inputs_ready) {
    #   return(NULL)  # Return NULL if inputs not available yet
    # }
    # Building encoding_dict
    choices <- lapply(columns, function(column) {
      input[[paste0("encoding_", column)]]
    })
    names(choices) <- columns
    return(choices)
  })

  # Encoding then output as processed dataset
  processed_data <- eventReactive(input$apply_encoding, {
    req(cleaned_dataset())
    req(encoding_dict())
    # Creation of CategoricalVerifier (from LogRegSISEM2)
    verifier <- CategoricalVerifier$new(
      cleaned_dataset(), 
      encoding_dict = encoding_dict(), 
      target_var=input$target_variable)
    # Applying encoding method
    verifier$apply_encoding()
    # Retrieving processed dataset
    verifier$get_dataset()
  })

  # Output in a renderDataTable
  output$processed_table <- renderDT({
    req(processed_data())
    datatable(
      processed_data(),
      class = 'cell-border stripe',
      options = list(pageLength = 10, scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "font-size: 20px; color: #007bff; font-weight: bold; padding: 10px; background-color: #f8f9fa; text-align: left;",
        "Encoded dataset")
    )
  })

  # ============================ LOGISTIC REGRESSION  ==========================================================
  
  # X and y variables
  X <- reactive({
    req(processed_data())
    processed_data()[, setdiff(names(processed_data()), input$target_variable), drop = FALSE]  # Exclude target variable
  })
  y <- reactive({
    req(processed_data())
    processed_data()[[input$target_variable]]  # Target variable
  })
  # Model parameters for logistic regression
  learning_rate <- reactive({
    as.numeric(input$learning_rate)
  })
  iterations <- reactive({
    as.integer(input$iterations)
  })
  # Initializing model and predictions
  reglog_model <- NULL
  predictions <- NULL
  probabilities <- NULL
  
  # Running logistic regression when button is clicked
  observeEvent(input$run_logreg, {
    req(X())  
    req(y())  
    req(learning_rate())
    req(iterations())
    
    # Train/test split (with seed = 123)
    set.seed(123)
    train_indices <- sample(seq_len(nrow(X())), size = 0.8 * nrow(X()))
    X_train <- X()[train_indices, , drop = FALSE]
    y_train <- as.factor(ifelse(y()[train_indices] == unique(y())[1], 1, 0))
    X_test <- X()[-train_indices, , drop = FALSE]
    y_test <- as.factor(ifelse(y()[-train_indices] == unique(y())[1], 1, 0))
    # Creating and training log. regression model
    reglog_model <<- LogisticRegression$new(
      learning_rate = learning_rate()
    )
    reglog_model$fit(X_train, y_train)
    # Predictions and probabilities
    predictions <<- reglog_model$predict(X_test)
    probabilities <<- reglog_model$predict_proba(X_test)
    # Printing model summary
    output$model_summary <- renderPrint({
      reglog_model$summary()
      cat("Accuracy: ", mean(predictions == y_test), "\n")
    })
    # Output summary in a table
    output$predictions_table <- renderDT({
      data.frame(
        Predicted = predictions,
        Probability = probabilities
      ) %>%
        datatable(
          class = 'cell-border stripe',
          options = list(pageLength = 10, scrollX = TRUE),
          caption = htmltools::tags$caption(
            style = "font-size: 20px; color: #007bff; font-weight: bold; padding: 10px; background-color: #f8f9fa; text-align: left;",
            "Predictions and probabilities"
          )
        )
    })
  })
  
}

# ===========================================================================================

# Running app
shinyApp(ui = ui, server = server)
