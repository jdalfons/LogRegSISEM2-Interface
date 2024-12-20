# Multimodal Logistic Regression Shiny App

This Shiny application allows users to perform multimodal logistic regression on a dataset. Users can upload a dataset, handle missing values, encode categorical variables, and run logistic regression to get predictions and probabilities.

## Exemples
You can find an example of how to use the application in the [exemples folder](https://github.com/jdalfons/LogRegSISEM2-Interface/tree/main/exemples).

## Features

- Upload datasets in `.csv`, `.xlsx`, or `.data` formats.
- Handle missing values by dropping them, or replacing them with mean or median values.
- Encode categorical variables using label encoding, one-hot encoding, or frequency encoding.
- Run logistic regression and view model summary, predictions, and probabilities.

## Requirements

- R (version 4.0.0 or higher)
- The following R packages:
  - `shiny`
  - `readxl`
  - `DT`
  - `data.table`
  - `shinyWidgets`
  - `LogRegSISEM2`

## Getting Started

### Step 1: Clone the Repository

```sh
git clone https://github.com/yourusername/LogRegSISEM2-interface.git
cd LogRegSISEM2-interface
```
### Step 2: Check for `renv` and Activate

Ensure you have `renv` installed and activate it:

```sh
if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
}
renv::activate()
```
### Step 3: Run the Application

```sh
R -e "shiny::runApp()"
```