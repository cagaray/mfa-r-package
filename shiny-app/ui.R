#ui.R for shiny app for wine data
library(shiny)

shinyUI(fluidPage(

  titlePanel("MFA demo"),
  sidebarLayout(
    sidebarPanel(
      ## Buttons to determine what plot to generate
      radioButtons("Choice",
                   label = h3("Plot Options"),
                   choices = list("Factor Scores" = "factor",
                                  "Partial Factor Scores" = "partial",
                                  "Loadings" = "loadings",
                                  "Eigenvalues" = "eig"),
                   selected = "factor"),
      
      ## Inputs for which Dimensions to be used
      conditionalPanel("input.Choice != 'eig'",
                       numericInput("dim1",
                                    label = "Dimension 1",
                                    value = 1,
                                    min=1,
                                    max=12),
                       numericInput("dim2",
                                    label = "Dimension 2",
                                    value = 2,
                                    min=1,
                                    max=12)
                       ),
      
      ## Conditional Panel to choose the table if partial factor Plot is chosen.
      conditionalPanel("input.Choice == 'partial'",
                       numericInput("table",
                                    label="Table",
                                    value=1,
                                    min=1,
                                    max = 10)
                       )),
    
    mainPanel(
        plotOutput("Plot")
    )
  )
))
