#server.R for shiny app for wine data

library(shiny)
library(MFA)

shinyServer(function(input, output) {

  ##groups for the plot of loadings
  assessors <- c("assessor 1", "assessor 2", "assessor 3", "assessor 4", "assessor 5",
                 "assessor 6", "assessor 7", "assessor 8", "assessor 9", "assessor 10")
  assessors.variable <- NULL
  for (i in 1:length(sets)) {
    assessors.variable <- c(assessors.variable, rep(assessors[i], length(sets[[i]])))
  }

  ##labels for the plot of loadings
  variables <- colnames(wines)[unlist(sets)]

  ##mfa analysis for wine data
  mfa.wines <- mfa(wines, sets)

  ##generate the plots
  output$Plot <- renderPlot({switch(input$Choice,
         "eig" = PlotEig(mfa.wines),
         "factor" = PlotFactorScores(mfa.wines, group = wines$country, label = wines$ID, dim = c(input$dim1, input$dim2)),
         "partial" = PlotPartialFactorScores(mfa.wines, k = input$table, group = wines$country, label = wines$ID, dim = c(input$dim1, input$dim2)),
         "loadings" = PlotLoadings(mfa.wines, var.group = assessors.variable, label = variables, dim = c(input$dim1, input$dim2)))
  })
})
