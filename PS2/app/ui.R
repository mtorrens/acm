################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #1 (Part 3)
# Date   : 2016.01.14
################################################################################
# source('~/Desktop/bgse/projects/github/acm/PS2/app/ui.R')
################################################################################

# Library
library(shiny)

# UI
shinyUI(fluidPage(
  tags$h1('Binary classification: Discriminant functions'),
  tags$h5('Shiny app built by Miquel Torrens (c) 2016'),
  tags$h2('Parameter settings'),
  fluidRow(
    column(2,
      tags$h3('Approved requests'),
      sliderInput(inputId = 'mean.x.app',
                  label = 'Mean of variable X',
                  min = 0, max = 15, value = 4, step = 0.1),
      sliderInput(inputId = 'sd.x.app',
                  label = 'Standard deviation of variable X',
                  min = 0, max = 5, value = 1, step = 0.1),
      sliderInput(inputId = 'mean.y.app',
                  label = 'Mean of variable Y',
                  min = 50, max = 200, value = 150),
      sliderInput(inputId = 'sd.y.app',
                  label = 'Standard deviation of variable Y',
                  min = 0, max = 40, value = 20)
    ),
    column(2,
      tags$h3('Denied requests'),
      sliderInput(inputId = 'mean.x.den',
        label = 'Mean of variable X',
        min = 0, max = 15, value = 10, step = 0.1),
      sliderInput(inputId = 'sd.x.den',
              label = 'Standard deviation of variable X',
              min = 0, max = 5, value = 2, step = 0.1),
      sliderInput(inputId = 'mean.y.den',
        label = 'Mean of variable Y',
        min = 50, max = 200, value = 100),
      sliderInput(inputId = 'sd.y.den',
              label = 'Standard deviation of variable Y',
              min = 0, max = 40, value = 30)
    ),
    column(8,
      tags$h3('Simulated data with resulting discriminant function'),
      plotOutput('bivariate.norm')),
      tags$h3('Confusion matrix'),
      tableOutput('conf.matrix')
  )
))
# END OF SCRIPT
