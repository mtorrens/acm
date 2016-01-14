################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #1 (Part 3)
# Date   : 2016.01.14
################################################################################
# source('~/Desktop/bgse/projects/github/acm/PS1/app/app.R')
################################################################################

################################################################################
# Professor's code
################################################################################
# create small wrapper functions
sigmaXY <- function(rho, sdX, sdY) {
    covTerm <- rho * sdX * sdY
    VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                       2, 2, byrow = TRUE)
    return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
    if(!is.na(seed)) set.seed(seed)
    rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
    return(rdraws)
}

# creating a function for all of this
loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                        sdDenied, rhoApproved, rhoDenied, seed=1111) {
    sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
    sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
    approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
    denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
    loanDf <- as.data.frame(rbind(approved,denied))
    deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
    target = c(rep(0, noApproved), rep(1, noDenied))
    loanDf <- data.frame(loanDf, deny, target)
    colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
    return(loanDf)
}
################################################################################
# END Professor's code
################################################################################

################################################################################
# Shiny App
################################################################################
library(shiny)
library(ggplot2)
library(mvtnorm)

# UI
ui <- fluidPage(
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
)

# Server
server <- function(input, output) {
  # Simulate the data
  res <- reactive({
    loanData(50, 50,
             c(input$mean.x.app, input$mean.y.app),
             c(input$mean.x.den, input$mean.y.den),
             c(input$sd.x.app, input$sd.y.app),
             c(input$sd.x.den, input$sd.y.den),
             -0.1, 0.6, seed = 1221)
  })

  # Train a discriminant functions
  reg <- reactive({ lm(target ~ solvency + PIratio + 1, data = res()) })
  w <- reactive({ coef(reg())[c('solvency', 'PIratio')] })
  b <- reactive({ coef(reg())[1] })  
  int <- reactive({ (0.5 - b()) / w()['PIratio'] })  
  slo <- reactive({ (-1) * (w()['solvency'] / w()['PIratio']) })
  preds <- reactive({
    ifelse(predict(reg()) < 0.5, 'Pred. Approved', 'Pred. Denied')
  })

  # Plot
  output$bivariate.norm <- renderPlot({
    ggplot(data = res(), 
           aes(x = solvency, y = PIratio, colour = deny, fill = deny)) +
           geom_point() +
           xlab('Solvency') +
           ylab('PI Ratio') +
           theme_bw() +
           geom_abline(intercept = int(), slope = slo())
  })

  # Confusion matrix
  output$conf.matrix <- renderTable({
    table(res()[, 'deny'], preds())
  })
}

# Run the app
shinyApp(ui = ui, server = server)
