################################################################################
# Barcelona Graduate School of Economics
# Master's Degree in Data Science
# Course : Avanced Computational Methods
################################################################################
# Author : (c) Miquel Torrens
# Title  : Problem Set #1 (Part 3)
# Date   : 2016.01.14
################################################################################
# source('~/Desktop/bgse/projects/github/acm/PS1/app/server.R')
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

# Library
library(shiny)
library(mvtnorm)
library(ggplot2)

# Server
function(input, output) {
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

