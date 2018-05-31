
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mark - Recapture Sample Size Calculator"),
  
  fluidRow(
    h3("Robson & Regier"),
    h5("In a two-event mark-recapture experiment, population abundance N can be estimated using the Chapman modification to the Petersen estimator:"),
    # h5("Nhat = (n1 + 1)(n2 + 1) / (m2 + 1) - 1"),
    withMathJax(h5('$$\\hat{N}=\\frac{(n_1+1)(n_2+1)}{(m_2+1)}-1$$')),
    h5("in which "),
    h5("n1 = number of individuals marked in the first sample, "),
    h5("n2 = number of individuals captured in the second sample, and"),
    h5("m2 = number of marked individuals recaptured in the second sample."),
    h5("Theoretical values are given below for the minimum sample size n2, given a specified n1 and an anticipated N, such that for 
       specified levels of confidence (1-alpha) and relative precision RP, the abundance will be estimated within RP*100%
       of the true value, (1-alpha)*100% of the time."),
    h5("If n2 is known and n1 is unknown, this methodology may be used to determine n1 by reversing the values for n1 and n2."),
    h5("In terms of total sample size, the efficiency is maximized when n1=n2."),
    sidebarLayout(
      sidebarPanel(
        numericInput("N",
                    "Anticipated abundance N:",
                    value = 20000),
        numericInput("n1",
                    "Sample size n1:",
                    value = 2000),
        selectInput("conf",
                    "Desired confidence (1-alpha):",
                    choices=c(0.99,0.95,0.9,0.85,0.8,0.75),
                    selected=0.95),
        downloadButton('downloadData', 'Download table...')),
      mainPanel(
        tableOutput("summary")
      )
    ),
    h5("Robson, D. S., and H. A. Regier.  1964.  Sample size in Petersen mark-recapture experiments.  Transactions of the American FisheriesSociety 93:215-226.")
    ),
  fluidRow(
    # Sidebar with a slider inputs
    h3("Simulation (hypergeometric draws)"),
    #h5("For each trial value of n2, the number of recaptures (m2) is simulated using the values of N and n1 specified above.  Abundance is estimated using a Chapman estimator for each simulated value, and the simulated confidence is calculated as the proportion within each specified value of relative precision from the specified N."),
    #h5("The technique above uses an approximation, and can be empirically validated or explored in more detail by means of simulation.
    #   In the top figure below (once the simulation is run), the relative precision of the upper and lower confidence interval bounds are expressed on the Y-axis,
    #   with these values calculated via simulation for a range of trial values of n2, expressed on the X-axis.  
    #   In the bottom figure, the empirical probability density is shown for an abundance estimate calculated from a single n2 value of interest."),
    #h5("For each trial value of n2, the number of recaptures (m2) is simulated many times using draws from a hypergeometric distribution, with 
    #    the values of anticipated abundance N and selected sample size n1 specified above.  An abundance estimate is then calculated from each simulated m2, giving a vector of 
    #    simulated abundance estimates for each trial value of n2.  The (alpha/2) and (1-alpha/2) quantiles are determined from 
    #    this vector, and expressed as relative precision, according to"),
    h5("The technique above uses an approximation, which can be empirically validated or explored in more detail by means of simulation.
       For a each of a set of trial values of n2, the number of recaptures (m2) is simulated many times using draws from a hypergeometric
       distribution, using the values of anticipated abundance N and sample size n1 as specified above, and each of the 
       range of trial values of n2 specified below by the user.  An abundance estimate is calculated from each
       simulated m2, giving a vector of simulated abundance estimates for each trial value of n2.  The (alpha/2) and (1-alpha/2) quantiles
       are determined from this vector, and expressed as relative precision, according to:"),
    withMathJax(h5("$$RP_{lower} = \\frac{\\hat{N}_{\\frac{\\alpha}{2}} - N}{N}$$")),
    withMathJax(h5("$$RP_{upper} = \\frac{\\hat{N}_{1-\\frac{\\alpha}{2}} - N}{N}$$")),
    h5("In the top figure (below), RP_upper and RP_lower are plotted for the specified range of trial values of n2, once the simulation
       is run.  In the bottom figure, the empirical probability density is shown for an abundance estimate calculated from a single n2 
       value of interest."),
    #h5("Here are some words.  They're very nice words.  Words words words."),
    sidebarLayout(
      sidebarPanel(
        numericInput("N_a",
                     "Anticipated abundance N:",
                     value = 20000),
        numericInput("n1_a",
                     "Sample size n1:",
                     value = 2000),
        numericInput("n2range1",
                    "Sample size n2 to consider (min):",
                    value = 200),
        numericInput("n2range2",
                    "Sample size n2 to consider (max):",
                    value = 4000),
        numericInput("step",
                    "Step size between values of n2:",
                    value = 10),
        sliderInput("precrange",
                    "Relative precision to show (y-axis):",
                    min = 0,
                    max = 1,
                    value = .5,step=0.05),
        checkboxGroupInput("conftry","Confidence(s) (1-alpha) to monitor",c(0.99,0.95,0.9,0.85,0.8,0.75),selected=c(0.99,0.95,0.9,0.85,0.8,0.75)),
        sliderInput("nsim",
                    "number of sims to run for each n2",
                    min = 100,
                    max = 100000,
                    value = 10000),
        numericInput("n22plot","Sample size n2 of interest:",value=1000),
        actionButton("goforit","Run simulation..."),
        
        downloadButton('downloadPlot', 'Download plot...')
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot1",height="950px")# ,
        # plotOutput("plot2",height="260px")
      )
    ))
  
  
))
