
library(shiny)

shinyUI(fluidPage(
  
# Application title
  titlePanel("Distributions"),

                   
# Sidebar 
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(6,
               selectInput("dist", "distribution", 
                           choices = list("normal" = "rnorm", "binominal" ="rbinom","poisson"="rpois","beta"="rbeta","chi squared"="rchisq"), 
                           selected = 1)),
        column(6,
               numericInput("obs", "samplesize", 5))
        
      ),
      
      conditionalPanel(
        condition = "input.dist == 'rnorm'",
        tags$hr(),
      fluidRow(
        column(6,
               numericInput("mean1", "mean", 0)),
        column(6,
               numericInput("sd", "sd", 1, min=0))
      ),
      
      checkboxInput("dispnorm","display normal distribution", value=FALSE)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'rchisq'",
        tags$hr(),
        fluidRow(
          column(6,
                 numericInput("chidf", "degrees of freedom", 5, min=0))
        ),
        
        checkboxInput("dispchi","display chi distribution", value=FALSE)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'rpois'",
        tags$hr(),
        fluidRow(
          column(6,
                 numericInput("poislamb", "lambda", 20, min=0))
        )
      ),
      
      conditionalPanel(
        condition = "input.dist == 'rbinom'",
        tags$hr(),
        fluidRow(
          column(6,
                 numericInput("binomsize", "trails", 5,min=0)),
          column(6,
                 numericInput("binomprob", "probability", 0.4, min=0,max=1,step = 0.05))
        )
      ),
      
      conditionalPanel(
        condition = "input.dist == 'rbeta'",
        tags$hr(),
        fluidRow(
          column(6,
                 numericInput("betashape1", "shape 1", 10,min=0)),
          column(6,
                 numericInput("betashape2", "shape 2", 5, min=0))
        ),
        checkboxInput("dispbeta","display beta distribution", value=FALSE)
      ),
      tags$hr(),
      numericInput("max", "upper group limit", 50, step=25),
      
      actionButton("go","add new observations"),
      actionButton("fill","fill until group limit is reached"),
      actionButton("clean","clean plots", icon=icon("exclamation")),
      plotOutput("selectPlot"),
      tags$hr(),
      fluidRow(
        column(6,
               radioButtons("fix", "y axis",
                            choices = c("fixed" = TRUE,
                                        "adjusted" = FALSE),
                            selected = FALSE),
               checkboxInput("hist","counts / density", value=FALSE)
               
              # radioButtons("hist","axis label",choices=c(TRUE,FALSE))
               
               ),
        conditionalPanel(
          condition = "input.hist == false",  
        column(6,
               checkboxInput("dense","overlay density plot", value=FALSE))
      )),
      
      radioButtons("pm", "print means",
                   choices = c(yes = TRUE,
                               no = FALSE),
                   selected = TRUE)

    ),
    
# Mainpanel
    mainPanel(plotOutput("distPlot"),
       plotOutput("histPlot")
    )
  )
))
