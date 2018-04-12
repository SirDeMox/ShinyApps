library(shiny)
#cr Markus Löcher, Max Philipp, HWR Berlin
navbarPage("Navbar!",
           tabPanel("score to p",
                    titlePanel("Random walks, coin flips"),
                    
                    # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("N1",
                                    "Number of Bernoulli trials:",
                                    min = 2,
                                    max = 10000,
                                    value = 30),
                        sliderInput("p1",
                                    "probability of success",
                                    min = 0,
                                    max = 1,
                                    value = 0.5),
                        uiOutput("uirt"),
                        uiOutput("uilt")
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotOutput("distPlot1")
                      )
                    )
                    
           ),
           tabPanel("p to score",

   # Application title
   titlePanel("Random walks, coin flips"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("N",
                     "Number of Bernoulli trials:",
                     min = 2,
                     max = 10000,
                     value = 1000,
                     step=50),
         sliderInput("p",
                     "probability of success",
                     min = 0,
                     max = 1,
                     value = 0.5,
                     step=0.01),
         sliderInput("rt",
                     "right tail",
                     min = 0.5,
                     max = 1,
                     step=0.01,
                     value = .95),
         sliderInput("lt",
                     "left tail",
                     min = 0,
                     max = .5,
                     step=0.01,
                     value = 0.05)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

)


