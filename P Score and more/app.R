library(shiny)
library(DT)

ui <- fluidPage(
  
   titlePanel("Score Tables"),
   
  fluidRow(column(6,numericInput("digit","Number of Digits",value=2,min=0,max=5,step=1)),
           column(6,numericInput("q","F Distributions alpha",value=0.05,min=0,max=1,step=0.01))),
  fluidRow(
    tabsetPanel(type = "tabs",
                tabPanel("Normal", DT::dataTableOutput("normal")),
                tabPanel("Student t", DT::dataTableOutput("ttable")),
                tabPanel("ChiSquare", DT::dataTableOutput("chitable")),
                tabPanel("F Statistic", DT::dataTableOutput("ftable"))
                )
    ))
            
server <- function(input, output) {

  digs <- reactive({input$digit})
#Normal  
    z = seq(1.,3.49,by=0.01)
    ndata <- reactive({round(pnorm(z),digs())})
    m <- reactive({matrix(ndata(),byrow=TRUE,ncol=10,dimnames= list(seq(1.,3.4,by=0.1),seq(0, 0.09,by=0.01)))})
#Student T  
    DF    <- c(1:10, seq(15,25,by=5), seq(30,100,by=10) )
    tails <- c(0.99, 0.975, 0.95, 0.9, 0.1, 0.05, 0.025, 0.01, 0.005, 0.001)
    tstat <- expand.grid(1-tails,DF)
    tdata <- reactive({round(qt(tstat$Var1,tstat$Var2), digs())})
    t <- reactive({matrix(tdata(),byrow=TRUE,ncol=10,dimnames= list(DF,tails))})
#Chi    
    DFchi    <- c(1:10, seq(15,25,by=5), seq(30,100,by=10), seq(125,250,by=25) )
    tailschi <- c(0.99, 0.975, 0.95, 0.9, 0.1, 0.05, 0.025, 0.01, 0.005, 0.001)
    chistat <- expand.grid(1-tailschi,DFchi)
    chidata <- reactive({round(qchisq(chistat$Var1,chistat$Var2), digs())})
    chi <- reactive({matrix(chidata(),byrow=TRUE,ncol=10,dimnames= list(DFchi,tailschi))})
#F Distribution
    DF1 <- c(1:10, seq(15,25,by=5))
    DF2    <- c(1:10, seq(15,25,by=5), seq(30,100,by=10), seq(125,250,by=25) )
    fstat <- expand.grid(DF1,DF2)
    fdata <- reactive({round(qf(input$q,fstat$Var1,fstat$Var2), digs())})
    f <- reactive({matrix(fdata(),byrow=TRUE,ncol=13,dimnames= list(DF2,DF1))})
    


#output normal    
    output$normal <- DT::renderDataTable({
      DT::datatable(m(),
                    options = list(pageLength = 15))
    })
#output student t    
    output$ttable <- DT::renderDataTable({
      DT::datatable(t(),
                    options = list(pageLength = 15))
    })
#output chi
    output$chitable <- DT::renderDataTable({
      DT::datatable(chi(),
                    options = list(pageLength = 15))
    })
#output F Distribution
    output$ftable <- DT::renderDataTable({
      DT::datatable(f(),
                    options = list(pageLength = 15,hover=TRUE))
    })
  

}


shinyApp(ui = ui, server = server)

