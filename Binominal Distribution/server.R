#cr Markus Löcher, Max Philipp, HWR Berlin
library(shiny)


# Define server logic required to draw a histogram
server <- function(input, output) {
   M=1000
   SimulatedCounts=FALSE;

   output$distPlot <- renderPlot({
      # generate binomial distribution
      
      
      if (SimulatedCounts){
        x  <- rbinom(M,input$N, input$p) 
        # draw the histogram with the specified number of bins
        hb=hist(x, col = 'darkgray', border = 'white', main = "", 
              xlab="num of successes", breaks=round(2*sqrt(input$N)), density=TRUE)
      } else {
        k = pmax(0,qbinom(0.001,input$N,input$p)):qbinom(0.999,input$N,input$p)
        x  <- dbinom(k,input$N, input$p)
        # barplot(x,names=k,col = 'darkgray', border = 'white', main = "", 
        #         xlab="num of successes",space=0);box()
        hb=list()
        hb$counts=x
        hb$breaks = c(min(k)-1,k);#hb$breaks=hb$breaks-min(hb$breaks)
        class(hb)="histogram"
        plot(hb,col = 'darkgray', border = 'white', main = "", 
                xlab="num of successes")
      } 
      grid()
      vr=qbinom(input$rt,input$N, input$p)
      vl=qbinom(input$lt,input$N, input$p)
      try({
        tit1 = paste("interval:", vr-vl, "(", round((vr-vl)/input$N,4),")\n")
        tit2 = paste("s:", round(sqrt(input$p*(1-input$p)/input$N),5))
        title(paste(tit1,tit2))
        
        abline(v=vl, col=2);mtext(paste(vl),side=3,at=vl,col=2,line=0.5)
        for (i in 1:(match(T,hb$breaks>vl)-2)) 
          rect(hb$breaks[i],0,hb$breaks[i+1],hb$counts[i],col=rgb(1,0,0,0.5),border=NA)
        rect(hb$breaks[i+1],0,vl,hb$counts[i+1],col=rgb(1,0,0,0.5),border=NA)
        
        abline(v=vr, col=2);mtext(paste(vr),side=3,at=vr,col=2,line=0.5)
        ir=match(T,hb$breaks>vr)
        for (i in ir:(length(hb$breaks)-1)) 
          rect(hb$breaks[i],0,hb$breaks[i+1],hb$counts[i],col=rgb(1,0,0,0.5),border=NA)
        rect(vr, 0,hb$breaks[ir],hb$counts[ir-1],col=rgb(1,0,0,0.5),border=NA)
      })
      }, height=450)
######################################################   
   M=1000
   SimulatedCounts=FALSE;
   rt <- reactive(qbinom(0.95,size = input$N1, prob = input$p1))
   lt <- reactive(qbinom(0.05,size = input$N1, prob = input$p1))
   output$uirt <- renderUI({
     numericInput("rt1",
                  "right tail",
                  value = rt())
   })
   output$uilt <- renderUI({
     numericInput("lt1",
                  "left tail",
                  value = lt())
   })
   output$distPlot1 <- renderPlot({
     # generate binomial distribution
     #input=list();input$N=30;input$p=0.5;input$rt=20;input$lt=10
     
     if (SimulatedCounts){
       x  <- rbinom(M,input$N1, input$p1) 
       # draw the histogram with the specified number of bins
       hb=hist(x, col = 'darkgray', border = 'white', main = "", 
               xlab="num of successes", breaks=round(2*sqrt(input$N1)), density=TRUE)
     } else {
       k = pmax(0,qbinom(0.0001,input$N1,input$p1)):qbinom(0.99999,input$N1,input$p1)
       x  <- dbinom(k,input$N1, input$p1)
       # barplot(x,names=k,col = 'darkgray', border = 'white', main = "", 
       #         xlab="num of successes",space=0);box()
       hb=list()
       hb$counts=x
       hb$breaks = c(min(k)-1,k);#hb$breaks=hb$breaks-min(hb$breaks)
       class(hb)="histogram"
       plot(hb,col = 'darkgray', border = 'white', main = "", 
            xlab="num of successes")
     } 
     grid()
     VR= round(1-pbinom(as.numeric(input$rt1),as.numeric(input$N1), as.numeric(input$p1)),3)
     VL= round(pbinom(as.numeric(input$lt1),as.numeric(input$N1), as.numeric(input$p1)),3)
     vl=input$lt1;vr=input$rt1
     
     try({
       if (vl>min(hb$breaks) & vl<max(hb$breaks)){
         abline(v=vl, col=2);mtext(paste(VL),side=3,at=vl,col=2,line=0.5)
         for (i in 1:(match(T,hb$breaks>vl)-2)) 
           rect(hb$breaks[i],0,hb$breaks[i+1],hb$counts[i],col=rgb(1,0,0,0.5),border=NA)
         rect(hb$breaks[i+1],0,vl,hb$counts[i+1],col=rgb(1,0,0,0.5),border=NA)
       }
       if (vr>min(hb$breaks) & vr<max(hb$breaks)){
         abline(v=vr, col=2);mtext(paste(VR),side=3,at=vr,col=2,line=0.5)
         ir=match(T,hb$breaks>vr)
         for (i in ir:(length(hb$breaks)-1)) 
           rect(hb$breaks[i],0,hb$breaks[i+1],hb$counts[i],col=rgb(1,0,0,0.5),border=NA)
         rect(vr, 0,hb$breaks[ir],hb$counts[ir-1],col=rgb(1,0,0,0.5),border=NA)
       }
     })
   }, height=450)
   
}
# Run the application 
#shinyApp(ui = ui, server = server)

