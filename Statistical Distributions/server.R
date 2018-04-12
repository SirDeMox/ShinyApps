#cr Markus Löcher, Max Philipp, HWR Berlin

library(shiny)
library(ggplot2, quietly=TRUE)
library(dplyr, quietly=TRUE)

i <- 0
n <- 0
selectplots <- cbind.data.frame(rnorm=rnorm(30000),rchisq=rchisq(30000,10),rpois=rpois(30000,7),rbeta=rbeta(30000,.5,.5),rbinom=rbinom(30000,20,0.3),stringsAsFactors=FALSE)
# Server
shinyServer(function(input, output) {
  
rv <- reactiveValues(s =  cbind.data.frame(value=rnorm(n),group=rep(1,n)))
m <- reactive({rv$s %>% group_by(group) %>% summarise(mean=mean(value))})


dist <- reactive({input$dist})
mean1 <- reactive({input$mean1})
sd <- reactive({input$sd})
binomsize <- reactive({input$binomsize})

arg1 <- reactive({
  ifelse(input$dist=="rnorm",input$mean1,
            ifelse(input$dist=="rbinom",input$binomsize,
                   ifelse(input$dist=="rbeta",input$betashape1,
                          ifelse(input$dist=="rchisq",input$chidf,
                                  ifelse(input$dist=="rpois",input$poislamb,NULL)))))
})

arg2 <- reactive({
  ifelse(input$dist=="rnorm",input$sd,
         ifelse(input$dist=="rbeta",input$betashape2,
                ifelse(input$dist=="rbinom",input$binomprob,NULL)))
})

observeEvent(input$go, {
  i <<- i+1
  
  if(dist() %in% c("rnorm","rbinom","rbeta")){
  
  rv$s <- bind_rows(isolate({rv$s}),cbind.data.frame(value=do.call(dist(),list(input$obs,
                                                                               arg1(),
                                                                               arg2())),group=rep(i,input$obs)))
  }
  if(dist() %in% c("rpois","rchisq")){
    
    rv$s <- bind_rows(isolate({rv$s}),cbind.data.frame(value=do.call(dist(),list(input$obs,
                                                                                 arg1())),group=rep(i,input$obs)))
  }
  
})

#cbind.data.frame(value=rnorm(input$obs),group=rep(i,input$obs))
#observeEvent(input$go, {
m <<-  reactive({rv$s %>% group_by(group) %>% summarise(mean=mean(value))})
#})

pm <- reactive({input$pm})
#dispnorm <- reactive({input$dispnorm})
fix <- reactive({input$fix})
dense <- reactive({input$dense})
outallowed <- reactive({nrow(rv$s)})
hist <- reactive({input$hist})



observeEvent(input$fill,{
  if(i<input$max){
  i <<- i+1
  neededevents <- input$max -i +1

  if(dist() %in% c("rnorm","rbinom","rbeta")){
    rv$s <- bind_rows(
      isolate({rv$s}),cbind.data.frame(
        value=do.call(dist(),list(input$obs*neededevents,arg1(),
                                  arg2())), # dist () is a reactive object not a function
        group=rep(i:input$max,each=input$obs)))
  }
  if(dist() %in% c("rpois","rchisq")){
    rv$s <- bind_rows(
      isolate({rv$s}),cbind.data.frame(
        value=do.call(dist(),list(input$obs*neededevents,arg1())), # dist () is a reactive object not a function
        group=rep(i:input$max,each=input$obs)))
    
  }

    
i <<- input$max

}}
)

observeEvent(input$clean, {
  i <<- 0
  rv$s <- cbind.data.frame(value=rnorm(n),group=rep(1,n))
})

output$selectPlot <- renderPlot({
  # if (outallowed()>0) {
  #   return(NULL)
  # } 
  g3 <- ggplot(selectplots)+
    {if(dist()=="rnorm")geom_histogram(aes(rnorm),color="black",fill="lightblue")}+
    {if(dist()=="rpois")geom_histogram(aes(rpois),bins=11,color="black",fill="lightblue")}+
    {if(dist()=="rbeta")geom_histogram(aes(rbeta),color="black",fill="lightblue")}+
    {if(dist()=="rbinom")geom_histogram(aes(rbinom),bins=15,color="black",fill="lightblue")}+
    {if(dist()=="rchisq")geom_histogram(aes(rchisq),bins=11,color="black",fill="lightblue")}+
    labs(title=paste("30000 observations of ",dist(),sep=""))+theme_bw()
  print(g3)
})

output$distPlot <- renderPlot({
  if (outallowed()==0) {
    return(NULL)
  } 
      g1 <- ggplot(rv$s %>% filter(group>=max(group)-20))+
      geom_line(aes(value,group,group=group), alpha=0.5)+
      geom_point(aes(value,group),color="black",size=3)+
      geom_point(aes(value,group),color="lightblue")+
      geom_point(data=rv$s %>% filter(group>=max(group)-20) %>% group_by(group) %>% summarise(value=mean(value)),  aes(value,group),color="red")+
      scale_y_reverse(limits=c(max(rv$s$group),max(max(rv$s$group)-20,1)),breaks=seq(max(rv$s$group),1,-1))+
      {if(dist()=="rnorm")scale_x_continuous(
        limits=c(isolate({mean1()})-3*isolate({sd()}),isolate({mean1()})+3*isolate({sd()})),
        breaks=seq(floor(isolate({mean1()})-3*isolate({sd()})),floor(isolate({mean1()})+3*isolate({sd()})),isolate({sd()})/2))}+
      {if(dist()=="rbinom")scale_x_continuous(limits=c(0,binomsize()),breaks=seq(0,binomsize(),1))}+
      {if(dist()=="rpois")scale_x_continuous(limit=c(0,isolate({input$poislamb})*2.5))}+
      theme_bw()
      print(g1)
  })

output$histPlot <- renderPlot({
  if (outallowed()==0) {
    return(NULL)
  } 
  
  g2 <- ggplot(m())+
  {if(hist())geom_histogram(aes(mean),bins=ifelse(dist()=="rnorm",31,
                                                  ifelse(dist()=="rbinom",binomsize()+1,
                                                         ifelse(dist()=="rpois",input$poislamb*2.5,31))), color="black", fill="lightblue")}+
    {if(!hist())geom_histogram(aes(mean,y=..density..),bins=ifelse(dist()=="rnorm",31,
                                                                   ifelse(dist()=="rbinom",binomsize()+1,
                                                                          ifelse(dist()=="rpois",input$poislamb*2.5,31))), color="black", fill="lightblue")}+
    {if(dense())geom_density(aes(mean, y=..density..),alpha=.2, fill="#FF6666")}+
    {if(pm())geom_vline(xintercept = mean(m()$mean),color="darkred",linetype=1,size=1)}+
    {if(pm())geom_vline(xintercept = mean(rv$s[rv$s$group==max(rv$s$group),1]),color="red",linetype=2)}+
    
    {if(input$dispnorm)stat_function(fun = dnorm,n=200,args=list(mean=input$mean1,sd=input$sd))} +
    {if(input$dispbeta)stat_function(fun = dbeta,n=200,args=list(shape1=input$betashape1,shape2=input$betashape2))} +
    {if(input$dispchi)stat_function(fun = dchisq,n=200,args=list(df=input$chidf))} +
    
    {if(fix())scale_y_continuous(limits=c(0,max(nrow(rv$s)/(floor(nrow(rv$s)/max(rv$s$group))*3.5),6)))}+
    {if(dist()=="rnorm")scale_x_continuous(
      limits=c(isolate({mean1()})-3*isolate({sd()}),isolate({mean1()})+3*isolate({sd()})),
      breaks=seq(floor(isolate({mean1()})-3*isolate({sd()})),floor(isolate({mean1()})+3*isolate({sd()})),isolate({sd()})/2))}+
    {if(dist()=="rbinom")scale_x_continuous(limits=c(0,binomsize()),breaks=seq(0,binomsize(),1))}+
    {if(dist()=="rpois")scale_x_continuous(limit=c(0,isolate({input$poislamb})*2.5))}+
    theme_bw()
  print(g2)
  })

})
