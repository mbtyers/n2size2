
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  D <- reactive({
    (input$conf==0.99)*c(48.707,135.529229,196.2951017,325.8179089,694.4679116,2684.858928,66380.75202) +
      (input$conf==0.95)*c(24.35001031,69.83385388,103.9898307,178.3155987,391.4504252,1543.721381,38421.81) +
      (input$conf==0.9)*c(14.7893,45.79545303,69.92066847,122.361479,272.5531006,1084.149431,27057.47792) +
      (input$conf==0.85)*c(9.74015,33.5685,52.11740329,92.33768116,207.4134585,829.0626252,20722.71022) +
      (input$conf==0.8)*c(6.64835,25.80786976,40.54417323,72.44665423,163.6673542,656.3687191,16423.22022) +
      (input$conf==0.75)*c(4.700876432,20.3312336,32.2203623,57.93630836,131.443365,528.4288547,13232.17423) 
  })
  n2_1 <- reactive({
    ceiling(input$N/(input$n1*(input$N-1)/((input$N-input$n1)*D())+1))
  })
  n2_2 <- reactive({
    ceiling(input$N/(sqrt((input$N-1)/D())+1))
  })
  
  table_out <- reactive({
    relprec <- c(.5,.25,.2,.15,.1,.05,.01)
    df <- data.frame(relprec,n2_1(),n2_2())
    dimnames(df)[[2]] <- c("Relative precision","n2 needed from specified n1","n needed if n1=n2")
    df
  })
  
  output$summary <- renderTable({
    dig.mat <- matrix(c(rep(2,7),rep(0,14)),nrow=7,ncol=3,byrow=T)
    table_out()},digits=matrix(c(rep(2,14),rep(0,14)),nrow=7,ncol=4,byrow=F),include.rownames=F)
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("n2size_for_N_",input$N,"_n1_",input$n1,"_conf_",100*as.numeric(input$conf), '.csv', sep='') },
    content = function(file) {
      write.csv(table_out(), file)
    }
  )
  
  precsim <- eventReactive(input$goforit,{
    whichn2 <- seq(input$n2range1,input$n2range2,by=input$step)
    conftry <- as.numeric(input$conftry)
    precsimcalc <- matrix(NA,nrow=length(whichn2),ncol=(2*length(input$conftry)+1))
    precsimcalc[,1] <- whichn2
    i <- 1
    for(nn22 in whichn2) {
      m2 <- rhyper(input$nsim,input$n1_a,input$N_a-input$n1_a,nn22)
      N.Chap <- (input$n1_a+1)*(nn22+1)/(m2+1) - 1
      for(j in 2:(length(input$conftry)+1)) {
        quantilesN <- quantile(N.Chap,c((1-conftry[j-1])/2,(1+conftry[j-1])/2))
        quantilesRP <- (quantilesN-input$N_a)/input$N_a
        precsimcalc[i,j] <- quantilesRP[1]
        precsimcalc[i,(j+length(input$conftry))] <- quantilesRP[2]
      }
      i <- i+1
    }
    precsimcalc
  })
  
  
  
  Nchapsim <- eventReactive(input$goforit,{
    m2 <- rhyper(input$nsim,input$n1_a,input$N_a-input$n1_a,input$n22plot)
    N.Chap <- (input$n1_a+1)*(input$n22plot+1)/(m2+1) - 1
    N.Chap
  },ignoreNULL=F)
  
  plot1toplot <- function(){#reactive({
    layout(mat=matrix(c(1,2),ncol=1,nrow=2),heights=c(2.5,1))
    cols <- c("#CC0000FF", "#CCCC00FF", "#00CC00FF", "#00CCCCFF", "#0000CCFF", "#CC00CCFF")
    plot(NA,xlim=c(precsim()[1,1],precsim()[1,1]+1*(precsim()[dim(precsim())[1],1]-precsim()[1,1])),
         ylim=c(-isolate(input$precrange),isolate(input$precrange)),xlab="sample size n2",ylab="Simulated relative precision",
         main=c(paste("Anticipated N:",isolate(input$N_a)),paste("Selected n1:",isolate(input$n1_a))))
    grid()
    for(i in 2:(length(isolate(input$conftry))+1)) {
      lines(precsim()[,1],precsim()[,i],col=cols[i-1],lwd=2)
      lines(precsim()[,1],precsim()[,(i+length(isolate(input$conftry)))],col=cols[i-1],lwd=2)
    }
    legend(par("usr")[1]+0.82*(par("usr")[2]-par("usr")[1]),
           par("usr")[4],
           legend=isolate(input$conftry),lwd=2,col=cols,title="conf (1-alpha)")
    
    stepsize <- max(density(Nchapsim())$y)/10/length(isolate(input$conftry))
    plot(density(Nchapsim()),yaxt='n',main=c("Simulated Chapman estimate for",paste("Anticipated N:",isolate(input$N_a)," /  Selected n1:",isolate(input$n1_a)," /  Selected n2:",isolate(input$n22plot))),xlab="Abundance estimate",lwd=2)
    grid()
    abline(h=0,lwd=1)
    axis(side=2,at=0,labels=0)
    for(i in 2:(dim(precsim())[2])) {
      confs <- c((1-as.numeric(isolate(input$conftry[i-1])))/2,(1+as.numeric(isolate(input$conftry[i-1])))/2)
      quantiles <- quantile(Nchapsim(),confs)
      abline(v=quantiles,col=cols[i-1],lwd=2)
    }
    legend(par("usr")[1]+0.82*(par("usr")[2]-par("usr")[1]),
           par("usr")[4],
           legend=isolate(input$conftry),lwd=2,col=cols,title="conf (1-alpha)")
  }#)
    
  output$plot1 <- renderPlot({plot1toplot()})
  
  
  output$downloadPlot <- downloadHandler(
    filename = paste0("simplot_",Sys.Date(),".png"),
    content = function(file) {
      png(file,width=800,height=1000)
      print(plot1toplot())
      dev.off()
    })
  
})
