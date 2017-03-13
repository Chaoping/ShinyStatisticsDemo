library(shiny)

ui = fluidPage(
  titlePanel('Normal Distribution'),
  sidebarLayout(
    sidebarPanel(
      h5(),
      textInput('mean', 'Mean', value = '0'),
      br(),
      br(),
      textInput('sd', 'Standard Deviation', value = '1'),
      br(),
      br(),
      textInput('lb', 'Lower Bound', value = '0'),
      br(),
      br(),
      textInput('ub', 'Upper Bound', value = '0')
    ),
    mainPanel(
      plotOutput('plot'),
      br(),
      br(),
      textOutput('desc')
    )
  )
)



server = shinyServer(function(input, output){
  output$plot = renderPlot({
      mean=as.numeric(input$mean); sd=as.numeric(input$sd)
      lb=as.numeric(input$lb); ub=as.numeric(input$ub)
      
      x <- seq(-4,4,length=100)*sd + mean
      hx <- dnorm(x,mean,sd)
      
      plot(x, hx, type="n", xlab="Values", ylab="",
           main="Normal Distribution", axes=FALSE)
      
      i <- x >= lb & x <= ub
      lines(x, hx)
      polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 
      
      area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
      result <- paste("P(",lb,"< Value <",ub,") =",
                      signif(area, digits=3))
      mtext(result,3)
      axis(1, at=seq(mean-4*sd, mean+4*sd, sd), pos=0)
  })
  
  output$desc = renderText({
    mean=as.numeric(input$mean); sd=as.numeric(input$sd)
    lb=as.numeric(input$lb); ub=as.numeric(input$ub)
    area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
    result <- paste("Congratulations! You beat ",signif(area, digits=3)*100,"% of all users!")
    return(result)

  })

  
})


shinyApp(ui = ui, server = server)