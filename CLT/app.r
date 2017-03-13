library(shiny)

ui = fluidPage(
  titlePanel('5,000 people conduct this same trial'),
  sidebarLayout(
    sidebarPanel(
      h5(),
      sliderInput('coin', 'coin tossed', 0, 200,value = '0',step = 1),
      br(),
      br(),
      sliderInput('dice', 'dice tossed', 0,200,value = '0', step = 1),
      br(),
      br()
    ),
    mainPanel(
      plotOutput('dist'),
      br(),
      br(),
      textOutput('desc')
    )
  )
)



server = shinyServer(function(input, output){
  update = reactive({
    ## allocate memory
    coin = numeric(5000)
    dice = numeric(5000)
    
    if(input$coin>=1){
      for(i in 1:5000){
        coin[i] = mean(sample(0:1, input$coin, replace = T))
      }
      return(coin)
    }else if(input$dice >= 1){
      for(i in 1:5000){
        dice[i] = mean(sample(1:6, input$dice, replace = T))
      }
      return(dice)
    }
    
  })
  
  output$dist = renderPlot({
    if(input$coin >=1 | input$dice >= 1)
      hist(update(), col = 'red')
  })
  
  output$desc = renderText({
    if(input$coin>=1)
      paste('')
    else if(input$dice>=1)
      paste('')
  })

  
})


shinyApp(ui = ui, server = server)