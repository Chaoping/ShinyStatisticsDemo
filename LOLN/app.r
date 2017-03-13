library(shiny)

ui = fluidPage(
  titlePanel('Law of Large Numbers'),
  sidebarLayout(
    sidebarPanel(
      sliderInput('coin', 'Coin tossed:', 0, 100000,value = '0',step = 10),
      br(),
      br(),
      sliderInput('dice', 'Dice tossed', 0,100000,value = '0', step = 10),
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
    coins = sample(c('Head','Tail'),input$coin, replace = T)
    coin = table(coins)
    dices = sample(1:6,input$dice, replace = T)
    dice = table(dices)
    return(list(coin = coin, dice = dice, dices = dices))
  })
  
  output$dist = renderPlot({
    if(input$coin>=1)
      barplot(update()$coin)
    else if(input$dice>=1)
      barplot(update()$dice)
  })
  
  output$desc = renderText({
    if(input$coin>=1)
      paste('Frequency on Head: ',as.numeric(update()$coin[1])/as.numeric(input$coin))
    else if(input$dice>=1)
      paste('Average point: ', mean(update()$dices))
  })

  
})


shinyApp(ui = ui, server = server)