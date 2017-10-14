ui <- shinyUI(fluidPage(
  
  titlePanel("Reactivity"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("caption", "Caption:", "Data Summary"),
      textInput("TEXT1", "tEXT:", "Data Summary"), 
      actionButton("goButton", "Go")
    ),
    
    mainPanel(
      h3(textOutput("caption", container = span)),
      h3(textOutput("TEXT1", container = span))
    )
  )
)
)


server <- shinyServer(function(input, output) {
  
  cap <- eventReactive(input$goButton, {
    input$caption
  })
  
  text <- eventReactive(input$goButton, {
    input$TEXT1
  })
  
  output$caption <- renderText({
    cap()
  })
  
  output$TEXT1 <- renderText({
    text()
  })
  
})

shinyApp(ui=ui,server=server)