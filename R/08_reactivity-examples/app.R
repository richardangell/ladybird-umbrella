ui <- shinyUI(fluidPage(
  titlePanel("Title"),
  sidebarLayout(
    sidebarPanel(
    ),
    mainPanel(
      plotOutput("graph", width = "100%", click = "plot_click"),
      verbatimTextOutput("click_info")
    )
  )
) 
)

server <- shinyServer(function(input, output, session) {
  data <- data.frame(x=c(1,2,1,2), y=c(1,1,2,2), 
                     values=c("A","B","C","D"), stringsAsFactors=FALSE)
  
  # Visualization output:  
  observe({
    output$graph <- renderPlot({
      plot(data$x, data$y, pch=data$values)
    })  
  })
  
  
  # interaction click in graph  
  observe({
    if(is.null(input$plot_click$x)) return(NULL)
    click         <- c(input$plot_click$x, input$plot_click$y)
    print(click)
    nearest_point <- which.min(apply(data[,1:2], 1, function(a) sum(((click-a)^2))))
    id <- data$values[nearest_point]
    
    output$click_info <- renderPrint({
      id
    })
    color <- rep("black",length(data$x))
    color[data$values==id] <- "red"
    
    isolate({
      output$graph <- renderPlot({
        plot(data$x, data$y, pch=data$values, col=color)
      }) 
    })
    
  })
})
shinyApp(ui=ui,server=server)