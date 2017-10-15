# https://stackoverflow.com/questions/39735032/dynamically-generate-plots-in-conditional-tabs-using-renderui-in-shiny

library(shiny)
library(ggplot2)

channels = c("Affiliate","Email","Media","SEO")
nObs = c(round(runif(1,100,200)))

myData = data.frame(
  Campaign = unlist(lapply(channels, FUN = function(x) paste(x,seq(from=1,to=nObs,by=1),sep=""))), 
  Channel = rep(channels,nObs), 
  Return = runif(nObs*length(channels),50,500), 
  Spend = runif(nObs*length(channels),10,100)
)

plotSingle = function(myData, channelName){
  #hist(myData[[channelName]])
  ggplot(myData[which(myData$Channel==channelName),], aes(x = Spend, y = Return)) +
    geom_point(color="black") +
    theme(panel.background = element_rect(fill = 'grey85'),
          panel.grid.major = element_line(colour = "white"))
}

ui <- fluidPage(
  headerPanel('Plot Testing'),
  mainPanel(    
    uiOutput('mytabs'),
    plotOutput('scatterPlot')
  )
)

server = function(input, output) {
  
  rawData <- reactive({
    myData
  })
  
  output$mytabs = renderUI({
    if(is.null(rawData())){return ()}
    channels = unique(rawData()$Channel)
    myTabs = unname(Map(tabPanel, channels))
    do.call(tabsetPanel, c(myTabs, id="channeltab"))
  })
  
  output$scatterPlot <- renderPlot({
    if(is.null(rawData()) | is.null(input$channeltab)){return ()}
    plotSingle(rawData(), input$channeltab)
  })
  
}

shinyApp(ui, server)