library(shiny)
library(ggplot2)

server <- function(input, output, session) {
  
  dataUpload<-reactive({
    
    inFile<-input$file1
    print(inFile)
    if(is.null(inFile))
      return(NULL)
    dt_frame = read.csv(inFile$datapath, header=input$header, sep=input$sep)
    updateSelectInput(session, "product", choices = names(dt_frame))
    return(dt_frame)
  })
  
  #output$contents <- renderTable({
  #   dataset<- dataUpload()
  #   dataset
  #})
  output$hist <- renderPlot({
    # we need error check also here
    if(is.null(input$file1))
      return(NULL)
    dataset<- dataUpload()
    hist(dataset[,input$product])
    
  })
  
  
}

ui <- pageWithSidebar(
  headerPanel( "Demand Forecast", "Flowserve"),
  sidebarPanel(
    fileInput('file1', 'Select csv file',accept=c('text/csv')
    ),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator', c(Comma=',',Tab='\t',  Semicolon=';' )
    ),
    tags$hr(),
    selectInput("product", "Select: ","")
  ),
  mainPanel(tableOutput('contents'),
            
            plotOutput("hist")
  )
)


shinyApp(ui = ui, server = server)