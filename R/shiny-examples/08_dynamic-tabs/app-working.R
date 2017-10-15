library(shinydashboard)

dashboardHeader <- dashboardHeader(title = "Basic dashboard")

dashboardSidebar <- dashboardSidebar(sidebarMenuOutput("menu"),
                                     tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }"))))

dashboardBody <- dashboardBody(uiOutput("TABUI"), plotOutput('plotHist'))



ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    skin = "purple")


df <- data.frame(a = rnorm(100),
                 b = runif(100),
                 c = rnorm(100),
                 d = runif(100),
                 e = rnorm(100),
                 f = runif(100),
                 g = rnorm(100),
                 h = runif(100))


plotSingle = function(myData, column){
  
  hist(myData[[column]])

}




server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  ntabs <- length(colnames(df))
  
  rawData <- reactive({
    df
  })
  
  output$plotHist <- renderPlot({plotSingle(df, input$channeltab)})
  
  output$TABUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = colnames(df)[i],
                           h2(colnames(df)[i]))#,
                           #fluidRow(box(plotOutput("plotHist"))))
    }
    do.call(tabItems, Tabs)
  })
  
  
  output$menu <- renderMenu({
    
    side_bar_items <- lapply(colnames(df),
                             function(x) menuItem(x, 
                                                  tabName = x,
                                                  icon = icon("calendar")))
    
    sidebarMenu(.list = side_bar_items, id = "channeltab")
    
  })
  
  


  # print which menu item is selected to console
  observe(print(input$channeltab))
  
}

shinyApp(ui, server)