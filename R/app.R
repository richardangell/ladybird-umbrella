# status:
# all tabs have the same layout; data import has empty plot box and plot tabs 
#   have drop down box
# need to set df programatically from drop down box selection



library(shinydashboard)


df <- data.frame(a = rnorm(100),
                 b = runif(100),
                 c = rnorm(100),
                 d = runif(100),
                 e = rnorm(100),
                 f = runif(100),
                 g = rnorm(100),
                 h = runif(100))


data_frame_objects <- function(objects_name) {
  
  df_objects<- sapply(objects_name, 
                      function(x) is.data.frame(eval(parse(text = x))))
  
  return(objects_name[df_objects])
  
}


dashboardHeader <- dashboardHeader(title = "ladybird-umbrella")

dashboardSidebar <- dashboardSidebar(sidebarMenuOutput("menu"),
                                     tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }"))))

dashboardBody <- dashboardBody(uiOutput("TABUI"), 
                               # Input: Selector for choosing dataset ----
                               selectInput(inputId = "dataset",
                                           label = "Choose a data.frame object:",
                                           choices = c(data_frame_objects(ls(envir = globalenv())))),
                               plotOutput('plotHist'))



ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    skin = "purple")





plotSingle = function(myData, column){
  
  if (column %in% colnames(myData)) {
    
    hist(myData[[column]])
    
  } else {
    
    return(NULL)
    
  }
  
}




server <- function(input, output, session) {

  #df <- get(input$dataset, globalenv())
    
  #df <- eval(parse(text=input$dataset))
  
  ntabs <- length(colnames(df))
  
  
  output$plotHist <- renderPlot({plotSingle(df, input$channeltab)})
  
  output$TABUI <- renderUI({
    Tabs <- vector("list", ntabs + 1)
    Tabs[[1]] <- tabItem(tabName = "data import",
                         h2("data import"))
    for(i in 1:ntabs){
      Tabs[[i+1]] <- tabItem(tabName = colnames(df)[i],
                             h2(colnames(df)[i]))
    }
    do.call(tabItems, Tabs)
  })
  
  
  output$menu <- renderMenu({
    
    side_bar_items <- lapply(c("data import", colnames(df)),
                             function(x) menuItem(x, 
                                                  tabName = x,
                                                  icon = icon("calendar")))
    
    sidebarMenu(.list = side_bar_items, id = "channeltab")
    
  })
  
  
  
  
  # print which menu item is selected to console
  observe(print(input$channeltab))
  observe(print(input$dataset))
  
  # stop app when web page is closed
  session$onSessionEnded(stopApp)
  
}

shinyApp(ui, server)