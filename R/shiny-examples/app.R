library(shiny)
library(shinydashboard)

df1 <- data.frame(a = 1:10, b = 2:11)
df2 <- data.frame(c = 1:10, d = 2:11)


data_frame_objects <- function(objects_name = ls(envir = globalenv())) {
  
  df_objects<- sapply(objects_name, 
                      function(x) is.data.frame(eval(parse(text = x))))
  
  return(objects_name[df_objects])
  
}



dashboardHeader <- dashboardHeader(title = "ladybird-umbrella")

dashboardSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", 
             tabName = "dashboard", 
             icon = icon("dashboard")),
    menuItem("Widgets", 
             icon = icon("th"), 
             tabName = "widgets"),
    menuItem("Charts", 
             icon = icon("bar-chart-o"), 
             startExpanded = FALSE,
             menuSubItem("Sub-item 1", tabName = "subitem1"),
             menuSubItem("Sub-item 2", tabName = "subitem2"))
  ),
  textOutput("res")
)


dashboardBody <- dashboardBody(
  tabItems(
    tabItem("dashboard", 
            "Dashboard tab content", 
            selectInput(inputId = "dataset",
                        label = "Choose a data.frame object:",
                        choices = c("please select", 
                                    data_frame_objects())),
            selectInput(inputId = "weights_col",
                        label = "weights column (required)",
                        choices = ""),
            selectInput(inputId = "observed_col",
                        label = "observed column (optional)",
                        choices = ""),
            selectInput(inputId = "pred1_col",
                        label = "predictions column 1 (optional)",
                        choices = ""),
            selectInput(inputId = "pred2_col",
                        label = "predictions column 2 (optional)",
                        choices = ""),
            actionButton("button", "An action button")),
    tabItem("widgets", "Widgets tab content"),
    tabItem("subitem1", "Sub-item 1 tab content"),
    tabItem("subitem2", "Sub-item 2 tab content")
  )
)


ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    title = "ladybird-umbrella")

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe(print(input$dataset))
  
  observe(print(input$df_columns))
  
  observe(print(input$button))
  
  observe({
  
    if (input$dataset == "please select") {
      
      df_col_choices <- "please select data.frame"
      
    } else {
      
      df_col_choices <- c("please select",
                          colnames(get(input$dataset, envir = globalenv())))
      
    }
    
    updateSelectInput(session, 
                      inputId = "weights_col",
                      label = "weights column (required)",
                      choices = df_col_choices)
    
    updateSelectInput(session, 
                      inputId = "observed_col",
                      label = "observed column (optional)",
                      choices = df_col_choices)
    
    updateSelectInput(session, 
                      inputId = "pred1_col",
                      label = "predictions column 1 (optional)",
                      choices = df_col_choices)
    
    updateSelectInput(session, 
                      inputId = "pred2_col",
                      label = "predictions column 2 (optional)",
                      choices = df_col_choices)
    
  })
  
  #output$input_dataset_colnames <- reactive({
  #  
  #  if (input$dataset == "please select") {
  #    
  #    "no data.frame selected"
  #    
  #  } else {
  #    
  #    colnames(get(input$dataset, envir = globalenv()))
  #    
  #  }})
  
  
  
  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })
}





shinyApp(ui, server)



