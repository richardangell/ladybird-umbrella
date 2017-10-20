library(shiny)
library(shinydashboard)

df1 <- data.frame(a = 1:10, b = 2:11)
df2 <- data.frame(c = 1:10, d = 2:11)


data_frame_objects <- function(objects_name = ls(envir = globalenv())) {
  
  df_objects<- sapply(objects_name, 
                      function(x) is.data.frame(eval(parse(text = x))))
  
  return(objects_name[df_objects])
  
}







ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets"),
      menuItem("Charts", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Sub-item 1", tabName = "subitem1"),
               menuSubItem("Sub-item 2", tabName = "subitem2")
      )
    ),
    textOutput("res")
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard", 
              "Dashboard tab content", 
              selectInput(inputId = "dataset",
                          label = "Choose a data.frame object:",
                          choices = c("please select", 
                                      data_frame_objects())),
              selectInput(inputId = "weights",
                          label = "Choose a column:",
                          choices = c("please select", 
                                      textOutput("input_dataset_colnames")))),
      tabItem("widgets", "Widgets tab content"),
      tabItem("subitem1", "Sub-item 1 tab content"),
      tabItem("subitem2", "Sub-item 2 tab content")
    )
  )
)

server <- function(input, output, session) {
  
  observe(print(input$dataset))
  
  output$input_dataset_colnames <- reactive({
    
    if (input$dataset == "please select") {
      
      "no data.frame selected"
      
    } else {
      
      colnames(get(input$dataset, envir = globalenv()))
      
    }})
  
  
  
  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })
}

shinyApp(ui, server)