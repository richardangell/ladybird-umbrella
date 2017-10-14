library(shinydashboard)


dashboardHeader <- dashboardHeader(title = "Basic dashboard")

#dashboardSidebar <- dashboardSidebar(
#  sidebarMenu(
#    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#    menuItem("Widgets", tabName = "widgets", icon = icon("th")),
#    menuItem("c", tabName = "c", icon = icon("th")),
#    menuItem("d", tabName = "d", icon = icon("th")),
#    menuItem("e", tabName = "e", icon = icon("th")),
#    menuItem("f", tabName = "f", icon = icon("th")),
#    menuItem("g", tabName = "g", icon = icon("th")),
#    menuItem("h", tabName = "h", icon = icon("th"))
#  )
#)

dashboardSidebar <- dashboardSidebar(sidebarMenuOutput("menu"),
                                     tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }"))))


dashboardBody <- dashboardBody(
  tabItems(
    
    # First tab content
    tabItem(tabName = "a",
            fluidRow(
              box(plotOutput("plot1", height = 250)))),
    
    # Second tab content
    tabItem(tabName = "b",
            h2("Widgets tab content"),
            fluidRow(
              box(plotOutput("plot2", height = 250)))),
    
    tabItem(tabName = "c",
            h2("Widgets tab content"),
            fluidRow(
              box(plotOutput("plot3", height = 250)))),
    
    tabItem(tabName = "d",
            h2("Widgets tab content"),
            fluidRow(
              box(plotOutput("plot4", height = 250)))),
    
    tabItem(tabName = "e",
            h2("Widgets tab content"),
            fluidRow(
              box(plotOutput("plot5", height = 250)))),
    
    tabItem(tabName = "f",
            h2("Widgets tab content"),
            fluidRow(
              box(plotOutput("plot6", height = 250)))),
    
    tabItem(tabName = "g",
            h2("Widgets tab content"),
            fluidRow(
              box(plotOutput("plot7", height = 250)))),
    
    tabItem(tabName = "h",
            h2("Widgets tab content"),
            fluidRow(
              box(plotOutput("plot8", height = 250))))
    
  )
)


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



server <- function(input, output) {

  
  for (i in 1:length(colnames(df))) {
    output[[paste0("plot", i)]] <- renderPlot({hist(df[[colnames(df)[i]]])})
  }
  
  #output$plot1 <- renderPlot({hist(df$a)})
  #output$plot2 <- renderPlot({hist(df$b)})
  #output$plot3 <- renderPlot({hist(df$c)})
  #output$plot4 <- renderPlot({hist(df$d)})
  #output$plot5 <- renderPlot({hist(df$e)})
  #output$plot6 <- renderPlot({hist(df$f)})
  #output$plot7 <- renderPlot({hist(df$g)})
  #output$plot8 <- renderPlot({hist(df$h)})
  
  output$menu <- renderMenu({
    
    side_bar_items <- lapply(colnames(df),
                             function(x) menuItem(x, 
                                                  tabName = x,
                                                  icon = icon("calendar")))
    
    sidebarMenu(.list = side_bar_items)
    
  })
  
}

shinyApp(ui, server)