library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(title = "Controls",
                    sliderInput("slider1", "Number of observations:", 1, 100, 50)))),
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content"),
              fluidRow(
                box(plotOutput("plot2", height = 250)),
                box(title = "Controls",
                    sliderInput("slider2", "Number of observations:", 1, 100, 50)))
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider1)]
    hist(data)
  })
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider2)]
    hist(data)
  })

}

shinyApp(ui, server)