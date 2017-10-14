library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
data = data.table(group = rep(c(1, 3, 6), each = 10), x = rep(1:10, times = 3), value = rnorm(30))

sidebar <- dashboardSidebar(
  uiOutput("Sidebar")
)

body <- dashboardBody(
  uiOutput("TABUI")
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "test tabbed inputs"),
  sidebar,
  body,
  skin = 'green'
)

server <- function(input, output) {
  
  ntabs <- 3
  tabnames <- paste0("tab", 1:ntabs) # "tab1", "tab2", ...
  checkboxnames <- paste0(tabnames, 'group') # "tab1group", "tab2group", ...
  plotnames <- paste0("plot", 1:ntabs) # "plot1", "plot2", ...
  
  output$Sidebar <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], tabName = tabnames[i], icon = icon("dashboard"), selected = i==1)
    }
    do.call(function(...) sidebarMenu(id = 'sidebarMenu', ...), Menus)
  })
  
  output$TABUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = tabnames[i],
                           fluidRow(
                             box(title = "Controls", 
                                 checkboxGroupInput(checkboxnames[i], 'group:', c(1, 3, 6), selected = 6, inline = TRUE), 
                                 width = 4),
                             box(plotOutput(paste0("plot",i)), width = 8)
                           )
      )
    }
    do.call(tabItems, Tabs)
  })
  
  RV <- reactiveValues()
  observe({
    selection <- input[[paste0(input$sidebarMenu, 'group')]]
    RV$plotData <- data[group %in% selection]
  })
  
  for(i in 1:ntabs){
    output[[plotnames[i]]] <- renderPlot({
      plotData <-  RV$plotData 
      p <- ggplot(plotData, aes(x = x, y = value, colour = factor(group))) + 
        geom_line() + geom_point()  
      print(p)
    })
  }
  
}

shinyApp(ui, server)