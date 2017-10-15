# https://stackoverflow.com/questions/37595124/react-to-menuitem-tab-selection

library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(
    title = "Shiny"
  ),
  
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
                menuItem("PointA_",tabName = "PointA") ,
                menuItem("PointB_",tabName = "PointB") 
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("PointA",h1("a")),
      tabItem("PointB",h1("b"))
    )
  )
)


server <- function(input, output) {
  observe(print(input$sbmenu))
}

shinyApp(ui,server)