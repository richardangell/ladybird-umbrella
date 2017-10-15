# https://stackoverflow.com/questions/37595124/react-to-menuitem-tab-selection


library(shiny)
library(shinydashboard)


convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}


ui <- dashboardPage(
  dashboardHeader(
    title = "Shiny"
  ),
  
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
                convertMenuItem(menuItem("PointA_",tabName="PointA", selected=TRUE,
                                         
                                         checkboxInput("tc", "Test check", value=FALSE)
                ),'PointA')        ,
                convertMenuItem(menuItem("PointB_",tabName="PointB",checkboxInput("tc2", "Test check", value=FALSE)
                ),'PointB') 
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
  
  observe({
    print(input$sbmenu)
    
  })
  
  
}

shinyApp(ui,server)