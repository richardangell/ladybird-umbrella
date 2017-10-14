
library(shiny)
library(shinydashboard)

# ========== Dynamic dropdownMenu ==========
# Example message data in a data frame
messageData <- data.frame(
  from = rep(c("Admininstrator", "New User", "Support"),100),
  message = rep(c("Sales are steady this month.",
                  "How do I register?",
                  "The new server is ready."),
                100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  xcol = rnorm(100),
  stringsAsFactors = FALSE
)


dashboardHeader <- dashboardHeader(title = "Dynamic menus",
                                   dropdownMenuOutput("messageMenu"))

dashboardSidebar <- dashboardSidebar(sidebarMenuOutput("menu"),
                                     tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }"))))

dashboardBody <- dashboardBody(fluidRow(box(plotOutput("plot1", height = 250)),
                              box(title = "Controls",
                                  sliderInput("slider", "Number of observations:", 1, 100, 50))))

ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    title = "TITLE")







server <- function(input, output) {
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. messageData
    # is a data frame with two columns, 'from' and 'message'.
    # Also add on slider value to the message content, so that messages update.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(
        from = row[["from"]],
        message = paste(row[["message"]], input$slider)
      )
    })
    
    dropdownMenu(type = "messages", .list = msgs)
    
  })
  
  output$menu <- renderMenu({
    
    side_bar_items <- lapply(colnames(messageData),
                             function(x) menuItem(x, icon = icon("calendar")))
    
    sidebarMenu(style = "position: fixed; overflow: visible;", 
                .list = side_bar_items)
    
  })
  
}

shinyApp(ui, server)