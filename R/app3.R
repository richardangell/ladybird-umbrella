# status:
# can dynamically generate sub menu items for each variable which react
#   to changes of the input df but it doesn't seem possible to dynamically
#   add some tabs to existing tabs
# see the following;
# https://groups.google.com/forum/#!topic/shiny-discuss/eTuY5Ly0bJM
# https://stackoverflow.com/questions/35020810/dynamically-creating-tabs-with-plots-in-shiny-without-re-creating-existing-tabs/
# https://stackoverflow.com/questions/43661006/dynamically-creating-tabs-in-shiny-dashboard
# easier solution is to have 1 tab for charts and have a single plpt
#   that changes to the variable selected from drop down

# actually might be possible;
#  https://groups.google.com/forum/#!topic/shiny-discuss/jIiD6HMgLlk



library(shiny)
library(shinydashboard)

df1 <- data.frame(a = 1:10, b = 2:11)
df2 <- data.frame(c = 10:19, d = 2:11)


data_frame_objects <- function(objects_name = ls(envir = globalenv())) {
  
  df_objects<- sapply(objects_name, 
                      function(x) is.data.frame(eval(parse(text = x))))
  
  return(objects_name[df_objects])
  
}


summarise_df_cols <- function(df, cols) {
  
  summary_results <- list()
  
  for (col in cols) {
    
    # change this for actual summary fcn later
    summary_results[[col]] <- mean(df[[col]])
    
  }
  
  return(summary_results)
  
}


dashboardHeader <- dashboardHeader(title = "ladybird-umbrella")

#dashboardSidebar <- dashboardSidebar(
#  sidebarMenu(
#    menuItem("Dashboard", 
#             tabName = "dashboard", 
#             icon = icon("dashboard")),
#    menuItem("Widgets", 
#             icon = icon("th"), 
#             tabName = "widgets"),
#    menuItem("Charts", 
#             icon = icon("bar-chart-o"), 
#             startExpanded = FALSE,
#             list(menuSubItem("Sub-item 1", tabName = "subitem1"),
#                  menuSubItem("Sub-item 2", tabName = "subitem2")))
#  ),
#  textOutput("res"),
#  tags$head(tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }")))
#)

dashboardSidebar <- dashboardSidebar(sidebarMenuOutput("menu"))



dashboardBody <- dashboardBody(
  tabItems(
    #tabItem("dashboard", 
    #        "Dashboard tab content", 
    #        selectInput(inputId = "dataset",
    #                    label = "Choose a data.frame object:",
    #                    choices = c("please select", 
    #                                data_frame_objects())),
    #        selectInput(inputId = "weights_col",
    #                    label = "weights column (required)",
    #                    choices = ""),
    #        selectInput(inputId = "observed_col",
    #                    label = "observed column (optional)",
    #                    choices = ""),
    #        selectInput(inputId = "pred1_col",
    #                    label = "predictions column 1 (optional)",
    #                    choices = ""),
    #        selectInput(inputId = "pred2_col",
    #                    label = "predictions column 2 (optional)",
    #                    choices = ""),
    #        actionButton("button", "Calculate variable summaries")),
    #tabItem("widgets", "Widgets tab content"),
    uiOutput("tabs")
    #tabItem("subitem1", "Sub-item 1 tab content", plotOutput("plot")),
    #tabItem("subitem2", "Sub-item 2 tab content")
  )
)


ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    title = "ladybird-umbrella")

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe(print(input$dataset))
  
  observe(print(input$weights_col))
  
  observe(print(input$observed_col))
  
  observe(print(input$pred1_col))
  
  observe(print(input$pred2_col))
  
  summary_reactive <- eventReactive(input$button, 
                                    {summarise_df_cols(get(input$dataset, envir = globalenv()),
                                                       colnames(get(input$dataset, envir = globalenv())))})
  
  output$plot <- renderPlot({plot(summary_reactive()[[1]], 
                                  summary_reactive()[[2]])})
  
  observe({
    
    if (input$dataset == "please select") {
      
      df_col_choices <- "please select data.frame"
      
      dynamic_drop_down <- list(menuSubItem("no df selected", 
                                            tabName = "no_df_selected"))
      
      dynamic_tabs <- list(tabItem(tabName = "no_df_selected",
                                   h2("no df selected")))
      
    } else {
      
      selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
      
      df_col_choices <- c("please select", selected_df_cols)
      
      dynamic_drop_down <- lapply(selected_df_cols,
                                  function(x) menuSubItem(x, tabName = x))
      
      dynamic_tabs <- lapply(selected_df_cols,
                             function(x) tabItem(tabName = x,
                                                 h2(x)))
      
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
    
    output$menu <- renderMenu({
      
      sidebarMenu(.list = list(menuItem("Dashboard", 
                                        tabName = "dashboard", 
                                        icon = icon("dashboard")),
                               menuItem("Widgets", 
                                        icon = icon("th"), 
                                        tabName = "widgets"),
                               menuItem("Charts", 
                                        icon = icon("bar-chart-o"), 
                                        startExpanded = FALSE,
                                        dynamic_drop_down)), 
                  id = "channeltab")
      
    })
    
    
    set_tabs <- list(
      
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
              actionButton("button", "Calculate variable summaries")),
      
      tabItem("widgets", "Widgets tab content")
      
    )
    
    output$tabs <- renderUI({
      
      do.call(tabItems, c(set_tabs, dynamic_tabs))
      
    })
    
    
  })
  
  
  
  
  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })
  
}





shinyApp(ui, server)



