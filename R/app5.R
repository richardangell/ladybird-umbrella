# status: (modified from app2)
# can dynamically generate sub menu items for each variable which react
#   to changes of the input df 
# (uncomment lines 41 onwards to have sub item tabs static but plot that updates as df does)
# have 1 tab which shows summary plots, but doesn't react to changes in menu sub item selection
#   might be easier to have multiple tabs - for for each variable

library(shiny)
library(shinydashboard)

df1 <- data.frame(a = 1:10, b = 2:11, c = 9:18)
df2 <- data.frame(c = 10:19, d = 2:11, e = 30:39)


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

dashboardBody <- dashboardBody(uiOutput("tabs"))

#dashboardBody <- dashboardBody(
#  tabItems(
#    tabItem("dashboard", 
#            "Dashboard tab content", 
#            selectInput(inputId = "dataset",
#                        label = "Choose a data.frame object:",
#                        choices = c("please select", 
#                                    data_frame_objects())),
#            selectInput(inputId = "weights_col",
#                        label = "weights column (required)",
#                        choices = ""),
#            selectInput(inputId = "observed_col",
#                        label = "observed column (optional)",
#                        choices = ""),
#            selectInput(inputId = "pred1_col",
#                        label = "predictions column 1 (optional)",
#                        choices = ""),
#            selectInput(inputId = "pred2_col",
#                        label = "predictions column 2 (optional)",
#                        choices = ""),
#            actionButton("button", "Calculate variable summaries")),
#    tabItem("widgets", "Widgets tab content"),
#    tabItem("summary_graphs", "summary graphs tab content"),#, plotOutput("plot")),
#    tabItem("subitem1", "Sub-item 1 tab content", plotOutput("plot")),
#    tabItem("subitem2", "Sub-item 2 tab content")
#  )
#)


ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    title = "ladybird-umbrella")

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe(print(paste0("channeltab: ", input$channeltab)))
  
  observe(print(paste0("dataset: ", input$dataset)))
  
  observe(print(paste0("weights_col: ", input$weights_col)))
  
  observe(print(paste0("observed_col: ", input$observed_col)))
  
  observe(print(paste0("pred1_col: ", input$pred1_col)))
  
  observe(print(paste0("pred2_col: ", input$pred2_col)))
  
  summary_reactive <- eventReactive(input$button, 
                                    {summarise_df_cols(get(input$dataset, envir = globalenv()),
                                                       colnames(get(input$dataset, envir = globalenv())))})
  
  output$plot <- renderPlot({plot(summary_reactive()[[1]], 
                                  summary_reactive()[[2]])})
  
  observe({
    
    # note, || to only evaluate first condition, second condition will error if
    #   first one is true
    if (length(input$dataset) == 0 || input$dataset == "please select") {
      
      df_col_choices <- "please select data.frame"
      
      dynamic_drop_down <- list(menuSubItem("no df selected", 
                                            tabName = "no_df_selected"))
      
      dynamic_tabs <- list(tabItem("no df selected", 
                                   paste("no df selected", "summary graphs tab content")))
      
    } else {
      
      selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
      
      df_col_choices <- c("please select", selected_df_cols)
      
      dynamic_drop_down <- lapply(selected_df_cols,
                                  function(x) menuSubItem(x, 
                                                          tabName = x))
      
      dynamic_tabs <- lapply(selected_df_cols,
                             function(x) tabItem(x, 
                                                 paste(x, "summary graphs tab content")))
      
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
                                        tabName = "summary_graphs",
                                        dynamic_drop_down)), 
                  id = "channeltab")
      
    })
      
    output$tabs <- renderUI({
      
      tabs_list <- list(tabItem("dashboard", 
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
                        tabItem("widgets", "Widgets tab content"),
                        #tabItem("summary_graphs", "summary graphs tab content")
                        tabItem("summary_graphs", "summary graphs tab content", plotOutput("plot"))
      )
      
      do.call(tabItems, c(tabs_list, dynamic_tabs))
      
    })
    
  })
  
  

  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })
  
}





shinyApp(ui, server)



