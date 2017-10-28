#------------------------------------------------------------------------------#
# status: 
# can dynamically generate sub menu items for each variable which react
#   to changes of the input df 
# (uncomment lines 41 onwards to have sub item tabs static but plot that 
#   updates as df does)
# have 1 tab which shows summary plot for df, but doesn't react to changes in 
#   menu sub  item selection might be easier to have multiple tabs - for for 
#   each variable
#------------------------------------------------------------------------------#

library(shiny)
library(shinydashboard)


df1 <- data.frame(a = 1:10, b = 2:11, c = 9:18)

df2 <- data.frame(c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39,
                  c = 10:19, d = 2:11, e = 30:39)


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

sidebar_scroll_text <- ".sidebar { height: 90vh; overflow-y: auto; }"

dashboardHeader <- dashboardHeader(title = "ladybird-umbrella")

dashboardSidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Data Select", 
      tabName = "data_select", 
      icon = icon("th")
    ),
    menuItem(
      "Summary Graphs", 
      icon = icon("bar-chart-o"), 
      tabName = "summary_graphs"
    ),
    radioButtons(
      inputId = "plot_var_check_box",
      label = "Select variable to plot:",
      choices = ""
    ),
    tags$head(tags$style(HTML(sidebar_scroll_text)))
  )
)

dashboardBody <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "data_select",
      h2("Data Select"), 
      box(
        selectInput(
          inputId = "dataset",
          label = "Choose a data.frame object:",
          choices = c("please select", data_frame_objects())
        ),
        width = 4,
        solidHeader = TRUE,
        background = "maroon",
        title = "Select data"),
      box(
        selectInput(
          inputId = "weights_col",
          label = "weights column (required)",
          choices = ""
        ),
        selectInput(
          inputId = "observed_col",
          label = "observed column (optional)",
          choices = ""
        ),
        selectInput(
          inputId = "pred1_col",
          label = "predictions column 1 (optional)",
          choices = ""
        ),
        selectInput(
          inputId = "pred2_col",
          label = "predictions column 2 (optional)",
          choices = ""
        ),
        width = 4,
        solidHeader = TRUE,
        background = "maroon",
        title = "Select summary columns"
      ),
      box(
        actionButton(
          inputId = "button", 
          label = "Calculate variable summaries",
          icon = icon("bar-chart-o")
        ),
        width = 4,
        solidHeader = TRUE,
        #status = "primary",
        background = "maroon",
        title = "Summarise by explanatory variables"
      )
    ),
    tabItem(
      "summary_graphs", 
      h2("Summary Graphs"),
      textOutput("selected_variable"),
      box(plotOutput("plot"))
    )
  )
)


ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    title = "ladybird-umbrella",
                    skin = "blue")

server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe(print(paste0("dataset: ", input$dataset)))
  
  observe(print(paste0("weights_col: ", input$weights_col)))
  
  observe(print(paste0("observed_col: ", input$observed_col)))
  
  observe(print(paste0("pred1_col: ", input$pred1_col)))
  
  observe(print(paste0("pred2_col: ", input$pred2_col)))
  
  summary_reactive <- eventReactive(
    input$button, {
      summarise_df_cols(
        get(
          input$dataset, envir = globalenv()
        ),
        colnames(
          get(
            input$dataset, envir = globalenv()
          )
        )
      )
    }
  )
  
  output$plot <- renderPlot({
    
    if (input$plot_var_check_box != "please select" & 
          input$plot_var_check_box != "please select data.frame") {
      
      plot(summary_reactive()[[input$plot_var_check_box]], 
           summary_reactive()[[input$plot_var_check_box]] * 2)
      
    }
    
  })
  
  output$selected_variable <- renderText({ 
    
    input$plot_var_check_box 
    
  })

  
  observeEvent(input$dataset, {
  
     # need || as second condition will error if first one is true
     if (length(input$dataset) == 0 || input$dataset == "please select") {
       
       df_col_choices <- "please select data.frame"
       
       df_col_choices_p <- df_col_choices
       
       choices_w <- df_col_choices
       
       choices_o <- df_col_choices
       
       choices_p1 <- df_col_choices
       
       choices_p2 <- df_col_choices
       
       #--------------------------------------------------------------------------#
       # else if the input dataset has been selected ----
       #--------------------------------------------------------------------------#
       
     } else {
       
       default_select <- c("please select data.frame", "please select")
       
       selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
       
       df_col_choices <- selected_df_cols
       
       df_col_choices_p <- c("please select", selected_df_cols)
       
       choices_w <- df_col_choices_p
       
       choices_o <- df_col_choices_p
       
       choices_p1 <- df_col_choices_p
       
       choices_p2 <- df_col_choices_p
     }
     
     updateSelectInput(
       session, 
       inputId = "weights_col",
       label = "weights column (required)",
       choices = choices_w
     )
     
     updateSelectInput(
       session, 
       inputId = "observed_col",
       label = "observed column (optional)",
       choices = choices_o
     )
     
     updateSelectInput(
       session, 
       inputId = "pred1_col",
       label = "predictions column 1 (optional)",
       choices = choices_p1
     )
     
     updateSelectInput(
       session, 
       inputId = "pred2_col",
       label = "predictions column 2 (optional)",
       choices = choices_p2
     )
     
     updateRadioButtons(
       session,
       inputId = "plot_var_check_box",
       label = "Select variable to plot:",
       choices = df_col_choices
     )
     
   })
  
  observeEvent(input$weights_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$weights_col %in% default_select) {
      
      selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
      
      selected_w <- input$weights_col
      
      selected_o <- input$observed_col
      
      selected_p1 <- input$pred1_col
      
      selected_p2 <- input$pred2_col
      
      reduced_df_cols <- setdiff(selected_df_cols,
                                 c(selected_w,
                                   selected_o,
                                   selected_p1,
                                   selected_p2))
      
      choices_w <- setdiff(selected_df_cols,
                           c(selected_o,
                             selected_p1,
                             selected_p2))
      
      choices_o <- setdiff(selected_df_cols,
                           c(selected_w,
                             selected_p1,
                             selected_p2))
      
      choices_p1 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p2))
      
      choices_p2 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p1))
      
      updateSelectInput(
        session, 
        inputId = "weights_col",
        label = "weights column (required)",
        choices = c("please select", choices_w),
        selected = selected_w
      )
      
      updateSelectInput(
        session, 
        inputId = "observed_col",
        label = "observed column (optional)",
        choices = c("please select", choices_o),
        selected = selected_o
      )
      
      updateSelectInput(
        session, 
        inputId = "pred1_col",
        label = "predictions column 1 (optional)",
        choices = c("please select", choices_p1),
        selected = selected_p1
      )
      
      updateSelectInput(
        session, 
        inputId = "pred2_col",
        label = "predictions column 2 (optional)",
        choices = c("please select", choices_p2),
        selected = selected_p2
      )
      
      updateRadioButtons(
        session,
        inputId = "plot_var_check_box",
        label = "Select variable to plot:",
        choices = reduced_df_cols
      )
        
    }
    
  })

  observeEvent(input$observed_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$observed_col %in% default_select) {
      
      selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
      
      selected_w <- input$weights_col
      
      selected_o <- input$observed_col
      
      selected_p1 <- input$pred1_col
      
      selected_p2 <- input$pred2_col
      
      reduced_df_cols <- setdiff(selected_df_cols,
                                 c(selected_w,
                                   selected_o,
                                   selected_p1,
                                   selected_p2))
      
      choices_w <- setdiff(selected_df_cols,
                           c(selected_o,
                             selected_p1,
                             selected_p2))
      
      choices_o <- setdiff(selected_df_cols,
                           c(selected_w,
                             selected_p1,
                             selected_p2))
      
      choices_p1 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p2))
      
      choices_p2 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p1))
      
      updateSelectInput(
        session, 
        inputId = "weights_col",
        label = "weights column (required)",
        choices = c("please select", choices_w),
        selected = selected_w
      )
      
      updateSelectInput(
        session, 
        inputId = "observed_col",
        label = "observed column (optional)",
        choices = c("please select", choices_o),
        selected = selected_o
      )
      
      updateSelectInput(
        session, 
        inputId = "pred1_col",
        label = "predictions column 1 (optional)",
        choices = c("please select", choices_p1),
        selected = selected_p1
      )
      
      updateSelectInput(
        session, 
        inputId = "pred2_col",
        label = "predictions column 2 (optional)",
        choices = c("please select", choices_p2),
        selected = selected_p2
      )
      
      updateRadioButtons(
        session,
        inputId = "plot_var_check_box",
        label = "Select variable to plot:",
        choices = reduced_df_cols
      )
      
    }
    
  })
  
  observeEvent(input$pred1_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$pred1_col %in% default_select) {
      
      selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
      
      selected_w <- input$weights_col
      
      selected_o <- input$observed_col
      
      selected_p1 <- input$pred1_col
      
      selected_p2 <- input$pred2_col
      
      reduced_df_cols <- setdiff(selected_df_cols,
                                 c(selected_w,
                                   selected_o,
                                   selected_p1,
                                   selected_p2))
      
      choices_w <- setdiff(selected_df_cols,
                           c(selected_o,
                             selected_p1,
                             selected_p2))
      
      choices_o <- setdiff(selected_df_cols,
                           c(selected_w,
                             selected_p1,
                             selected_p2))
      
      choices_p1 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p2))
      
      choices_p2 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p1))
      
      updateSelectInput(
        session, 
        inputId = "weights_col",
        label = "weights column (required)",
        choices = c("please select", choices_w),
        selected = selected_w
      )
      
      updateSelectInput(
        session, 
        inputId = "observed_col",
        label = "observed column (optional)",
        choices = c("please select", choices_o),
        selected = selected_o
      )
      
      updateSelectInput(
        session, 
        inputId = "pred1_col",
        label = "predictions column 1 (optional)",
        choices = c("please select", choices_p1),
        selected = selected_p1
      )
      
      updateSelectInput(
        session, 
        inputId = "pred2_col",
        label = "predictions column 2 (optional)",
        choices = c("please select", choices_p2),
        selected = selected_p2
      )
      
      updateRadioButtons(
        session,
        inputId = "plot_var_check_box",
        label = "Select variable to plot:",
        choices = reduced_df_cols
      )
      
    }
    
  })
  
  
  observeEvent(input$pred2_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$pred2_col %in% default_select) {
      
      selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
      
      selected_w <- input$weights_col
      
      selected_o <- input$observed_col
      
      selected_p1 <- input$pred1_col
      
      selected_p2 <- input$pred2_col
      
      reduced_df_cols <- setdiff(selected_df_cols,
                                 c(selected_w,
                                   selected_o,
                                   selected_p1,
                                   selected_p2))
      
      choices_w <- setdiff(selected_df_cols,
                           c(selected_o,
                             selected_p1,
                             selected_p2))
      
      choices_o <- setdiff(selected_df_cols,
                           c(selected_w,
                             selected_p1,
                             selected_p2))
      
      choices_p1 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p2))
      
      choices_p2 <- setdiff(selected_df_cols,
                            c(selected_o,
                              selected_w,
                              selected_p1))
      
      updateSelectInput(
        session, 
        inputId = "weights_col",
        label = "weights column (required)",
        choices = c("please select", choices_w),
        selected = selected_w
      )
      
      updateSelectInput(
        session, 
        inputId = "observed_col",
        label = "observed column (optional)",
        choices = c("please select", choices_o),
        selected = selected_o
      )
      
      updateSelectInput(
        session, 
        inputId = "pred1_col",
        label = "predictions column 1 (optional)",
        choices = c("please select", choices_p1),
        selected = selected_p1
      )
      
      updateSelectInput(
        session, 
        inputId = "pred2_col",
        label = "predictions column 2 (optional)",
        choices = c("please select", choices_p2),
        selected = selected_p2
      )
      
      updateRadioButtons(
        session,
        inputId = "plot_var_check_box",
        label = "Select variable to plot:",
        choices = reduced_df_cols
      )
      
    }
    
  })
}





shinyApp(ui, server)