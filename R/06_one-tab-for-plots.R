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

  observe({
    
    # note, || to only evaluate first condition, second condition will error if
    #   first one is true
    if (length(input$dataset) == 0 || input$dataset == "please select") {
      
      df_col_choices <- "please select data.frame"
    
      df_col_choices_w_please_sel <- df_col_choices
      
    } else {
      
      selected_df_cols <- colnames(get(input$dataset, envir = globalenv()))
      
      df_col_choices <- selected_df_cols
      
      df_col_choices_w_please_sel <- c("please select", selected_df_cols)
      
    }
    
    updateSelectInput(
      session, 
      inputId = "weights_col",
      label = "weights column (required)",
      choices = df_col_choices_w_please_sel
    )
    
    updateSelectInput(
      session, 
      inputId = "observed_col",
      label = "observed column (optional)",
      choices = df_col_choices_w_please_sel
    )
    
    updateSelectInput(
      session, 
      inputId = "pred1_col",
      label = "predictions column 1 (optional)",
      choices = df_col_choices_w_please_sel
    )
    
    updateSelectInput(
      session, 
      inputId = "pred2_col",
      label = "predictions column 2 (optional)",
      choices = df_col_choices_w_please_sel
    )
  
    updateRadioButtons(
      session,
      inputId = "plot_var_check_box",
      label = "Select variable to plot:",
      choices = df_col_choices
    )
    
  })
  
  observeEvent(
    input$weights_col, {
      if (length(input$dataset) != 0 && input$dataset != "please select") {
        if (input$weights_col == "please select data.frame") {
          
          var_select_options <- "please select data.frame"
          
          var_select_options_please <- var_select_options
          
        } else if (input$weights_col == "please select") {
          
          var_select_options <- colnames(get(input$dataset, 
                                             envir = globalenv()))
          
          var_select_options_please <- c("please select", var_select_options)
          
        } else {
          
          var_select_options <- setdiff(colnames(get(input$dataset, 
                                                     envir = globalenv())),
                                        input$weights_col)
          
          var_select_options_please <- c("please select", var_select_options)
          
        }
        
        updateSelectInput(
          session, 
          inputId = "observed_col",
          label = "observed column (optional)",
          choices = var_select_options_please
        )
        
        updateSelectInput(
          session, 
          inputId = "pred1_col",
          label = "predictions column 1 (optional)",
          choices = var_select_options_please
        )
        
        updateSelectInput(
          session, 
          inputId = "pred2_col",
          label = "predictions column 2 (optional)",
          choices = var_select_options_please
        )
        
        updateRadioButtons(
          session,
          inputId = "plot_var_check_box",
          label = "Select variable to plot:",
          choices = var_select_options
        )
        
      }
      
    }
    
  )
  
}





shinyApp(ui, server)