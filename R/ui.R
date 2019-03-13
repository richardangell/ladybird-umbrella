
dashboardHeader <- dashboardHeader(
  title = "ladybird-umbrella"
)

sidebar_scroll_html <- ".sidebar { height: 90vh; overflow-y: auto; }"

dashboardSidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      text = "Data Select", 
      tabName = "data_select", 
      icon = icon("th")
    ),
    menuItem(
      "Summary graphs [show/hide]", 
      icon = icon("bar-chart-o"), 
      expandedName = "summary_graphs_expanded",
      menuSubItem('Summary graphs',
                  tabName = 'summary_graphs',
                  icon = icon("bar-chart-o")
      ),
      radioButtons(
        inputId = "plot_var_check_box",
        label = "Select variable to display:",
        choices = "" 
      )
    ),
    tags$head(tags$style(HTML(sidebar_scroll_html)))  
  ),
  width = 400
)






drop_down_weights <- selectInput(
  inputId = "weights_col",
  label = "weights column (required)",
  choices = ""
)


drop_down_observed <- selectInput(
  inputId = "observed_col",
  label = "observed column (optional)",
  choices = ""
)


drop_down_predictions1 <- selectInput(
  inputId = "pred1_col",
  label = "predictions column 1 (optional)",
  choices = ""
)


drop_down_predictions2 <- selectInput(
  inputId = "pred2_col",
  label = "predictions column 2 (optional)",
  choices = ""
)



calculate_summaries_button <- actionButton(
  inputId = "button", 
  label = "Calculate variable summaries",
  icon = icon("bar-chart-o")
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
        width = 3,
        solidHeader = TRUE,
        background = "maroon",
        title = "Select data"),
      box(
        drop_down_weights,
        drop_down_observed,
        drop_down_predictions1,
        drop_down_predictions2,
        width = 3,
        solidHeader = TRUE,
        background = "maroon",
        title = "Select summary columns"
      ),
      box(
        calculate_summaries_button,
        width = 3,
        solidHeader = TRUE,
        background = "maroon",
        title = "Summarise by explanatory variables"
      )
    ),
    tabItem(
      tabName = "summary_graphs", 
      h2("Summary Graphs"),
      textOutput("selected_variable"),
      #box(plotOutput("plot"))
      # see https://github.com/rstudio/shiny-examples/tree/master/087-crandash for example of dynamic values in boxes
      fluidRow(
        valueBoxOutput("rate"),
        valueBoxOutput("count"),
        valueBoxOutput("users")
      ),
      fluidRow(
                column(width = 3, tableOutput('tbl')),
               column(width = 4, plotlyOutput("plot")))
    )
  ),
  useShinyalert()
)







ui <- shinydashboard::dashboardPage(
  dashboardHeader,
  dashboardSidebar,
  dashboardBody,
  title = "ladybird-umbrella",
  skin = "blue"
)


