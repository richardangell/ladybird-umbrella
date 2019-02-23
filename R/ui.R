sidebar_scroll_text <- ".sidebar { height: 90vh; overflow-y: auto; }"

dashboardHeader <- dashboardHeader(title = "ladybird-umbrella",
                                   dropdownMenuOutput("messageMenu"))

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
      tabName = "summary_graphs", 
      h2("Summary Graphs"),
      textOutput("selected_variable"),
      #box(plotOutput("plot"))
      fluidRow(column(width = 3, tableOutput('tbl')),
               column(width = 4, plotlyOutput("plot")))
    )
  )
)


ui <- dashboardPage(dashboardHeader,
                    dashboardSidebar,
                    dashboardBody,
                    title = "ladybird-umbrella",
                    skin = "blue")