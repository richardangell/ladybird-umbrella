server <- function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe(print(paste0("dataset: ", input$dataset)))
  
  observe(print(paste0("weights_col: ", input$weights_col)))
  
  observe(print(paste0("observed_col: ", input$observed_col)))
  
  observe(print(paste0("pred1_col: ", input$pred1_col)))
  
  observe(print(paste0("pred2_col: ", input$pred2_col)))
  
  observe(print(paste0("is.null pred2_col: ", input$pred2_col == "")))
  
  #---------------------------------------------------------------------------#
  # show error messages if summary button pressed w/o acceptable inputs ----
  #---------------------------------------------------------------------------#
  
  id <- NULL
  
  observeEvent(input$button, {
    
    if (input$dataset == "please select") {
      
      id <<- showNotification("Calculate variable summaries button pressed. ERROR: No data.frame selected, no summaries calculated.",
                              type = "error")
      
    } else if (input$weights_col == "please select") {
      
      id <<- showNotification("Calculate variable summaries button pressed. ERROR: No weights column selected, no summaries calculated.",
                              type = "error")
      
    } else {
      
      id <<- showNotification(paste("Calculate variable summaries button pressed."),
                              type = "message")
      
    }
    
  })
  
  #---------------------------------------------------------------------------#
  # react to summary button click ----
  #---------------------------------------------------------------------------#
  
  summary_reactive <- eventReactive(input$button, {
    
    # only perform calculation if a data.frame and weights col are selected
    if (input$dataset != "please select" & 
        input$weights_col != "please select") {
      
      # get colnames from user selected data.frame  
      df_cols <- get_selected_df_cols(input$dataset)
      
      # remove selected summary columns from col list
      reduced_df_cols <- setdiff(df_cols,
                                 c(input$weights_col,
                                   input$observed_col,
                                   input$pred1_col,
                                   input$pred2_col))
      
      #summarise_df_cols(get(input$dataset, envir = globalenv()),
      #                  reduced_df_cols)
      
      summary_args <- list(df = get(input$dataset, envir = globalenv()),
                           cols = reduced_df_cols)
      
      if (input$weights_col != "please select") {
        
        summary_args$weight <- input$weights_col
        
      }
      
      if (input$observed_col != "please select") {
        
        summary_args$observed <- input$observed_col
        
      }
      
      if (input$pred1_col != "please select") {
        
        summary_args$predictions1 <- input$pred1_col
        
      }
      
      if (input$pred2_col != "please select") {
        
        summary_args$predictions2 <- input$pred2_col
        
      }
      
      # call summary function on selected columns in data.frame
      summary_results <- do.call(what = summarise_variables,
                                 args = summary_args)
      
      # add summarised dataset info to results
      results_plus_metadata <- c(
        list(
          metadata = c(data.frame = input$dataset,
                       weights = input$weights_col,
                       observed = input$observed_col,
                       predictions1 = input$pred1_col,
                       predictions2 = input$pred2_col)
        ),
        summary_results
      )
      
      return(results_plus_metadata)   
      
    }
    
  })
  
  #---------------------------------------------------------------------------#
  # plot for selected explanatory variable ----
  #---------------------------------------------------------------------------#
  
  #output$plot <- renderPlot({
  #  
  #  if (input$plot_var_check_box != "please select" & 
  #        input$plot_var_check_box != "please select data.frame") {
  #    
  #    plot(summary_reactive()[[input$plot_var_check_box]], 
  #         summary_reactive()[[input$plot_var_check_box]] * 2)
  #    
  #  }
  #  
  #})
  
  output$plot <- renderPlotly({
    
    if (input$plot_var_check_box != "please select" & 
        input$plot_var_check_box != "please select data.frame") {
      
      plot_agrs <- list(df = summary_reactive()[[input$plot_var_check_box]]$summary,
                        col = input$plot_var_check_box)
      
      if (input$weights_col != "please select") {
        
        plot_agrs$weight <- summary_reactive()$metadata[["weights"]]
        
      }
      
      if (input$observed_col != "please select") {
        
        plot_agrs$observed <- summary_reactive()$metadata[["observed"]]
        
      }
      
      if (input$pred1_col != "please select") {
        
        plot_agrs$predictions1 <- summary_reactive()$metadata[["predictions1"]]
        
      }
      
      if (input$pred2_col != "please select") {
        
        plot_agrs$predictions2 <- summary_reactive()$metadata[["predictions2"]]
        
      }
      
      do.call(what = plot_bar_line_graph,
              args = plot_agrs)
      
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
      
      selected_df_cols <- get_selected_df_cols(input$dataset)
      
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
  
  available_cols_update <- function() {
    
    selected_df_cols <- get_selected_df_cols(input$dataset)
    
    selected_w <- input$weights_col
    
    selected_o <- input$observed_col
    
    selected_p1 <- input$pred1_col
    
    selected_p2 <- input$pred2_col
    
    reduced_df_cols <- setdiff(selected_df_cols,
                               c(selected_w,
                                 selected_o,
                                 selected_p1,
                                 selected_p2))
    
    if (length(reduced_df_cols) == 0) {
      
      reduced_df_cols <- "no columns left to select"
      
    }
    
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
  
  observeEvent(input$weights_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$weights_col %in% default_select) {
      
      available_cols_update()
      
    }
    
  })
  
  observeEvent(input$observed_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$observed_col %in% default_select) {
      
      available_cols_update()
      
    }
    
  })
  
  observeEvent(input$pred1_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$pred1_col %in% default_select) {
      
      available_cols_update()
      
    }
    
  })
  
  
  observeEvent(input$pred2_col, {
    
    default_select <- c("please select data.frame")#, "please select")
    
    if (length(input$dataset) > 0 && 
        input$dataset != "please select" && 
        !input$pred2_col %in% default_select) {
      
      available_cols_update()
      
    }
    
  })
  
  
  
  output$tbl <- renderTable({ 
    
    
    if (input$plot_var_check_box != "please select" & 
        input$plot_var_check_box != "please select data.frame") {
      
      summary_reactive()[[input$plot_var_check_box]]$summary
      
    }
    
  }) 
  
  
}