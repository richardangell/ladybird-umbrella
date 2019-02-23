library(shiny)
library(shinydashboard)
library(dplyr)
library(rlang)
library(plotly)
#source("./R/summarise_variables.R", local = TRUE)
#source("./R/summarise_column.R", local = TRUE)
#source("./R/dplyr_summarise.R", local = TRUE)
#source("./R/bin_ordered.R", local = TRUE)
#source("./R/plot_bar_line_graphs.R", local = TRUE)




data_frame_objects <- function(objects_name = ls(envir = globalenv())) {
  
  df_objects <- sapply(objects_name, 
                       function(x) is.data.frame(get(x, envir = globalenv())))
  
  return(objects_name[df_objects])
  
}


get_selected_df_cols <- function(df_name) {
  
  return(colnames(get(df_name, envir = globalenv())))

}  
  
summarise_df_cols <- function(df, cols) {
  
  summary_results <- list()
  
  for (col in cols) {
    
    # change this for actual summary fcn later
    summary_results[[col]] <- mean(df[[col]])
    
  }
  
  return(summary_results)
  
}






ladybird_umbrella <- function() {
  
  shinyApp(ui, server)
  
}



