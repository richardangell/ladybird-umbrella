library(data.table)
library(shiny)
library(shinydashboard)
library(dplyr)
library(rlang)
library(plotly)
library(helpers)


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



source('./R/ui.R')
source('./R/server.R')


ladybird_umbrella <- function() {
  
  runApp('R')
  
}



