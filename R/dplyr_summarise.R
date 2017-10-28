

dplyr_summarise <- function(df,
                            col,
                            observed, 
                            predictions1, 
                            predictions2, 
                            weights) {
  
  if (is.null(weights)) {
    
    stop("weights must be passed")
    
  }
  
  by_var <- parse_quosure(col)
  
  summary_results <- df %>% 
                       group_by(!!by_var) %>%
                       summarise_at(c(observed, 
                                      predictions1, 
                                      predictions2, 
                                      weights), 
                                    sum)
  
  if (!is.null(observed)) {
    
    summary_results[[observed]] <- summary_results[[observed]] / 
      summary_results[[weights]]
    
  }
  
  if (!is.null(predictions1)) {
    
    summary_results[[predictions1]] <- summary_results[[predictions1]] / 
      summary_results[[weights]]
    
  }
  
  if (!is.null(predictions2)) {
    
    summary_results[[predictions2]] <- summary_results[[predictions2]] / 
      summary_results[[weights]]
    
  }
  
  return(summary_results)
  
}




