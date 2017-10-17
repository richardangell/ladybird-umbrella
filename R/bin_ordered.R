
bin_ordered <- function(ordered, bins = 30, binning = "equal_width") {
  
  if (binning == "equal_width") {
    
    ordered_binned <- cut(ordered, breaks = bins, include.lowest = T, ordered_result = T)
    
    ordered_binned <- addNA(ordered_binned)
    
  } else {
    
    stop(gettextf("unexpected value for binning argument; (%s)", 
                  sQuote(binning)))
    
  }
  
  return(ordered_binned)
  
}

