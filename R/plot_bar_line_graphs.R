library(plotly)
library(dplyr)
library(rlang)

data <- data.frame(a = runif(100),
                   b = rnorm(100),
                   c = rpois(100, 3),
                   d = rnorm(100),
                   e = runif(100),
                   f = factor(sample(100)),
                   g = as.character(sample(5, size = 100, replace = T)))


xx <- summarise_column(df = data,
                       col = "a",
                       observed = "b",
                       predictions1 = "e", 
                       predictions2 = "d", 
                       weights = "c")


plot_bar_line_graph(df = xx$summary,
                    col = "binned_ordered",
                    weight = "c",
                    observed = "b")


plot_bar_line_graph <- function(df,
                                col,
                                weight,
                                observed,
                                rounding_digits = 5) {
  
  # need to find a better way to set the width so it looks good in the nb.html
  p <- plot_ly(df, width = 900) %>%
    
    add_trace(x = ~ eval(parse(text = col)), 
              y = ~ eval(parse(text = weight)), 
              type = 'bar', 
              name = 'weight',
              marker = list(color = 'yellow'),
              hoverinfo = "text",
              text = ~ paste(round(eval(parse(text = weight)),
                                   rounding_digits), 
                             ' (sum weight)')) %>%
    
    add_trace(x = ~ eval(parse(text = col)), 
              y = ~ eval(parse(text = observed)), 
              type = 'scatter', 
              mode = 'lines', 
              name = 'observed', 
              yaxis = 'y2',
              line = list(color = 'pink'),
              hoverinfo = "text",
              text = ~ paste(round(eval(parse(text = observed)),
                                   rounding_digits), 
                             '(ave obs)')) %>%
    
    layout(#title = paste0("<br>", col),
           title = col,
           xaxis = list(title = ""),
           yaxis = list(side = 'right', 
                        title = 'total weight', 
                        showgrid = F, 
                        zeroline = F),
           yaxis2 = list(side = 'left', 
                         overlaying = "y", 
                         title = 'average observed', 
                         showgrid = F, 
                         zeroline = F))
  
  p
  
}



