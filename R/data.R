x <- data.frame(o = rnorm(100),
                p = rnorm(100),
                p2 = runif(100),
                w = runif(100),
                e1 = rpois(100, 3),
                e2 = rpois(100, 3),
                e3 = rpois(100, 3),
                e4 = factor(rpois(100, 3)))

obs = "o"
pred = "p"
pred2 = "p2"
wts = "w"

library(data.table)

setDT(x)

x[ , 
   list(average_observed = sum(o) / .N,
          average_predicted = sum(p) / .N,
          average_predicted2 = sum(p2) / .N,
          total_weight = sum(w),
          observation_count = .N), 
   by = e4]


x[ , 
   list(average_observed = sum(o) / .N,
        average_predicted = sum(p3) / .N,
        average_predicted2 = sum(p2) / .N,
        total_weight = sum(w),
        observation_count = .N), 
   by = e4]

?expression


expression(list(average_observed = sum(o) / .N,
                average_predicted = sum(p3) / .N,
                average_predicted2 = sum(p2) / .N,
                total_weight = sum(w),
                observation_count = .N))
