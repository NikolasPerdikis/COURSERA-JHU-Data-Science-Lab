library(ggplot2)
qplot(interval, tot_steps, data = tbl_nonull, geom = "line", facets=Wday~.)