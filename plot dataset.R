#plotting with dataset

D1 <- read.csv("data/time -- QuickSortH_WC+QuickSortL_WC+CI01+SE86 -- 2.000000, 4.857143.csv", row.names=1)
D2 <- read.csv("data/time -- QuickSortH_WC+QuickSortL_WC+CI01+SE86 -- 5.142857, 6.000000.csv", row.names=1)

D <- rbind(D1, D2)

yvars <- head(names(D), -1)

frml = paste(
  paste(sprintf("I(%s / (itr*log(itr)))", yvars),
    collapse="+"),
  "~itr")

xyplot(as.formula(frml), data = D, 
       type=c('p', 'l'),
       auto.key = TRUE,
       scales = list(y = list(log=10),x=list(log = 10))
)
