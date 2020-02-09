#practise plots
library(lattice)
library(microbenchmark)

avg <- function(funcs = list(), r1=20, r2=20, l = 100){
  y <- numeric(length(funcs))
  names(y) <- funcs
  for (i in 1:r1) {
    s = sample(1:l)
    fns = lapply(funcs, function(x) call(x, s))
    ts = microbenchmark(list = fns, unit = "us", times = r2)
    y <- rbind(y, 
      summary(ts)[["mean"]]
    )
  }
  colMeans(y[-1,])
}

random_sample = sample(1:10000)
avg(c("SE86", "TO92", "CI01"))

sort_plot <- function(k1=3, k2=5, r1=20, r2=5, funcs = c("SE86", "TO92", "CI01")){
  itr <- sapply(k1:k2, function (x) 5^x)
  y <- numeric(length(funcs))
  names(y) <- funcs
  for (i in itr) {
    y <- rbind(y, 
      avg(funcs, r1, r2, l=i)
      )
    print(i)
  }
  
  D = as.data.frame(cbind(y[-1, ], itr))
  frml = paste(
    paste(
      names(D)[-dim(D)[2]], 
      collapse="+"),
    "~itr")
  
  write.csv(D, sprintf("data/time-%s-%d-%d.csv", paste(funcs, collapse = "+"), k1, k2))
  
  xyplot(as.formula(frml), data = D, 
         type=c('p', 'l'),
         auto.key = TRUE,
         scales = list(x=list(log = 5))
  )
}


sort_plot(k1=3, k2=5, funcs = c("QuickSortH_WC", "RQsort"))

RQsort <- function(x) sort(x, method="quick")
avg()

f <- function(x) x+2
