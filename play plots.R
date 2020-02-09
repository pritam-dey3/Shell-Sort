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

sort_plot <- function(n=4000, r1=20, r2=5, funcs = c("SE86", "TO92", "CI01")){
  k = floor(log(n, 5)) + 1
  itr <- sapply(3:k, function (x) 5^x)
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
  
  write.csv(D, "data.csv")
  
  xyplot(as.formula(frml), data = D, 
         type=c('p', 'l'),
         auto.key = TRUE,
         scales = list(x=list(log = 5))
  )
}


sort_plot(n = 2000, funcs = c("QuickSortH_WC", "RQsort"))

RQsort <- function(x) sort(x, method="quick")
avg()

f <- function(x) x+2
