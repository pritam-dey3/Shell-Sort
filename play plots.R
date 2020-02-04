#practise plots

avg <- function(funcs = list(), r=20, l = 100){
  y <- numeric(length(funcs))
  names(y) <- funcs
  for (i in 1:r) {
    s = sample(1:l)
    fns = lapply(funcs, function(x) call(x, s))
    ts = microbenchmark(list = fns, unit = "us", times = 20L)
    y <- rbind(y, 
      summary(ts)[["mean"]]
    )
  }
  colMeans(y[-1,])
}

random_sample = sample(1:10000)
avg(c("SE86", "TO92", "CI01"))

sort_plot <- function(n=4000, funcs = c("SE86", "TO92", "CI01")){
  k = floor(log2(n)) + 1
  itr <- sapply(7:k, function (x) 2^x)
  y <- numeric(length(funcs))
  names(y) <- funcs
  for (i in itr) {
    y <- rbind(y, 
      avg(funcs, l=i)
      )
    print(i)
  }
  
  D = as.data.frame(cbind(y[-1, ], itr))
  frml = paste(
    paste(
      names(D)[-dim(D)[2]], 
      collapse="+"),
    "~itr")
  
  xyplot(as.formula(frml), data = D, 
         type=c('p', 'l'),
         auto.key = TRUE,
         scales = list(x=list(log = 2))
  )
}


sort_plot(n = 4000, funcs = c("QuickSortL", "QuickSortH", "RQsort"))

RQsort <- function(x) sort(x, method="quick")


f <- function(x) x+2
