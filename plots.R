#plots
library(lattice)
library(microbenchmark)

## plot number of comparisons per log2nfact
avg1 <- function(i = 10, r = 100){
  avg = numeric(4)
  for (j in 1:r) {
    random_samples <- sample(1:i)
    comps = c(se86 = ShellSort(random_samples, Gaps = SE86)[2],
              to92 = ShellSort(random_samples, Gaps = TO92)[2],
              ci01 = ShellSort(random_samples, Gaps = CI01)[2],
              qui = QuickSort(random_samples)[2]
            )
    avg = rbind(avg, comps)
  }
  colMeans(avg)
}

 
sort_plot1 <- function(n=4000, intv=50, r=100){
  itr <- seq(10, n, intv)
  y <- numeric(4)
  for (i in itr) {
    avg <- avg1(i)
    log2nfact = sum(sapply(1:i, function(n) log2(n)))
    y <- rbind(y, avg/(log2nfact))
  }
  D = as.data.frame(cbind(y[-1, ], itr))
  xyplot(se86 + to92 + ci01 + qui ~ itr, data = D, 
         type=c('p', 'l'),
         auto.key = TRUE,
         scales = list(x=list(log = 2))
         )
}

sort_plot1()


## plot system time
avg2 <- function(i = 10, r = 100){
  random_samples <- sample(1:i)
  
  secs = c(se86 = summary(microbenchmark(ShellSort(random_samples, Gaps = SE86_)), unit="ms")[["mean"]],
            to92 = summary(microbenchmark(ShellSort(random_samples, Gaps = TO92_)), unit="ms")[["mean"]],
            ci01 = summary(microbenchmark(ShellSort(random_samples, Gaps = CI01_)), unit="ms")[["mean"]],
            qui = summary(microbenchmark(QuickSort(random_samples)), unit="ms")[["mean"]]
          )
  secs
}

sort_plot2 <- function(t=1, n=4000, intv=50, r=100){
  k = floor(log2(n)) + 1
  itr <- sapply(1:k, function (x) 2^x)
  y <- numeric(4)
  for (i in itr) {
    y <- rbind(y, get(paste("avg", t, sep = ""))(i))
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

sort_plot2(t=2, n=4000)

avg2.1 <- function(i = 10, r = 100){
  random_samples <- sample(1:i)
  
  secs = c(se86 = summary(microbenchmark(ShellSort(random_samples, Gaps = SE86)), unit="ms")[["mean"]],
           to92 = summary(microbenchmark(ShellSort(random_samples, Gaps = TO92)), unit="ms")[["mean"]],
           ci01 = summary(microbenchmark(ShellSort(random_samples, Gaps = CI01)), unit="ms")[["mean"]],
           quiR = summary(microbenchmark(sort(random_samples, method="quick")), unit="ms")[["mean"]]
  )
  secs
}


