#plots
library(lattice)
library(microbenchmark)

## plot number of comparisons per log2nfact
avg1 <- function(funcs = list(), r1=20, l = 100){
  y <- numeric(length(funcs))
  names(y) <- funcs
  avg = numeric(4)
  for (i in 1:r1) {
    s <- sample(1:l)
    comps = sapply(funcs, function(x) get(x)(s)[2])
    y <- rbind(y, comps)
  }
  
  log2nfact = sum(sapply(1:l, function(n) log2(n)))
  colMeans(y[-1,]) / log2nfact
}

avg1(c("CI01", "SE86"))

sort_plot1 <- function(seqn, r1=20, funcs = c("SE86", "TO92", "CI01")){
  itr <- sapply(seqn, function (x) floor(10^x))
  y <- numeric(length(funcs))
  names(y) <- funcs
  for (i in itr) {
    y <- rbind(y, 
      avg1(funcs, r1, l=i)
    )
    print(i)
  }
  
  D = as.data.frame(cbind(y[-1, ], itr))
  frml = paste(
    paste(
      names(D)[-dim(D)[2]], 
      collapse="+"),
    "~itr")
  
  write.csv(D, sprintf("data/time -- %s -- %f, %f.csv", paste(funcs, collapse = " + "), seqn[1], seqn[length(seqn)]))
  
  xyplot(as.formula(frml), data = D, 
         type=c('p', 'l'),
         auto.key = TRUE,
         scales = list(x=list(log = 10))
  )
}

seqn = seq(from=2, to=6, length=15)
sort_plot1(seqn, funcs = c("CI01", "SE86", "TO92"))

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
