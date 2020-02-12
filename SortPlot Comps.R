#plots
library(lattice)
library(microbenchmark)

## plot number of comparisons per log2nfact
avg1 <- function(funcs = list(), r1=20, l = 100, measure=2){
  y <- numeric(length(funcs))
  names(y) <- funcs
  avg = numeric(4)
  for (i in 1:r1) {
    s <- sample(1:l)
    comps = sapply(funcs, function(x) get(x)(s)[measure])
    y <- rbind(y, comps)
  }
  
  log2nfact = sum(sapply(1:l, function(n) log2(n)))
  colMeans(y[-1,]) / log2nfact
}

#[measure] decides wheather to count number of swaps(1) or comparisons(2)
sort_plot1 <- function(seqn, r1=20, funcs = c("SE86", "TO92", "CI01"), measure=2){
  itr <- sapply(seqn, function (x) floor(10^x))
  y <- numeric(length(funcs))
  names(y) <- funcs
  for (i in itr) {
    y <- rbind(y, 
      avg1(funcs, r1, l=i, measure)
    )
    print(i)
  }
  
  D = as.data.frame(cbind(y[-1, ], itr))
  frml = paste(
    paste(
      names(D)[-dim(D)[2]], 
      collapse="+"),
    "~itr")
  
  write.csv(D, sprintf("data/count(%d) -- %s -- %f, %f.csv", measure, paste(funcs, collapse = " + "), seqn[1], seqn[length(seqn)]))
  
  xyplot(as.formula(frml), data = D, 
         type=c('p', 'l'),
         auto.key = TRUE,
         scales = list(x=list(log = 10))
  )
}

seqn = seq(from=2, to=6, length=15)
sort_plot1(seqn, funcs = c("QuickSortH", "QuickSortL", "CI01", "TO92", "SE86"))

sort_plot1(seqn, funcs = c("QuickSortH", "QuickSortL", "CI01", "TO92", "SE86"), measure = 1)

