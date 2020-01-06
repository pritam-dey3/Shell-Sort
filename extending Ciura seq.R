ext <- function(x) {
  while(length(x) < 20){
    x <- c(x, floor(2.25*x[length(x)]))
    print(x)
  }
  x
}

CI01 <- ext(CI01)
