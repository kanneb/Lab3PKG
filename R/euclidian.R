euclidean <- function(a,b) {
  stopifnot(is.numeric(a), is.numeric(b))

  while (b != 0) {
     rem <- a %% b
     a <- b
     b <- rem
  }

  return(a)
}

#euclidean(123612, 13892347912)
#euclidean(100, 1000)
