### montecarlo.R file ###

# fsearch - function that searches the matrix s for the best solution
#    s - matrix with solutions (each row is a solution)
#    fn - evaluation function
#    type - "min" or "max"
#    ... - extra parameters for fn
fsearch <- function(s, fn, type = "min", ...) {
  n <- nrow(s)
  evals <- numeric(n)
  
  for(i in 1:n) {
    evals[i] <- fn(s[i, ], ...)
  }
  
  if(type == "min") {
    best_idx <- which.min(evals)
  } else {
    best_idx <- which.max(evals)
  }
  
  return(list(
    sol = s[best_idx, ],
    eval = evals[best_idx],
    mat = s,
    index = best_idx
  ))
}

# montecarlo uniform search method
#    fn - evaluation function
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    N - number of samples
#    type - "min" or "max"
#    ... - extra parameters for fn
mcsearch <- function(fn, lower, upper, N, type = "min", ...) {
  D <- length(lower)
  s <- matrix(nrow = N, ncol = D)  # set the search space 
  for(i in 1:N) {
    s[i, ] <- runif(D, lower, upper)
  }
  fsearch(s, fn, type, ...)  # best solution
}