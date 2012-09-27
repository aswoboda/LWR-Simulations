# Placing functions useful for making Figures in here for easy sourcing

# here's a function I might use that came from https://github.com/hadley/ggplot2/wiki/Case-Study%3a-Raman-Spectroscopic-Grading-of-Gliomas

# peeling contours: 2d quantiles
peel <- function (x, y, weights = 1, probs = NA, threshold = 1 - 1e-3){
  if (missing (y) && ncol (x) == 2){
    y <- x [, 2]
    x <- x [, 1]
  }
  
  if  (length (x) != length (y))
    stop ("x and y need to have the same length.")
  
  weights <- rep (weights, length.out = length (x))
  
  ## start with all points
  pts.in <- seq_along (x)
  step <- 1
  hulls <- list ()
  
  ## too small weights can confuse the peeling as the hull polygon treats all points equally
  
  exclude <- weights < threshold
  if (any (exclude)) {
    ##   warning (sum (exclude), " points put into first hull due to too small weights")
    
    hulls [[1]] <- pts.in [exclude]
    pts.in <- pts.in [! exclude]
    step <- step + 1
  }
  
  ## peel off the hull polygons until nothing is left
  while (length (pts.in) > 1){
    hull <- chull (x [pts.in], y [pts.in])
    hulls [[step]] <- pts.in [hull]
    pts.in <- pts.in [-hull]
    step <- step + 1
  }
  
  # now count the number of point-equivalents in each hull
  n <- sapply (hulls, function (i) sum (weights [i]))
  
  ## and convert to percentiles
  n <- cumsum (n)
  qtl <- c(1, 1 - head (n, -1) / tail (n, 1))
  
  names (hulls) <- qtl
  
  if (! all (is.na (probs))){
    i <- round (approx (qtl[-1], seq_along (hulls[-1]), probs, rule = 2)$y) + 1
    hulls <- hulls [i]
  }
  
  hulls
}



