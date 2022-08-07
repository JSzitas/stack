pkgload::load_all(compile = TRUE)
remove(list=ls())
library(magrittr)

exmpl <- readRDS("data/example_stacking_data.rds")

y <- exmpl$y
X <- exmpl$X

rmse <- function(true, predicted) {
  sqrt(mean((true - predicted)^2, na.rm = TRUE))
}

popping_stacker_r <- function( y, X, max_iterations = 100, popping = 0.03 ) {
  weights <- rep(1, ncol(X))/ncol(X)

  prediction <- c(rowSums(X * weights, na.rm = TRUE))
  error <- rmse( y, prediction )
  used_weights <- seq_len(ncol(X))
  start_error <- error

  for(i in seq_len(max_iterations)) {
    # screw around the weights a bit
    nonzero_weights <- intersect( used_weights,
                                  sample( used_weights,
                                          length(used_weights)*(1 - popping)
                                  )
    )
    if( length(nonzero_weights)  < 1) {
      break;
    }
    # rescale
    new_weights <- weights[ nonzero_weights ]
    new_weights <- new_weights/sum(new_weights)
    # new prediction
    new_prediction <- c(rowSums(as.matrix(X[, nonzero_weights] * new_weights), na.rm = TRUE))
    new_error <- rmse( y, new_prediction )

    if( new_error < error ) {
      used_weights <- nonzero_weights
      error <- new_error
    }
    # print(i)
  }
  weights <- rep(1, ncol(X))
  weights[-used_weights] <- 0
  weights <- weights/sum(weights)

  return(weights)
}


speed_bench <- bench::press(
  smpl = c(10,50,100,500,1000,5000,10000),
  {
    y_ <- y[seq_len(smpl)]
    X_ <- X[seq_len(smpl),]
    bench::mark( iterations = 50,
                 default_r =popping_stacker_r( y_, X_),
                 r_more_iter = popping_stacker_r(  y_, X_,  max_iter = 200),
                 cpp = popping_stacker( y_, X_),
                 cpp_more_iter = popping_stacker( y_, X_,  max_iter = 200),
                 check = FALSE
    )
  }
)

speed_bench %>%
  ggplot2::autoplot() %>%
  plot()
