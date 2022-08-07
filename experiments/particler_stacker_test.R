pkgload::load_all(compile = FALSE)
remove(list=ls())
library(magrittr)

exmpl <- readRDS("data/example_stacking_data.rds")

y <- exmpl$y
X <- exmpl$X

X[is.nan(X)] <- NA

resample_particles <- function( particle_mat,
                                importance_weights,
                                resample_rate = 0.000025 ) {

  sampled_particles <- sample( seq_len(ncol(particle_mat)),
                               ncol(particle_mat),
                               prob = importance_weights/sum(importance_weights))

  particle_mat <- particle_mat[,sampled_particles] +
  matrix(
    runif( ncol(particle_mat) * nrow(particle_mat), 0, resample_rate ),
    ncol = ncol(particle_mat),
    nrow = nrow(particle_mat)
  )
  apply( particle_mat, 2, function(weights) {
    weights/sum(weights)
  })
  # particle_mat
}

bin_resample_particles <- function( particle_mat,
                                    importance_weights,
                                    resample_rate = 0.2 ) {

  importance_weights <- importance_weights/sum(importance_weights)
  bin_weights <- sample( length(importance_weights),
                         size = resample_rate * length(importance_weights),
                         prob = importance_weights )

  particle_mat[,sample( bin_weights,
                        ncol(particle_mat),
                        prob = importance_weights[bin_weights],
                        replace = TRUE) ]


  # sampled_particles <- sample( seq_len(ncol(particle_mat)),
  #                              ncol(particle_mat),
  #                              prob = importance_weights/sum(importance_weights))
}


particle_stacker <- function( y,
                              X,
                              n_particles = 100,
                              max_iterations = 2000,
                              regeneration = 0.0003 ) {

  weight_mat <- replicate( n_particles,
                           {
                             rep(1, ncol(X))/ncol(X)
                           })
  errors <- apply( weight_mat, 2, function(weights) {
    rmse( y, c( X %*% weights ))
  })
  # set adaptive constant for regeneration
  adapt_const <- 1
  last_error <- rmse( y, c(rowSums(X * rowMeans( weight_mat ))))
  print(last_error)
  # return(list( y = y, predictions = predictions, errors = errors ))
  for(i in seq_len(max_iterations)) {
    # sample particles according to 'quasi' importance weights
    # and mutate them
    weight_mat <- resample_particles( particle_mat = weight_mat, errors, regeneration * adapt_const )
    # return(weight_mat)
    # generate predictions and errors
    errors <- apply( weight_mat, 2, function(weights) {
      rmse( y, c( X %*% weights ))
    })
    # aggregate error
    new_error<- rmse( y, c(rowSums(X * rowMeans( weight_mat ))))
    print(new_error)
    # readjust adaptive constant as needed
    # based on last_error vs new_error
    # adapt_const <- max( ifelse( last_error < new_error,
    #                             0.99*adapt_const,
    #                             adapt_const),
    #                     0.001)
    last_error <- new_error
  }
  return(weight_mat)
}

particle_stacker(y, X) -> rs
# greedy_more <- c( X %*% greedy_stacker( y, X, max_iter = 500 ) )
# popped_more <- c( X %*% popping_stacker(y, X, 500) )


