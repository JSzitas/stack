remove(list=ls())
pkgload::load_all(compile = TRUE)
library(magrittr)

exmpl <- readRDS("data/example_stacking_data.rds")

y <- exmpl$y
X <- exmpl$X

X[is.nan(X)] <- NA

# X <- X[,sample(ncol(X), 20)]

rmse <- function(true, predicted) {
  sqrt(mean((true - predicted)^2, na.rm = TRUE))
}
tictoc::tic("duration:")
validation <- replicate(n = 100,
          {
            # out of sample evaluation
            train_id <- sample(nrow(X), 0.7*nrow(X))
            valid_id <- seq_len(nrow(X))[-train_id]

            greedy <- greedy_stacker( y[train_id], X[train_id,])
            greedy_more <- greedy_stacker( y[train_id], X[train_id,],
                                           max_iter = 500 )
            popped <- popping_stacker(y[train_id], X[train_id,])
            popped_more <- popping_stacker(y[train_id], X[train_id,], max_iter = 500)
            popped_avg <- averaged_popping_stacker( y[train_id], X[train_id,] )
            popped_avg_more <- averaged_popping_stacker( y[train_id], X[train_id,], max_iter = 500, repetitions = 200 )

            c( greedy = rmse( y[valid_id], c(X[valid_id,] %*% greedy)),
               greedy_more = rmse( y[valid_id], c(X[valid_id,] %*% greedy_more)),
               popped = rmse( y[valid_id], c(X[valid_id,] %*% popped)),
               popped_more = rmse( y[valid_id], c(X[valid_id,] %*% popped_more)),
               popped_averaged = rmse( y[valid_id], c(X[valid_id,] %*% popped_avg)),
               popped_averaged_more = rmse( y[valid_id], c(X[valid_id,] %*% popped_avg_more)),
               baseline = rmse( y[valid_id], c(X[valid_id,] %*% rep(1, ncol(X))/ncol(X)) )
            )
          })
tictoc::toc()
validation <- validation %>%
  t() %>%
  as.data.frame %>%
  cbind( index = seq_len(ncol(validation)) ) %>%
  tidyr::pivot_longer(cols = -tidyselect::all_of(c("index")))

validation %>%
  ggplot2::ggplot(ggplot2::aes(x = index, y = value, color = name)) +
    ggplot2::geom_point()

validation %>%
  ggplot2::ggplot(ggplot2::aes(x = value, color = name)) +
  ggplot2::geom_density()

tidyr::pivot_wider(validation, id_cols = c("name", "index")) %>%
  dplyr::select(-index) %>%
  as.matrix %>%
  tsutils::nemenyi( plottype = "vmcb")

