pkgload::load_all(compile = TRUE)
remove(list=ls())
library(magrittr)

exmpl <- readRDS("data/example_stacking_data.rds")

y <- exmpl$y
X <- exmpl$X

X[is.nan(X)] <- NA

# X <- X[,sample(ncol(X), 20)]

validation <- replicate(n = 100,
          {
            # out of sample evaluation
            train_id <- sample(nrow(X), 0.7*nrow(X))
            valid_id <- seq_len(nrow(X))[-train_id]

            greedy <- greedy_stacker( y[train_id], X[train_id,])
            greedy_more <- greedy_stacker( y[train_id], X[train_id,],
                                           max_iter = 500 )
            popped <- popping_stacker_r(y[train_id], X[train_id,])
            popped_more <- popping_stacker_r(y[train_id], X[train_id,], 500)

            c( greedy = rmse( y[valid_id], c(X[valid_id,] %*% greedy)),
               greedy_more = rmse( y[valid_id], c(X[valid_id,] %*% greedy_more)),
               popped_r = rmse( y[valid_id], c(X[valid_id,] %*% popped)),
               popped_more_r = rmse( y[valid_id], c(X[valid_id,] %*% popped_more))
            )
          })

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

