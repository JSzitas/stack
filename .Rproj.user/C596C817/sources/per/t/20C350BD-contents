pkgload::load_all(compile = TRUE)
remove(list=ls())
library(magrittr)

exmpl <- readRDS("data/example_stacking_data.rds")

y <- exmpl$y
X <- exmpl$X

tolerant_check <- purrr::partial( all.equal, tolerance = 0.01 )

# smpl = 10

rmse_stack <- function(true, predicted) {
  sqrt(mean((#stats::na.omit(
    true - predicted#)
  )^2))
}

mse_stack <- function(true, predicted) {
  mean((#stats::na.omit(
    true - predicted#)
  )^2)
}

mae_stack <- function(true, predicted) {
  mean(abs(stats::na.omit(true - predicted)))
}
# greedy stacking of models, or determining model weights greedily using
# arbitrary metrics
# see: https://bmcresnotes.biomedcentral.com/articles/10.1186/s13104-020-4931-7
greedy_stacker_r <- function(y, Z, metric = rmse_stack, criterium_better = which.min, max_iter = 100) {
  # s in the original paper
  step <- 0
  # x in the original paper
  evaluations <- rep(0, ncol(Z))
  P <- rep(0, nrow(Z))
  weights <- rep(0, ncol(Z))
  while (TRUE) {
    step <- step + 1
    for (col in seq_len(ncol(Z))) {
      evaluations[col] <- metric(y, (P + Z[, col]) / step)
    }
    eval_max <- criterium_better(evaluations)
    # they use an X[,j_max] in the original paper, but I am 99.9%
    # sure they actually mean Z instead of X (since there is no X in the
    # algorithm anywhere, and Z actually makes sense)
    P <- P + Z[, eval_max]
    # update weights
    weights[eval_max] <- weights[eval_max] + 1
    # they do not go into more detail on the convergence criterion so maybe it
    # can be improved further
    if (step > max_iter) {
      break
    }
    print(paste0(step,"\n"))
  }
  return(weights / step)
}



speed_bench <- bench::press(
  smpl = c(10,50,100,500,1000,5000,10000),
  {
    bench::mark( iterations = 50,
                 default_r = greedy_stacker_r( y[seq_len(smpl)], X[seq_len(smpl),]),
                 r_more_iter = greedy_stacker_r( y[seq_len(smpl)], X[seq_len(smpl),],  max_iter = 200),
                 cpp = greedy_stacker(y[seq_len(smpl)], X[seq_len(smpl),]),
                 cpp_more_iter = greedy_stacker(y[seq_len(smpl)], X[seq_len(smpl),],  max_iter = 200),
                 check = FALSE#tolerant_check
                 )
  }
)

speed_bench %>%
  ggplot2::autoplot() %>%
  plot()




