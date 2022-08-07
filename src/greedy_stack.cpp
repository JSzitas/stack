#include <RcppEigen.h>
#include "Eigen/Dense"
using Eigen::VectorXf;
#include "utils.h"
#include <string.h>

// [[Rcpp::depends(RcppEigen)]]

#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
Eigen::VectorXf greedy_stacker( Eigen::VectorXf y,
                                 Eigen::MatrixXf Z,
                                 const char metric = 'S',
                                 const int & max_iter = 100) {

  int step = 0;
  // x in the original paper
  Eigen::VectorXf evaluations = VectorXf::Zero( Z.cols());
  Eigen::VectorXf P = VectorXf::Zero(Z.rows());
  Eigen::VectorXf weights = VectorXf::Zero(Z.cols());

  int eval_max=0;
  std::function<double(Eigen::VectorXf,Eigen::VectorXf)> metric_used;

  switch(metric) {
    case 'S' :
      metric_used = mse;
    break;
    case 'A' :
      metric_used = mae;
    break;
    case 'C' :
      metric_used = accuracy;
      break;
    default :
      stop("**metric** not found.");
  }

  while (true) {
    step += 1;
    #pragma omp parallel for
    for( int j = 0; j < Z.cols(); j++ ) {
      evaluations(j) = metric_used( y, (P + Z.col(j))/step );
    }
    eval_max = eig_which_is_min(evaluations);
    //they use an X[,j_max] in the original paper, but I am 99.9%
    // sure they actually mean Z instead of X (since there is no X in the
    // algorithm anywhere, and Z actually makes sense)
    P += Z.col(eval_max);
    // update weights
    weights(eval_max) += 1;
    // they do not go into more detail on the convergence criterion so maybe it
    // can be improved further
    if (step > max_iter) {
      break;
    }
  }
  return weights/step;
}
