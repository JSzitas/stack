#include <RcppEigen.h>
#include "Eigen/Dense"
using Eigen::VectorXf;
using Eigen::MatrixXf;
#include "utils.h"
#include "rng_module.h"

// [[Rcpp::depends(RcppEigen)]]
#ifdef _OPENMP
#include <omp.h>
#endif
// [[Rcpp::plugins(openmp)]]

// [[Rcpp::export]]
Eigen::VectorXf popping_stacker( Eigen::VectorXf y,
                                 Eigen::MatrixXf Z,
                                 const char metric = 'S',
                                 const int & max_iter = 100,
                                 const float & popping_rate = 0.03) {

  Eigen::VectorXf predictions = VectorXf::Zero(Z.rows());
  Eigen::VectorXf weights = VectorXf::Constant(Z.cols(), float(1.0/Z.cols()));
  std::function<double(Eigen::VectorXf,Eigen::VectorXf)> metric_used;

  switch(metric) {
  case 'R' :
    metric_used = rmse;
    break;
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

  predictions = (Z * weights).rowwise().sum();
  float error = metric_used( y, predictions );

  std::vector<int> default_seq = sequence(0,int(Z.cols()), 1);
  std::vector<int> used_weights = default_seq;
  std::vector<int> new_used_weights;
  float new_error, drop_rate = (1-popping_rate), popped_items;

  for( int step =0; step < max_iter; step++) {
    // reset weights to 0
    eigvec_indexed_replacement( weights, default_seq, 0 );
    // pop some weights off and find a new intersecting set of weights
    // TODO: check if this pops as many as it should
    popped_items = drop_rate * used_weights.size();
    // proposal = sample_from_sequence(used_weights, popped_items);
    new_used_weights = intersect<std::vector<int>, int>( used_weights,
                                                         ordered_sample_from_sequence(used_weights, static_cast<int>(popped_items)));
    if( new_used_weights.size()  < 1 ) {
      break;
    }
    // only use nonzero weigths
    eigvec_indexed_replacement( weights, new_used_weights, float(1.0/new_used_weights.size()) );
    // form new predictions and compute error
    new_error = metric_used( y, (Z * weights).rowwise().sum() );
    // if better than before, update
    if( new_error < error ) {
      used_weights = new_used_weights;
      error = new_error;
    }
  }
  eigvec_indexed_replacement( weights, default_seq, 0 );
  eigvec_indexed_replacement( weights, used_weights, float(1.0/used_weights.size()) );
  return weights;
}

// [[Rcpp::export]]
Eigen::VectorXf averaged_popping_stacker( Eigen::VectorXf y,
                                          Eigen::MatrixXf Z,
                                          const char metric = 'S',
                                          const int & max_iter = 100,
                                          const float & popping_rate = 0.03,
                                          const int & repetitions = 100) {

  Eigen::MatrixXf result_mat( Z.cols(), repetitions );

  #pragma omp parallel for
  for( int j = 0; j < repetitions; j++ ) {
    result_mat.col(j) = popping_stacker( y, Z, metric, max_iter, popping_rate);
  }

  Eigen::VectorXf result = result_mat.rowwise().mean();
  return result;
}



