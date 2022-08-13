#include <RcppEigen.h>
#include "Eigen/Dense"
#include <vector>
using Eigen::VectorXf;

#include <Rcpp.h>
using namespace Rcpp;

double mse( const Eigen::VectorXf & y, const Eigen::VectorXf & y_hat ) {
  return (y - y_hat).array().isNaN().select(0,y - y_hat).array().square().mean();
}

double rmse( const Eigen::VectorXf & y, const Eigen::VectorXf & y_hat ) {
  return sqrt((y - y_hat).array().isNaN().select(0,y - y_hat).array().square().mean());
}

double mae( const Eigen::VectorXf & y, const Eigen::VectorXf & y_hat ) {
  return (y - y_hat).array().isNaN().select(0,y - y_hat).array().abs().mean();
}

double accuracy( const Eigen::VectorXf & y,
                 const Eigen::VectorXf & y_hat) {
  Eigen::VectorXf resid = (y - y_hat).array().isNaN().select(0,y - y_hat).array();
  double acc;
  for( int i = 0; i < resid.size(); i++) {
    // I am willing to accept criticism for hard-coded tolerances, but this should
    // be quite safe
    acc += abs(resid(i)) < 0.001;
  }
  return acc/resid.size();
}

int eig_which_is_min(Eigen::VectorXf x) {
  int min_elem_index = 0;
  float min_elem = x(0);
  for( int index = 1; index < x.size(); index++  ) {
    if( x(index) < min_elem ) {
      min_elem = x(index);
      min_elem_index = index;
    }
  }
  return min_elem_index;
}
